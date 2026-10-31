#!/bin/bash
# Run a release binary over a Jasmin corpus, one `check` per .jazz and .jinc
# file, and fail unless every file gets a verdict.
#
#   scripts/corpus-smoke.sh <binary> <corpus> [-j N] [--arch NAME] [--write FILE] [--expect FILE]
#
# This is the smoke test of a release binary, not parity: which files ought
# to pass is the compiler's to say. A file fails when the binary crashes,
# exits other than 0 or 1, prints no summary line, or reports an internal
# diagnostic. --write keeps the per-file verdicts and --expect compares them
# with another platform's, which must be the same table. Exit status: 0 every
# file got a verdict, 1 one did not or --expect differs, 2 a usage error.

set -eu -o pipefail

usage() {
	printf 'usage: %s <binary> <corpus-root> [-j N] [--arch NAME] [--write FILE] [--expect FILE]\n' \
		"${0##*/}" >&2
	exit 2
}

say() { printf '%s: %s\n' "${0##*/}" "$*"; }
die() {
	printf '%s: error: %s\n' "${0##*/}" "$*" >&2
	exit 2
}

group_of() {
	case $1 in
	tests/*) rest=${1#tests/} ;;
	*) rest=$1 ;;
	esac
	case $rest in
	*/*) printf '%s\n' "${rest%%/*}" ;;
	*) printf '%s\n' "$rest" ;;
	esac
}

run_one() {
	index=$1
	relative=$2
	status=0
	(
		cd "$corpus" &&
			"$binary" check ${arch:+--arch "$arch"} -- "$relative"
	) >"$work/out.$index" 2>"$work/err.$index" </dev/null || status=$?
	internal=$(awk '/: internal: /{n++} END{print n+0}' "$work/out.$index")
	last=$(awk 'END{sub(/\r$/,""); print}' "$work/out.$index")
	verdict=none
	case $last in
	"$relative: ok") verdict=ok ;;
	"$relative: 1 error") verdict=errors=1 ;;
	"$relative: "*" errors") verdict=errors=${last##*: } ;;
	esac
	verdict=${verdict% errors}
	{
		printf '%s\n' "$status"
		printf '%s\n' "$internal"
		printf '%s\n' "$verdict"
	} >"$work/result.$index"
}

main() {
	local binary= corpus= jobs=4 arch= write= expect= index= running= i given=
	local status internal verdict group count=0
	local bad=0 groups=

	[ $# -ge 2 ] || usage
	binary=$1
	corpus=$2
	shift 2
	while [ $# -gt 0 ]; do
		case $1 in
		-j)
			[ $# -ge 2 ] || usage
			jobs=$2
			shift 2
			;;
		--arch)
			[ $# -ge 2 ] || usage
			arch=$2
			shift 2
			;;
		--write)
			[ $# -ge 2 ] || usage
			write=$2
			shift 2
			;;
		--expect)
			[ $# -ge 2 ] || usage
			expect=$2
			shift 2
			;;
		*) usage ;;
		esac
	done

	case $jobs in
	'' | *[!0-9]*) die "-j wants a number of checks, not '$jobs'" ;;
	esac
	[ "$jobs" -ge 1 ] || die "-j is below one: $jobs"

	[ -f "$binary" ] || die "no such file: $binary"
	[ -x "$binary" ] || die "not executable: $binary"
	[ -d "$corpus" ] || die "no such directory: $corpus"
	[ -z "$expect" ] || [ -f "$expect" ] || die "no such file: $expect"

	given=$binary
	binary=$(cd -- "$(dirname -- "$binary")" && pwd)/$(basename -- "$binary")
	corpus=$(cd -- "$corpus" && pwd)

	say "binary $given"
	if ! version=$("$binary" --version 2>&1); then
		printf '%s: error: %s --version failed, so the binary does not run here:\n%s\n' \
			"${0##*/}" "$binary" "$version" >&2
		return 1
	fi
	case $version in
	*' '*) say "version $(printf '%s' "$version" | sed -n '1p')" ;;
	*)
		printf '%s: error: %s --version printed no version line:\n%s\n' \
			"${0##*/}" "$binary" "$version" >&2
		return 1
		;;
	esac

	files=$(cd "$corpus" && find . -type f \( -name '*.jazz' -o -name '*.jinc' \) |
		sed 's|^\./||' | LC_ALL=C sort)
	[ -n "$files" ] ||
		die "no .jazz or .jinc file under $corpus, so there is nothing to check"
	count=$(printf '%s\n' "$files" | grep -c .)
	say "corpus $corpus: $count files, $jobs at a time"

	work=$(mktemp -d "${TMPDIR:-/tmp}/corpus-smoke.XXXXXX") ||
		die "cannot make a work directory"
	trap 'rm -rf "$work"' EXIT
	trap 'exit 2' HUP INT TERM
	printf '%s\n' "$files" >"$work/list"

	index=0
	running=0
	while IFS= read -r rel; do
		[ -n "$rel" ] || continue
		index=$((index + 1))
		run_one "$index" "$rel" &
		running=$((running + 1))
		if [ "$running" -ge "$jobs" ]; then
			wait
			running=0
		fi
	done <"$work/list"
	wait

	i=1
	: >"$work/table"
	: >"$work/groups"
	while [ "$i" -le "$index" ]; do
		rel=$(sed -n "${i}p" "$work/list")
		status=$(sed -n '1p' "$work/result.$i")
		internal=$(sed -n '2p' "$work/result.$i")
		verdict=$(sed -n '3p' "$work/result.$i")
		failed=0
		case $status in
		0 | 1) : ;;
		*)
			printf '%s: error: %s exited %s, which no check does\n' \
				"${0##*/}" "$rel" "$status" >&2
			verdict="exit=$status"
			failed=1
			;;
		esac
		if [ "$internal" -gt 0 ]; then
			printf '%s: error: %s reported an internal diagnostic:\n' \
				"${0##*/}" "$rel" >&2
			awk '/: internal: /' "$work/out.$i" | sed 's/^/  /' >&2
			failed=1
			[ "$verdict" = none ] || verdict="$verdict,internal"
		fi
		if [ "$verdict" = none ]; then
			printf '%s: error: %s printed no verdict; the last line was:\n' \
				"${0##*/}" "$rel" >&2
			sed -n '$p' "$work/out.$i" | sed 's/^/  /' >&2
			failed=1
		fi
		bad=$((bad + failed))
		printf '%s\t%s\n' "$rel" "$verdict" >>"$work/table"
		group=$(group_of "$rel")
		printf '%s\t%s\n' "$group" "$verdict" >>"$work/groups"
		i=$((i + 1))
	done

	local ok errors
	ok=$(awk -F'\t' '$2=="ok"{n++} END{print n+0}' "$work/table")
	errors=$(awk -F'\t' '$2 ~ /^errors=/{n++} END{print n+0}' "$work/table")
	printf '  %-12s %6s %6s %6s\n' group files ok errors
	groups=$(cut -f1 "$work/groups" | awk '!seen[$0]++')
	for group in $groups; do
		awk -F'\t' -v g="$group" '$1==g{
			files++
			if ($2=="ok") ok++
			else if ($2 ~ /^errors=/) errors++
		} END{printf "  %-12s %6d %6d %6d\n", g, files+0, ok+0, errors+0}' \
			"$work/groups"
	done

	[ -z "$write" ] || {
		cp "$work/table" "$write.tmp" && mv "$write.tmp" "$write"
		say "wrote the verdict table to $write"
	}

	local differ=0
	if [ -n "$expect" ]; then
		if ! differ=$(diff -u "$expect" "$work/table" 2>&1); then
			printf '%s: error: the verdicts differ from %s:\n%s\n' \
				"${0##*/}" "$expect" "$differ" >&2
			bad=$((bad + 1))
		else
			say "the verdicts match $expect line for line"
		fi
	fi

	printf '%s: %s files: %s ok, %s with errors, %s failed\n' \
		"${0##*/}" "$count" "$ok" "$errors" "$bad"
	if [ "$bad" -ne 0 ]; then
		say "fail: $bad of $count files did not get a verdict from this binary"
		return 1
	fi
	say "ok: every file got a verdict, no internal diagnostic, no crash"
	return 0
}

main "$@"
