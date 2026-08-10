#!/bin/sh

set -u

if [ "$#" -lt 2 ]; then
	printf 'usage: %s "<dune command>" <dune arguments...>\n' "${0##*/}" >&2
	exit 2
fi

dune=$1
shift

cd "$(dirname "$0")/.." || exit 2

build_dir=${DUNE_BUILD_DIR:-_build}
context=${DUNE_CONTEXT:-default}

manifest=test/conditional.tests

tests=$(mktemp "${TMPDIR:-/tmp}/jasmin-runtest-tests.XXXXXX") || exit 2
dirs=$(mktemp "${TMPDIR:-/tmp}/jasmin-runtest-dirs.XXXXXX") || exit 2
entries=$(mktemp "${TMPDIR:-/tmp}/jasmin-runtest-declared.XXXXXX") || exit 2
cleanup() { rm -f "$tests" "$dirs" "$entries"; }
trap cleanup EXIT
trap 'cleanup; exit 130' INT TERM

find test -type f -name '*.t' | sort >"$tests"
if [ ! -s "$tests" ]; then
	printf 'runtest: no cram tests under test/, so a green test step would prove nothing\n' >&2
	exit 1
fi

cram_stanza() {
	awk '
		{ line = $0; sub(/;.*/, "", line) }

		!inside && line ~ /^[ \t]*\(cram([ \t]|$)/ { inside = 1; depth = 0 }

		inside {
			text = text line "\n"
			depth += gsub(/\(/, "(", line)
			depth -= gsub(/\)/, ")", line)
			if (depth <= 0) { printf "%s", text; inside = 0; text = "" }
		}
	' "$1" 2>/dev/null
}

declared() {
	awk -v test="$1" '$1 == test { print $2 }' "$entries"
}

check_manifest() {
	lineno=0
	while IFS= read -r line || [ -n "$line" ]; do
		lineno=$((lineno + 1))
		case $line in
		*'#'*) line=${line%%#*} ;;
		esac
		set -f
		set -- $line
		set +f
		[ "$#" -eq 0 ] && continue

		if [ "$#" -ne 2 ]; then
			printf 'runtest: %s:%d: a line names one test and one variable\n' \
				"$manifest" "$lineno" >&2
			return 1
		fi

		case $1 in
		*.t) ;;
		*)
			printf 'runtest: %s:%d: %s is not a .t file\n' \
				"$manifest" "$lineno" "$1" >&2
			return 1
			;;
		esac

		if [ ! -f "$1" ]; then
			printf 'runtest: %s:%d: %s is not a file in the tree\n' \
				"$manifest" "$lineno" "$1" >&2
			return 1
		fi

		case $2 in
		'' | [0-9]* | *[!A-Za-z0-9_]*)
			printf 'runtest: %s:%d: %s is not a variable name\n' \
				"$manifest" "$lineno" "$2" >&2
			return 1
			;;
		esac

		if [ -n "$(declared "$1")" ]; then
			printf 'runtest: %s:%d: %s is declared twice\n' \
				"$manifest" "$lineno" "$1" >&2
			return 1
		fi

		dune_file=${1%/*}/dune
		if [ ! -f "$dune_file" ]; then
			printf 'runtest: %s:%d: %s has no dune file, so nothing enables it\n' \
				"$manifest" "$lineno" "$dune_file" >&2
			return 1
		fi
		stanza=$(cram_stanza "$dune_file")
		case $stanza in
		*enabled_if*) ;;
		*)
			printf 'runtest: %s:%d: the cram stanza of %s has no enabled_if, so %s always runs: remove the line\n' \
				"$manifest" "$lineno" "$dune_file" "$1" >&2
			return 1
			;;
		esac
		case $stanza in
		*"%{env:$2="*) ;;
		*)
			printf 'runtest: %s:%d: the cram stanza of %s does not read %s, so the line would excuse %s on every run\n' \
				"$manifest" "$lineno" "$dune_file" "$2" "$1" >&2
			return 1
			;;
		esac

		printf '%s %s\n' "$1" "$2" >>"$entries"
	done <"$manifest"
}

if [ -e "$manifest" ]; then
	other=$(find test -name conditional.tests ! -path "$manifest")
	if [ -n "$other" ]; then
		printf 'runtest: %s is the conditional-test manifest; %s is read by nothing\n' \
			"$manifest" "$other" >&2
		exit 1
	fi
	check_manifest || exit 1
fi

while IFS= read -r test_file; do
	dune_file=${test_file%/*}/dune
	[ -f "$dune_file" ] || continue
	case $(cram_stanza "$dune_file") in
	*enabled_if*) ;;
	*) continue ;;
	esac
	if [ -z "$(declared "$test_file")" ]; then
		if [ -e "$manifest" ]; then
			printf 'runtest: %s runs under an enabled_if in %s and %s does not declare it\n' \
				"$test_file" "$dune_file" "$manifest" >&2
			printf 'runtest: add the line "%s <the variable the enabled_if reads>" to %s\n' \
				"$test_file" "$manifest" >&2
		else
			printf 'runtest: %s runs under an enabled_if in %s and %s is missing\n' \
				"$test_file" "$dune_file" "$manifest" >&2
			printf 'runtest: the file must declare it, or a switched-off test cannot be told from one that did not run\n' >&2
		fi
		exit 1
	fi
done <"$tests"

sed 's|/[^/]*$||' "$tests" | sort -u >"$dirs"

while IFS= read -r test_dir; do
	path="$build_dir/$context/$test_dir"
	[ -d "$path" ] || continue
	sh -c "$dune \"\$@\"" sh clean --root . "$path"
	status=$?
	if [ "$status" -ne 0 ]; then
		printf 'runtest: dune could not clear the records of %s\n' "$test_dir" >&2
		exit "$status"
	fi
done <"$dirs"

sh -c "$dune \"\$@\"" sh "$@"
status=$?
if [ "$status" -ne 0 ]; then
	exit "$status"
fi

executed=0
skipped=0
missing=0
while IFS= read -r test_file; do
	record="$build_dir/$context/$test_file"
	record="${record%/*}/.cram.${test_file##*/}/cram.out"

	var=$(declared "$test_file")
	if [ -n "$var" ]; then
		eval "value=\"\${$var-none}\""
		if [ "$value" = none ]; then
			if eval "[ -n \"\${$var+x}\" ]"; then
				reason="$var is none"
			else
				reason="$var is not set"
			fi
			printf 'runtest: %s not executed (%s)\n' "$test_file" "$reason"
			skipped=$((skipped + 1))
			continue
		fi
	fi

	if [ -f "$record" ]; then
		executed=$((executed + 1))
	else
		printf 'runtest: %s did not execute: no %s after the run\n' \
			"$test_file" "$record" >&2
		missing=$((missing + 1))
	fi
done <"$tests"

if [ "$missing" -ne 0 ]; then
	printf 'runtest: %d of %d cram tests did not execute\n' \
		"$missing" "$((missing + executed))" >&2
	exit 1
fi

if [ "$skipped" -eq 0 ]; then
	printf 'runtest: %d cram tests executed\n' "$executed"
else
	printf 'runtest: %d cram tests executed, %d not executed\n' \
		"$executed" "$skipped"
fi
