#!/bin/bash
# Build the release artefacts twice, from two clean trees at one commit, and
# record whether each file matched.
#
#
#
#
#
#
#

set -eu
if (set -o pipefail) 2>/dev/null; then set -o pipefail; fi

usage() {
	printf 'usage: %s <tree-a> <tree-b> <output-directory> [--require-identical]\n' \
		"${0##*/}" >&2
	exit 2
}

say() { printf '%s: %s\n' "${0##*/}" "$*"; }
die() {
	printf '%s: error: %s\n' "${0##*/}" "$*" >&2
	exit 1
}

require_identical=no
if [ "$#" -eq 4 ]; then
	case $4 in
	--require-identical) require_identical=yes ;;
	*) usage ;;
	esac
fi
[ "$#" -eq 3 ] || [ "$#" -eq 4 ] || usage
tree_a=$1
tree_b=$2
out=$3

jobs=${DUNE_JOBS:-4}
flags=${JIDE_LINK_FLAGS:-}
src_prefix=${JIDE_SRC_PREFIX:-jasmin-lsp/}

artefacts=${JIDE_ARTEFACTS-server src.tar.gz}
count=0
for name in $artefacts; do
	case $name in
	server | src.tar.gz) ;;
	*) usage ;;
	esac
	count=$((count + 1))
done
[ "$count" -gt 0 ] ||
	die "JIDE_ARTEFACTS names no artefact, so a comparison of none would pass while measuring nothing"

case $src_prefix in
'' | /* | *..* | *' '*) usage ;;
	*/) ;;
	*) usage ;;
esac

remove_stage() {
	if [ -n "${stage:-}" ] && [ -d "$stage" ]; then
		rm -rf "$stage"
	fi
	stage=
}
trap remove_stage EXIT
trap 'remove_stage; exit 130' INT TERM

dune_command() {
	if [ -n "${DUNE:-}" ]; then
		printf '%s\n' "$DUNE"
	elif command -v dune >/dev/null 2>&1; then
		printf 'dune\n'
	elif command -v opam >/dev/null 2>&1; then
		printf 'opam exec --switch %s -- dune\n' "${OPAM_SWITCH:-jasmin-ide}"
	else
		die "no dune and no opam on PATH, so the server cannot be built"
	fi
}

commit_of() {
	git -C "$1" rev-parse --verify HEAD 2>/dev/null ||
		die "$1 is not a git work tree, so the commit it holds cannot be named"
}

check_tree() {
	tree=$1
	[ -d "$tree" ] || die "no such tree: $tree"
	[ -d "$tree/vendor/jasmin" ] ||
		die "$tree has no vendor/jasmin, so its build would not link the library the repository vendors; assemble it with vendor/fetch.sh first"
	status=$(git -C "$tree" status --porcelain --untracked-files=normal 2>/dev/null) ||
		die "$tree is not a git work tree, so it cannot be shown to be clean"
	[ -z "$status" ] ||
		die "$tree is not clean, so a difference could come from the tree and not from the build: $(printf '%s' "$status" | head -3 | tr '\n' ' ')"
}

check_tree "$tree_a"
check_tree "$tree_b"

commit_a=$(commit_of "$tree_a")
commit_b=$(commit_of "$tree_b")
[ "$commit_a" = "$commit_b" ] ||
	die "the trees are at different commits ($commit_a and $commit_b), so a comparison of their builds would measure the difference between two sources"

epoch=${SOURCE_DATE_EPOCH:-}
if [ -z "$epoch" ]; then
	epoch=$(git -C "$tree_a" log -1 --format=%ct) || epoch=
	epoch=${epoch:-0}
fi

mkdir -p "$out" || die "cannot make $out"

write_flags_workspace() {
	path=$1
	{
		printf '(lang dune 3.24)\n'
		printf '(env\n (_\n  (link_flags\n   ('
		for flag in $flags; do
			printf '%s ' "$flag"
		done
		printf '))))\n'
	} >"$path"
}

build_server() {
	tree=$1
	destination=$2
	stage=$(mktemp -d "${TMPDIR:-/tmp}/jasmin-double.XXXXXX")
	write_flags_workspace "$stage/dune-workspace"
	if ! SOURCE_DATE_EPOCH=$epoch $(dune_command) build --root "$tree" \
		--profile release --build-dir "$stage/build" \
		--workspace "$stage/dune-workspace" -j "$jobs" \
		lsp-server/native/main.exe; then
		remove_stage
		die "the build in $tree failed"
	fi
	[ -f "$stage/build/default/lsp-server/native/main.exe" ] ||
		die "the build in $tree reported success but wrote no lsp-server/native/main.exe"
	rm -f "$destination" ||
		die "cannot replace $destination"
	cp "$stage/build/default/lsp-server/native/main.exe" "$destination"
	remove_stage
}

build_src() {
	tree=$1
	destination=$2
	absolute=$(cd "$(dirname "$destination")" && pwd)/$(basename "$destination")
	git -C "$tree" archive --format=tar.gz --prefix="$src_prefix" \
		-o "$absolute" "$commit_a" ||
		die "git archive failed in $tree"
	[ -s "$destination" ] ||
		die "git archive in $tree wrote no bytes"
}

build() {
	case $1 in
	server) build_server "$2" "$3" ;;
	src.tar.gz) build_src "$2" "$3" ;;
	*) usage ;;
	esac
}

build_pass() {
	tree=$1
	pass=$2
	[ -d "$out/pass-$pass" ] || mkdir -p "$out/pass-$pass" ||
		die "cannot make $out/pass-$pass"
	for name in $artefacts; do
		build "$name" "$tree" "$out/pass-$pass/$name"
	done
}

say "building $count artefact(s) in $tree_a (pass a) at $commit_a"
build_pass "$tree_a" a
say "building $count artefact(s) in $tree_b (pass b) at $commit_b"
build_pass "$tree_b" b

sha_of() {
	if command -v sha256sum >/dev/null 2>&1; then
		sha256sum "$1" | cut -d' ' -f1
	elif command -v shasum >/dev/null 2>&1; then
		shasum -a 256 "$1" | cut -d' ' -f1
	else
		die "neither sha256sum nor shasum is installed"
	fi
}

bytes_of() { wc -c <"$1" | tr -d ' '; }

occurs_in() {
	if grep -qF -- "$1" "$2" 2>/dev/null; then printf 'yes'; else printf 'no'; fi
}

record=$out/double-build.txt
{
	printf '# Double build of the release artefacts, from two clean trees at one commit\n'
	printf 'commit %s\n' "$commit_a"
	printf 'tree-a %s\n' "$tree_a"
	printf 'tree-b %s\n' "$tree_b"
	printf 'source-date-epoch %s\n' "$epoch"
	printf 'platform %s %s\n' "$(uname -s)" "$(uname -m)"
	printf 'profile release\n'
	printf 'jobs %s\n' "$jobs"
	if [ -n "$flags" ]; then
		printf 'link-flags %s\n' "$flags"
	else
		printf 'link-flags (none)\n'
	fi
	printf 'src-prefix %s\n' "$src_prefix"
	printf 'artefacts %s\n' "$count"
} >"$record"

compared=0
differing=0
for name in $artefacts; do
	a=$out/pass-a/$name
	b=$out/pass-b/$name
	[ -f "$a" ] || die "pass a wrote no $name"
	[ -f "$b" ] || die "pass b wrote no $name"
	sha_a=$(sha_of "$a")
	sha_b=$(sha_of "$b")
	bytes_a=$(bytes_of "$a")
	bytes_b=$(bytes_of "$b")
	compared=$((compared + 1))
	{
		printf 'artefact %s\n' "$name"
		printf 'a:sha256 %s\n' "$sha_a"
		printf 'a:bytes %s\n' "$bytes_a"
		printf 'b:sha256 %s\n' "$sha_b"
		printf 'b:bytes %s\n' "$bytes_b"
	} >>"$record"
	if [ "$sha_a" = "$sha_b" ]; then
		printf 'match yes\n' >>"$record"
		say "identical $name ${sha_a} (${bytes_a} bytes)"
		continue
	fi

	differing=$((differing + 1))
	#
	set +e
	n=$(cmp -l "$a" "$b" 2>/dev/null | wc -l | tr -d ' ')
	first=$(cmp "$a" "$b" 2>&1 |
		sed -n -e 's/.* char \([0-9][0-9]*\),.*/\1/p' \
			-e 's/.* byte \([0-9][0-9]*\),.*/\1/p' | head -1)
	set -e
	{
		printf 'match no\n'
		printf 'differing-bytes %s\n' "$n"
		printf 'first-differing-byte %s\n' "${first:-unknown}"
		printf 'a:holds-tree-a-path %s\n' "$(occurs_in "$tree_a" "$a")"
		printf 'a:holds-tree-b-path %s\n' "$(occurs_in "$tree_b" "$a")"
		printf 'b:holds-tree-a-path %s\n' "$(occurs_in "$tree_a" "$b")"
		printf 'b:holds-tree-b-path %s\n' "$(occurs_in "$tree_b" "$b")"
	} >>"$record"
	say "differ $name ($n of $bytes_a bytes; first at ${first:-unknown})"
done

[ "$compared" -gt 0 ] ||
	die "compared no artefact, so there is nothing this run can claim"

if [ "$differing" -eq 0 ]; then
	verdict=identical
else
	verdict=differ
fi
{
	printf 'verdict %s\n' "$verdict"
	printf 'artefacts-compared %s\n' "$compared"
	printf 'artefacts-differing %s\n' "$differing"
} >>"$record"

if [ "$verdict" = identical ]; then
	say "double build: $compared of $compared artefacts identical"
else
	say "double build: $differing of $compared artefacts differ"
fi
say "record: $record"

if [ "$verdict" = differ ] && [ "$require_identical" = yes ]; then
	die "the two builds of $commit_a are not identical, and --require-identical was given"
fi
