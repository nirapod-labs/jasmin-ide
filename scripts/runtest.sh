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

tests=$(mktemp "${TMPDIR:-/tmp}/jasmin-runtest-tests.XXXXXX") || exit 2
dirs=$(mktemp "${TMPDIR:-/tmp}/jasmin-runtest-dirs.XXXXXX") || exit 2
cleanup() { rm -f "$tests" "$dirs"; }
trap cleanup EXIT
trap 'cleanup; exit 130' INT TERM

find test -type f -name '*.t' | sort >"$tests"
if [ ! -s "$tests" ]; then
	printf 'runtest: no cram tests under test/, so a green test step would prove nothing\n' >&2
	exit 1
fi

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

tests_run=0
missing=0
while IFS= read -r test_file; do
	tests_run=$((tests_run + 1))
	record="$build_dir/$context/$test_file"
	record="${record%/*}/.cram.${test_file##*/}/cram.out"
	if [ ! -f "$record" ]; then
		printf 'runtest: %s did not execute: no %s after the run\n' \
			"$test_file" "$record" >&2
		missing=$((missing + 1))
	fi
done <"$tests"

if [ "$missing" -ne 0 ]; then
	printf 'runtest: %d of %d cram tests did not execute\n' \
		"$missing" "$tests_run" >&2
	exit 1
fi

printf 'runtest: %d cram tests executed\n' "$tests_run"
