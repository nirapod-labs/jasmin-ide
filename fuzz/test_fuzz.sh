#!/bin/sh
# The harness's command line, and one short run of it.
#
#   test_fuzz.sh <fuzz_main>
#
# Every check prints a line only when it fails. Exit status: 0 when the
# command line behaves and a short run passes, 1 otherwise.

set -u

exe=${1:?usage: test_fuzz.sh <fuzz_main>}
failures=0

check() {
	name=$1
	shift
	if "$@"; then :; else
		failures=$((failures + 1))
		printf 'FAIL %s\n' "$name"
	fi
}

# run <status> <file> <args...>: run the harness, keep stdout and stderr in
# <file>, and check the status it exits with.
run() {
	want=$1
	file=$2
	shift 2
	"$exe" "$@" > "$file" 2>&1
	got=$?
	if [ "$got" -ne "$want" ]; then
		failures=$((failures + 1))
		printf 'FAIL %s exits %d, not %d\n' "$*" "$got" "$want"
		sed -n 1,5p "$file"
	fi
}

work=${TMPDIR:-/tmp}/fuzz-test.$$
mkdir -p "$work" || exit 2
trap 'rm -rf "$work"' EXIT INT TERM

run 2 "$work/help" --help
check "the usage is printed" grep -q 'usage: fuzz' "$work/help"

run 2 "$work/seed" --seed one
check "a seed that is not a number is named" \
	grep -q -- '--seed takes an integer' "$work/seed"

run 2 "$work/surface" --surface wire
check "a surface that does not exist is named" \
	grep -q -- '--surface takes both, text or edit' "$work/surface"

run 2 "$work/unknown" --quick
check "an unknown argument is named" grep -q 'unknown argument --quick' "$work/unknown"

run 0 "$work/short" --cases 5 --seconds 0 --seed 3
check "the seed is printed with the run" grep -q 'fuzz: seed 3' "$work/short"
check "the case count is printed with the run" grep -q 'text 5, edit 5' "$work/short"
check "the run reports no failure" grep -q '0 failures' "$work/short"

run 0 "$work/text" --cases 2 --seconds 0 --surface text
check "the text surface alone runs" grep -q 'text 2, edit 0' "$work/text"

run 0 "$work/edit" --cases 2 --seconds 0 --surface edit
check "the edit surface alone runs" grep -q 'text 0, edit 2' "$work/edit"

mkdir -p "$work/corpus/deep" || exit 2
printf 'fn f(reg u32 a) -> reg u32 {\n  return a;\n}\n' \
	> "$work/corpus/deep/unit.jazz"
run 0 "$work/corpus-run" --cases 4 --seconds 0 --corpus "$work/corpus"
check "the corpus is read" grep -q '1 corpus texts' "$work/corpus-run"

run 0 "$work/empty-corpus" --cases 2 --seconds 0 --corpus "$work/nowhere"
check "an empty corpus falls back to the generators" \
	grep -q 'no .jazz unit' "$work/empty-corpus"

if [ "$failures" -eq 0 ]; then
	printf 'all fuzz command-line tests passed\n'
	exit "$failures"
fi
printf '%d fuzz command-line test(s) failed\n' "$failures"
exit 1
