#!/bin/sh
# The measurement driver behind `make bench`.
#
# It refuses to start while a build is running, because a build makes every
# number meaningless, and then measures two things on each unit:
#
#   whole      a whole check, parse and type, in this process
#   keystroke  a didChange through the protocol loop to the diagnostics
#
# The units are the largest files of a Jasmin compiler tests directory and,
# when named, the rows of a units table. The corpus and the table are not
# part of this repository, so both come from the caller.
#
# Environment:
#   JASMIN_BENCH_CORPUS  a Jasmin compiler `tests` directory (required)
#   JASMIN_BENCH_UNITS   a units table to measure as well (optional)
#   JASMIN_BENCH_RUNS    checks per unit (default 20)
#   JASMIN_BENCH_COUNT   how many of the largest corpus files (default 4)
#   JASMIN_BENCH_EXE     the benchmark binary (the Makefile sets it)
#   JASMIN_BENCH_LOCKS   a directory of build slots, each held by a running
#                        build as build.lock.N (optional)
#   JASMIN_BENCH_WINDOW  a path that exists only inside a timing window
#                        (optional)
#
# A units table has one row per unit, four tab-separated fields:
#
#   label<TAB>arch<TAB>idirs<TAB>path
#
# `arch` is an architecture name or `auto`, which takes the first path
# component that names one; `idirs` is `-` or a comma-separated list of
# name=path include paths.

set -u

refuse() {
	printf 'bench: %s\n' "$*" >&2
	exit 2
}

root=$(cd -- "$(dirname -- "$0")/.." && pwd) || refuse "cannot find the repository"
exe=${JASMIN_BENCH_EXE:-$root/_build/default/bench/bench.exe}
corpus=${JASMIN_BENCH_CORPUS:-}
runs=${JASMIN_BENCH_RUNS:-20}
count=${JASMIN_BENCH_COUNT:-4}

# 1. Refuse to measure beside a build. A held slot is a build; so is a dune
#    process that is building, which catches a build started outside the
#    slots. Nothing of this script is a dune process, so no exclusion is
#    needed.
if [ -n "${JASMIN_BENCH_LOCKS:-}" ]; then
	for lock in "$JASMIN_BENCH_LOCKS"/build.lock.*; do
		[ -d "$lock" ] && refuse "a build is running ($lock is held)"
	done
fi
running=$(ps -A -o pid=,command= 2>/dev/null |
	awk '$2 == "dune" && /build/ { print "  " $0 }')
[ -z "$running" ] || refuse "a build is running:
$running"

# 2. Refuse to measure outside a timing window, when a caller names one.
if [ -n "${JASMIN_BENCH_WINDOW:-}" ] && [ ! -e "$JASMIN_BENCH_WINDOW" ]; then
	refuse "no timing window: $JASMIN_BENCH_WINDOW does not exist"
fi

# 3. A stale binary measures code that is no longer here.
if [ ! -x "$exe" ]; then
	refuse "no benchmark binary at $exe; run make bench-build first"
fi
stale=$(find "$root" \
	\( -name _build -o -name .git -o -name .worktrees \) -prune -o \
	-name '*.ml' -newer "$exe" -print 2>/dev/null | head -n 1)
[ -z "$stale" ] || refuse "$stale is newer than $exe; run make bench-build"

# 4. Refuse without a corpus: the repository holds none.
[ -n "$corpus" ] || refuse "no corpus: set JASMIN_BENCH_CORPUS to a Jasmin compiler tests directory"
[ -d "$corpus" ] || refuse "not a directory: $corpus"

table=${TMPDIR:-/tmp}/jasmin-bench.$$
: >"$table" || refuse "cannot write a temporary file at $table"
trap 'rm -f "$table"' EXIT HUP INT TERM

tab=$(printf '\t')
"$exe" largest --count "$count" "$corpus" |
while IFS=$tab read -r _ path; do
	printf '%s\tauto\t-\t%s\n' "${path#"$corpus"/}" "$path" >>"$table"
done

if [ -n "${JASMIN_BENCH_UNITS:-}" ]; then
	[ -f "$JASMIN_BENCH_UNITS" ] || refuse "no units table at $JASMIN_BENCH_UNITS"
	cat "$JASMIN_BENCH_UNITS" >>"$table"
fi

printf 'bench: %s\n' "$(date -u +%FT%TZ)"
printf 'bench: %d units, %s runs each\n' "$(wc -l <"$table" | tr -d ' ')" "$runs"
printf 'bench: load %s\n' "$(uptime | sed 's/.*load averages*: //')"
while IFS=$tab read -r label arch idirs path; do
	printf '  %s\t%s\t%s\t%s\n' "$label" "$arch" "$idirs" "$path"
done <"$table"

"$exe" whole --units "$table" --runs "$runs" --samples || exit $?
"$exe" keystroke --units "$table" --runs "$runs" --samples || exit $?

printf 'bench: %s\n' "$(date -u +%FT%TZ)"
