#!/bin/sh
# The measurement driver behind `make bench`.
#
# It refuses to start outside a timing window and while a build is running,
# because both make every number meaningless, and then measures three things
# on each unit:
#
#   whole      a whole check, parse and type, in this process
#   keystroke  a didChange through the protocol loop to the diagnostics
#   request    completion, signature help, hover, definition, references and
#              rename on a snapshot the server already holds
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
#   JASMIN_BENCH_WINDOW  a path that exists only inside a timing window
#                        (required; JASMIN_BENCH_NO_WINDOW=1 measures anyway)
#   JASMIN_BENCH_NO_WINDOW
#                        set to anything to measure without a window; the
#                        run says so, and its numbers are not window evidence
#   JASMIN_BENCH_LOCKS   a directory of build slots, each held by a running
#                        build as build.lock.N (default: the window's own
#                        directory)
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
window=${JASMIN_BENCH_WINDOW:-}

# 1. Refuse to print a number outside a timing window. The window is the
#    caller's to open and the caller's to name, so a run that cannot name one
#    is a refusal rather than a measurement: a number taken beside a build is
#    a measurement of the build. JASMIN_BENCH_NO_WINDOW is the one explicit
#    way past this, and it says on stderr that the numbers are not window
#    evidence. docs/bench.md has the recipe: open the window, then name it.
if [ -z "$window" ]; then
	if [ -n "${JASMIN_BENCH_NO_WINDOW:-}" ]; then
		printf 'bench: measuring outside a timing window (JASMIN_BENCH_NO_WINDOW is set); the numbers are not window evidence\n' >&2
	else
		refuse "no timing window: open one and set JASMIN_BENCH_WINDOW to the path it creates, or set JASMIN_BENCH_NO_WINDOW=1 to measure anyway (not window evidence); docs/bench.md"
	fi
elif [ ! -e "$window" ]; then
	refuse "no timing window: $window does not exist (open one first, or set JASMIN_BENCH_NO_WINDOW=1)"
fi

# 2. Refuse to measure beside a build. A held slot is a build; so is a dune
#    process that is building, which catches a build started outside the
#    slots. Nothing of this script is a dune process, so no exclusion is
#    needed.
#
#    The slots are the window's neighbours unless the caller names another
#    directory, so a caller who names only the window still gets the slot
#    check; the process probe runs whether or not a slot directory is named.
locks=${JASMIN_BENCH_LOCKS:-}
if [ -z "$locks" ] && [ -n "$window" ]; then
	locks=$(dirname -- "$window")
fi
if [ -n "$locks" ]; then
	for lock in "$locks"/build.lock.*; do
		[ -d "$lock" ] && refuse "a build is running ($lock is held)"
	done
fi
running=$(ps -A -o pid=,command= 2>/dev/null |
	awk '$2 == "dune" && /build/ { print "  " $0 }')
[ -z "$running" ] || refuse "a build is running:
$running"

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
"$exe" request --units "$table" --runs "$runs" --samples || exit $?

printf 'bench: %s\n' "$(date -u +%FT%TZ)"
