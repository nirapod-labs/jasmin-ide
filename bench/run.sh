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

