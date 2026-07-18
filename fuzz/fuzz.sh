#!/bin/sh
# Run the fuzz harness for a bounded time.
#
#   fuzz/fuzz.sh <corpus> <seconds> <seed>
#
# <corpus> may be empty, may be a directory of `.jazz` units, or may be the
# release root that holds `compiler/tests`; without one the harness generates
# its own texts. The binary is the one this workspace builds, so the build
# step has to have run: a run that cannot find it says so and exits 2 rather
# than building one inside the run.
#
# Everything else is the harness's own command line; `--help` there lists it.
# A run's last line carries the seed and the case count, and a failing case
# prints the seed and the ordinal that replay it.

set -u

usage() {
	printf '%s\n' \
		'usage: fuzz/fuzz.sh <corpus> <seconds> <seed>' \
		'<corpus> is a Jasmin corpus such as the compiler/tests directory of the' \
		'release the server is built against, or empty for generated texts.' \
		'"0" seconds runs until the case count, which is not set here, so it runs' \
		'no case at all; give a positive number of seconds.'
}

if [ $# -ne 3 ]; then
	usage
	exit 2
fi

corpus=$1
seconds=$2
seed=$3

here=$(unset CDPATH; cd -- "$(dirname -- "$0")" && pwd)
root=$(unset CDPATH; cd -- "$here/.." && pwd)
binary=${FUZZ_BIN:-$root/_build/default/fuzz/fuzz_main.exe}
if [ ! -x "$binary" ]; then
	printf '%s\n' "fuzz: no harness at $binary; build it first" >&2
	exit 2
fi

# A caller may name the release root instead of the tests directory, which is
# one level down.
if [ -n "$corpus" ] && [ -d "$corpus/compiler/tests" ]; then
	corpus=$corpus/compiler/tests
fi

if [ -n "$corpus" ]; then
	exec "$binary" --seconds "$seconds" --seed "$seed" --corpus "$corpus"
fi
exec "$binary" --seconds "$seconds" --seed "$seed"
