#!/bin/sh
# The measurement: one reference per typed call, over a corpus.
#
#   sh test/parity/calls.sh <corpus root>
#
# <corpus root> is the `compiler/tests` directory of a Jasmin release, the
# `compiler` directory that holds it, or the release root that holds that. The
# harness reads every `.jazz` file under it, types it with the compiler's own
# check, and asks the engine's references question at the byte each typed call
# writes its callee at. It prints the counts and one line per difference, and
# exits 0 only when every typed call has exactly one reference to its callee
# inside the instruction that makes it.
#
# The corpus is not part of this repository, so without one the step is a
# `skip` that the Makefile prints before this script runs. The harness needs
# `_build/default/test/parity/calls.exe`, so build first: `make build` or
# `dune build --root . -j 4`.
#
# Environment:
#   JASMIN_CALLS_LIMIT  read at most that many files (a quick run)
#
# Exit status: the harness's own (0 all one, 1 a difference, 2 it could not
# run), or 2 when the corpus or the binary is missing.

set -u

if [ $# -ne 1 ]; then
	printf 'usage: %s <corpus root>\n' "${0##*/}" >&2
	exit 2
fi

corpus=$1
[ -d "$corpus" ] || {
	printf 'calls: no corpus directory: %s\n' "$corpus" >&2
	exit 2
}

here=$(cd -- "$(dirname -- "$0")" && pwd)
harness=${JASMIN_CALLS_HARNESS:-$here/../../_build/default/test/parity/calls.exe}
[ -x "$harness" ] || {
	printf 'calls: no harness at %s (build the tree first)\n' "$harness" >&2
	exit 2
}

limit=${JASMIN_CALLS_LIMIT:-}
set -- --corpus "$corpus"
if [ -n "$limit" ]; then
	set -- "$@" --limit "$limit"
fi

printf 'calls: %s\n' "$harness"
"$harness" "$@"
