#!/bin/sh
# Run the recovery mutation harness over a corpus.
#
#   test/mutation/run.sh <corpus>... [harness options]
#
# <corpus> is a directory of Jasmin sources, searched recursively for .jazz
# files: the compiler/tests and compiler/examples directories of a Jasmin
# release are what the gate is measured on. The corpus is not part of this
# repository, so a caller names it; nothing about a host is written here.
#
# The harness is built first, with the dune the caller names in DUNE (the
# Makefile passes its own, which already goes through the heavy runner when
# one is set). The run prints to stdout; progress goes to stderr.

set -eu

if [ $# -lt 1 ]; then
	printf '%s\n' "usage: $0 <corpus>... [harness options]" >&2
	exit 2
fi

here=$(unset CDPATH; cd -- "$(dirname -- "$0")" && pwd)
root=$(unset CDPATH; cd -- "$here/../.." && pwd)
binary=$root/_build/default/test/mutation/mutation.exe

dune=${DUNE:-dune}

cd "$root" || exit 2
$dune build --root . test/mutation/mutation.exe -j 4
exec "$binary" "$@"
