#!/bin/sh
# The engine's intrinsic table against the compiler's own list, per
# architecture.
#
#   sh test/parity/intrinsics.sh [<compiler>]
#
# <compiler> is a command that takes the compiler's arguments: `-arch <arch>`
# and `-help-intrinsics` for the list, `-arch <arch> -until_typing <file>` for
# the suffix probes. A binary works, and so does a wrapper that runs the
# compiler somewhere else, such as in a virtual machine. Without an argument
# the compiler is `$JASMINC`, and without either the run says what it looked
# for and exits 2: a run that compared nothing must not look green.
#
# For each architecture the program prints the compiler's list, the engine's
# table, the number of names and suffixes compared and every difference. It
# exits 1 when one is not equal, so the gate's answer is its exit status.
#
# Environment:
#   JASMINC          the compiler command, when no argument is given.
#   INTRINSICS       the program. Default: the one this tree builds.
#   INTRINSICS_RAW   a directory to write the compiler's own words to: the
#                    `-help-intrinsics` text per architecture and one line
#                    per probe. Empty by default.
#   PARITY_JOBS      how many compiler runs at once, default 4.
#   INTRINSICS_ARCH  a space-separated architecture list, default the engine's.

set -u

name=intrinsics

say() { printf '%s: %s\n' "$name" "$*"; }
die() {
	say "$*" >&2
	exit 2
}

if [ $# -gt 1 ]; then
	echo "usage: sh test/parity/intrinsics.sh [<compiler>]" >&2
	exit 2
fi
compiler=${1:-${JASMINC:-}}
[ -n "$compiler" ] ||
	die "no compiler: name one, or set JASMINC to the reference jasminc"
case $compiler in
*/*) [ -x "$compiler" ] || die "the compiler is not executable: $compiler" ;;
*) command -v "$compiler" >/dev/null 2>&1 || die "the compiler is not on PATH: $compiler" ;;
esac

here=$(cd -- "$(dirname -- "$0")" && pwd)
program=${INTRINSICS:-$here/../../_build/default/test/parity/intrinsics.exe}
[ -x "$program" ] || die "no program at $program (build the tree, or set INTRINSICS)"

jobs=${PARITY_JOBS:-4}
case $jobs in
'' | *[!0-9]*) die "PARITY_JOBS is not a number: $jobs" ;;
esac
[ "$jobs" -ge 1 ] || die "PARITY_JOBS is below one: $jobs"

set -- "$program" --compiler "$compiler" --jobs "$jobs"
if [ -n "${INTRINSICS_RAW:-}" ]; then
	mkdir -p "$INTRINSICS_RAW" || die "cannot make INTRINSICS_RAW=$INTRINSICS_RAW"
	set -- "$@" --raw-dir "$INTRINSICS_RAW"
fi
if [ -n "${INTRINSICS_ARCH:-}" ]; then
	for arch in $INTRINSICS_ARCH; do
		set -- "$@" --arch "$arch"
	done
fi

say "compiler $compiler"
say "jobs $jobs${INTRINSICS_ARCH:+, architectures $INTRINSICS_ARCH}"
exec "$@"
