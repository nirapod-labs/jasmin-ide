#!/bin/sh
# The measurement: rename and rename back over a corpus, with the compiler's
# verdict before and after.
#
#   sh test/parity/rename.sh <corpus root> <jasminc>
#
# <corpus root> is a tree of `.jazz` and `.jinc` files: a Jasmin release (its
# `compiler/tests` is used), or a library such as libjade. Every `.jazz` file
# is an entry. The harness copies the tree into a scratch directory, renames
# sampled declarations there through the server's own rename, asks <jasminc>
# for the verdict of every entry that reads an edited file, renames back and
# compares the bytes. It prints the counts by kind and one line per
# difference, and exits 0 only when every made rename restores the text and
# keeps the verdict.
#
# The corpus and the compiler are not part of this repository, so without them
# the step is a `skip` that the Makefile prints before this script runs. The
# harness needs `_build/default/test/parity/rename_trip.exe`, so build first:
# `make build` or `dune build --root . -j 4`.
#
# Environment:
#   RENAME_SEED    the sampling seed (default 1)
#   RENAME_SAMPLE  declarations drawn from each population (default 120)
#   RENAME_JOBS    compilers run at once (default 4)
#   RENAME_IDIRS   include paths, NAME=PATH separated by spaces, PATH under
#                  the corpus root (libjade needs Jade=.)
#   RENAME_PLANT   `skip` runs the negative control, which must exit 1
#   RENAME_FULL    any value runs the compiler through every pass, not to typing
#
# Exit status: the harness's own (0 all exact, 1 a difference, 2 it could not
# run), or 2 when the corpus, the compiler or the binary is missing.

set -u

if [ $# -ne 2 ]; then
	printf 'usage: %s <corpus root> <jasminc>\n' "${0##*/}" >&2
	exit 2
fi

corpus=$1
jasminc=$2
[ -d "$corpus" ] || {
	printf 'rename: no corpus directory: %s\n' "$corpus" >&2
	exit 2
}
[ -x "$jasminc" ] || {
	printf 'rename: no compiler at %s\n' "$jasminc" >&2
	exit 2
}

here=$(cd -- "$(dirname -- "$0")" && pwd)
harness=${JASMIN_RENAME_HARNESS:-$here/../../_build/default/test/parity/rename_trip.exe}
[ -x "$harness" ] || {
	printf 'rename: no harness at %s (build the tree first)\n' "$harness" >&2
	exit 2
}

set -- --corpus "$corpus" --jasminc "$jasminc" \
	--seed "${RENAME_SEED:-1}" --sample "${RENAME_SAMPLE:-120}" \
	--jobs "${RENAME_JOBS:-4}"
for binding in ${RENAME_IDIRS:-}; do
	set -- "$@" --idir "$binding"
done
case ${RENAME_PLANT:-} in
'') ;;
skip) set -- "$@" --plant skip ;;
*)
	printf 'rename: RENAME_PLANT takes skip, not %s\n' "$RENAME_PLANT" >&2
	exit 2
	;;
esac

if [ -n "${RENAME_FULL:-}" ]; then
	set -- "$@" --full
fi

printf 'rename: %s (%s)\n' "$harness" "$("$jasminc" -version 2>/dev/null | head -n 1)"
"$harness" "$@"
