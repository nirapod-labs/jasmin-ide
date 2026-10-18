#!/bin/sh
# The names completion offers against the resolver's scope, at bytes
# sampled the same way on every host.
#
#   sh test/parity/completion_scope.sh <corpus root> [--step N] [--seed N]
#                                      [--limit N]
#
# <corpus root> is a Jasmin release tree (the one that holds `compiler/`) or
# any directory holding `.jazz` and `.jinc` files; every one of them under it
# is sampled. The sample is the program's: every Nth byte, moved inside one
# step by the file's own hash and the seed, and capped at a total. The three
# numbers are printed by the run, so a rerun can name them.
#
# At each sampled byte inside a function the run compares the names
# `Complete.completion` offers (the lexer's keywords aside) with the names
# `Resolve.scope_at` holds there, filtered by the prefix completion read, and
# prints every position where the two differ. After every `#` in the corpus,
# the run compares what `Complete.completion` offers with the compiler's own
# intrinsic list, `-arch <arch> -help-intrinsics`, for every architecture. A
# `#[` asks for the annotation keys instead and is counted apart, and a byte
# outside every function is not a name position, so it is counted apart too. A
# byte the resolver never reached has no scope to compare with, so it is counted
# apart and fails the run. A negative control checks that the answers differ
# between architectures, so the comparison cannot pass for all of them at once.
#
# Environment:
#   JASMINC           the reference compiler, run once per architecture for the
#                     intrinsic list. Required: a run without it cannot compare
#                     the intrinsics, so it does not run.
#   COMPLETION_SCOPE  the program. Default: the one this tree builds.
#   COMPLETION_STEP   the distance between sampled bytes, default 13.
#   COMPLETION_SEED   the seed folded into the sample, default 1.
#   COMPLETION_LIMIT  the most name positions to compare, default 20000.
#
# Exit status: 0 when every name position compared is equal, none is unreached,
# every intrinsic answer equals the compiler's list and the negative control
# holds; 1 when one of those does not; 2 when the run cannot be made (no
# corpus, no program, no compiler, a bad argument).

set -u

name=completion-scope

say() { printf '%s: %s\n' "$name" "$*"; }
die() {
	say "$*" >&2
	exit 2
}

usage() {
	cat >&2 <<'USAGE'
usage: sh test/parity/completion_scope.sh <corpus root> [--step N] [--seed N] [--limit N]

environment: JASMINC (the reference compiler, required),
             COMPLETION_SCOPE (the program, default the built one),
             COMPLETION_STEP (default 13), COMPLETION_SEED (default 1),
             COMPLETION_LIMIT (default 20000)
USAGE
	exit 2
}

[ $# -ge 1 ] || usage
root=$1
shift
[ -d "$root" ] || die "no corpus directory: $root"
root=$(cd -- "$root" && pwd)

here=$(cd -- "$(dirname -- "$0")" && pwd)
program=${COMPLETION_SCOPE:-$here/../../_build/default/test/parity/completion_scope.exe}
[ -x "$program" ] ||
	die "no program at $program (build the tree, or set COMPLETION_SCOPE)"

compiler=${JASMINC:-}
[ -n "$compiler" ] ||
	die "no compiler: set JASMINC to the reference jasminc"
case $compiler in
*/*) [ -x "$compiler" ] || die "the compiler is not executable: $compiler" ;;
*) command -v "$compiler" >/dev/null 2>&1 || die "the compiler is not on PATH: $compiler" ;;
esac

step=${COMPLETION_STEP:-13}
seed=${COMPLETION_SEED:-1}
limit=${COMPLETION_LIMIT:-20000}
for number in "$step" "$seed" "$limit"; do
	case $number in
	'' | *[!0-9]*) die "not a number: $number" ;;
	esac
done

say "corpus $root"
say "compiler $compiler"
say "step $step, seed $seed, limit $limit"
exec "$program" --compiler "$compiler" --corpus "$root" --step "$step" --seed "$seed" --limit "$limit" "$@"
