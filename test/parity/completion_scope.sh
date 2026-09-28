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
# prints every position where the two differ. A byte after `#` asks for the
# annotation keys instead, and a byte outside every function is not a name
# position; both are counted apart, never compared. A byte the resolver never
# reached has no scope to compare with, so it is counted apart and fails the
# run.
#
# Environment:
#   COMPLETION_SCOPE  the program. Default: the one this tree builds.
#   COMPLETION_STEP   the distance between sampled bytes, default 13.
#   COMPLETION_SEED   the seed folded into the sample, default 1.
#   COMPLETION_LIMIT  the most positions to compare, default 20000.
#
# Exit status: 0 when every position compared is equal and none is unreached,
# 1 when one is not or one is unreached, 2 when the run cannot be made (no
# corpus, no program, a bad argument).

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

environment: COMPLETION_SCOPE (the program, default the built one),
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

step=${COMPLETION_STEP:-13}
seed=${COMPLETION_SEED:-1}
limit=${COMPLETION_LIMIT:-20000}
for number in "$step" "$seed" "$limit"; do
	case $number in
	'' | *[!0-9]*) die "not a number: $number" ;;
	esac
done

say "corpus $root"
say "step $step, seed $seed, limit $limit"
exec "$program" --corpus "$root" --step "$step" --seed "$seed" --limit "$limit" "$@"
