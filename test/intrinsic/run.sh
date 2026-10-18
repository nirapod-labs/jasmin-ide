#!/bin/sh
# The driver of the arity comparison (arity.ml).
#
#   sh test/intrinsic/run.sh <arity-program> [--jasminc <path>] [<corpus-root>]
#
# The program must agree: exit 0. Then each control runs with the same
# arguments and must differ: exit 1. A control that agrees, or that cannot be
# run, means the comparison cannot be trusted, and the step fails. When a corpus
# is named, each control must also differ in the corpus part, since a corpus
# comparison that no control can move has not been shown to fail.
set -u

if [ $# -lt 1 ]; then
	echo "usage: sh test/intrinsic/run.sh <arity-program> [--jasminc <path>] [<corpus-root>]" >&2
	exit 2
fi
program=$1
shift
if [ ! -x "$program" ]; then
	echo "arity: no program at $program (build test/intrinsic/arity.exe)" >&2
	exit 2
fi

"$program" "$@"
status=$?
if [ "$status" -ne 0 ]; then
	echo "arity: the comparison does not agree (exit $status)" >&2
	exit "$status"
fi

for control in ignore-arch drop-parameter; do
	out=$("$program" --control "$control" "$@" 2>&1)
	status=$?
	if [ "$status" -ne 1 ]; then
		echo "arity: control $control exited $status, expected 1: the comparison cannot be trusted" >&2
		exit 1
	fi
	if printf '%s\n' "$out" | grep -q '^corpus differences: '; then
		if ! printf '%s\n' "$out" | grep -q '^corpus differences: [1-9]'; then
			echo "arity: control $control differs in the table but not in the corpus: the corpus comparison cannot be trusted" >&2
			exit 1
		fi
	fi
	echo "control $control: differs, as it must (exit 1)"
done
