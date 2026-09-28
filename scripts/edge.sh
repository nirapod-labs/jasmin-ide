#!/bin/sh

set -u

name=edge
here=$(cd -- "$(dirname -- "$0")/.." && pwd) || exit 2

say() { printf '%s: %s\n' "$name" "$*"; }
warn() { printf '%s: %s\n' "$name" "$*" >&2; }

usage() {
	cat >&2 <<'USAGE'
usage: sh scripts/edge.sh [<jasmin-checkout>]

  <jasmin-checkout>  a git checkout of the Jasmin commit to build; without
                     it, or with a missing directory, the step skips

environment: JASMIN_EDGE_OPAM_SWITCH (a switch whose Rocq runs the
extraction), JASMIN_EDGE_RUNNER (a heavy-step runner), JASMIN_EDGE_DUNE
(default dune), JASMIN_EDGE_JOBS (default 4), JASMIN_EDGE_KEEP (a directory
to build in and keep)

The switch is the caller's. JASMIN_EDGE_OPAM_SWITCH names one, and the only
values this script reads are the lab's own `jasmin-ide` switch and a
throwaway switch named `jide-edge-<name>` that the caller creates and removes
through `tools/heavy.sh`; any other name, another lab's switch included, is
refused before an opam command runs. Empty is the default, and then no opam
switch is read at all: the script runs no opam command and takes coqc or rocq
from PATH.
USAGE
	exit 2
}

case ${1:-} in
-h | --help) usage ;;
esac
if [ "$#" -gt 1 ]; then usage; fi

repo=$here
src=${1:-}

if [ -z "$src" ]; then
	say "skip edge (no Jasmin checkout: set JASMIN_EDGE_SRC)"
	exit 0
fi
if [ ! -d "$src" ]; then
	say "skip edge (no Jasmin checkout at $src)"
	exit 0
fi
src=$(cd -- "$src" && pwd) || exit 2

if ! git -C "$src" rev-parse --git-dir >/dev/null 2>&1; then
	warn "not a git checkout: $src (the commit cannot be named)"
	exit 2
fi

switch=${JASMIN_EDGE_OPAM_SWITCH:-}
runner=${JASMIN_EDGE_RUNNER:-}
dune=${JASMIN_EDGE_DUNE:-dune}
jobs=${JASMIN_EDGE_JOBS:-4}
keep=${JASMIN_EDGE_KEEP:-}

case $jobs in
'' | *[!0-9]*) warn "JASMIN_EDGE_JOBS is not a number: $jobs"; exit 2 ;;
esac
[ "$jobs" -ge 1 ] || {
	warn "JASMIN_EDGE_JOBS is below one: $jobs"
	exit 2
}

case $switch in
'' | jasmin-ide | jide-edge-*) ;;
*)
	warn "refusing switch $switch: name the lab's own jasmin-ide switch, or a throwaway jide-edge-<name> switch created and removed through tools/heavy.sh"
	exit 2
	;;
esac

if [ -n "$switch" ]; then
	if ! command -v opam >/dev/null 2>&1; then
		warn "no opam, so the switch named in JASMIN_EDGE_OPAM_SWITCH cannot be read"
		exit 2
	fi
	if rocq_version=$(opam exec --switch "$switch" -- coqc --version 2>/dev/null); then
		:
	elif rocq_version=$(opam exec --switch "$switch" -- rocq --version 2>/dev/null); then
		:
	else
		warn "no Rocq in opam switch $switch"
		exit 2
	fi
else
	if command -v coqc >/dev/null 2>&1; then
		rocq_version=$(coqc --version 2>/dev/null)
	elif command -v rocq >/dev/null 2>&1; then
		rocq_version=$(rocq --version 2>/dev/null)
	else
		say "skip edge (no Rocq: set JASMIN_EDGE_OPAM_SWITCH or put coqc on PATH)"
		exit 0
	fi
fi
rocq_version=$(printf '%s\n' "$rocq_version" | sed -n 1p)

if [ -n "$keep" ]; then
	if [ -e "$keep" ] && [ -n "$(ls -A "$keep" 2>/dev/null)" ]; then
		warn "JASMIN_EDGE_KEEP is not empty: $keep"
		exit 2
	fi
	mkdir -p "$keep" || exit 2
	work=$(cd -- "$keep" && pwd) || exit 2
	cleanup() { :; }
else
	work=$(mktemp -d "${TMPDIR:-/tmp}/jasmin-edge.XXXXXX") || exit 2
	cleanup() { rm -rf "$work"; }
fi
trap cleanup EXIT HUP INT TERM

steps_log=$work/steps.tsv
: >"$steps_log"

now() {
	t=$(date +%s%N 2>/dev/null)
	case $t in
	'' | *[!0-9]*) echo $(($(date +%s) * 10)) ;;
	*) echo $((t / 100000000)) ;;
	esac
}

seconds() { printf '%d.%ds' $(($1 / 10)) $(($1 % 10)); }

step() {
	step_name=$1
	step_body=$2
	step_start=$(now)
	"$step_body"
	step_status=$?
	step_elapsed=$(($(now) - step_start))
	printf '%s\t%s\t%s\n' "$step_name" "$step_status" "$(seconds "$step_elapsed")" >>"$steps_log"
	if [ "$step_status" -eq 0 ]; then
		printf 'ok %s %s\n' "$step_name" "$(seconds "$step_elapsed")"
		return 0
	fi
	printf 'fail %s %s (exit %s)\n' "$step_name" "$(seconds "$step_elapsed")" "$step_status"
	return "$step_status"
}

all_steps="source tree rocq cil dist digest vendor build parity"

after() {
	seen=
	for s in $all_steps; do
		[ -n "$seen" ] && printf '%s\n' "$s"
		[ "$s" = "$1" ] && seen=1
	done
}

stop() {
	code=$2
	[ "$code" -ne 0 ] || code=1
	for s in $(after "$1"); do
		printf 'skip %s (the %s step failed)\n' "$s" "$1"
	done
	printf 'edge: upstream %s\n' "${commit:-unknown}"
	printf 'edge: %s failed with exit %s\n' "$1" "$2"
	exit "$code"
}

commit=
subject=
when=

