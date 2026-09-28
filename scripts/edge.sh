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

