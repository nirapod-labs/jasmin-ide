#!/bin/bash
#
#   scripts/build-static.sh <linux-x86_64|linux-aarch64|macos-aarch64> [<output>]
#
#
#
#
#
#
#
#

set -eu -o pipefail

here=$(unset CDPATH && cd -- "$(dirname -- "$0")/.." && pwd)

usage() {
	printf 'usage: %s <linux-x86_64|linux-aarch64|macos-aarch64> [<output>]\n' \
		"${0##*/}" >&2
	exit 2
}

say() { printf '%s: %s\n' "${0##*/}" "$*"; }
refuse() {
	printf '%s: refusing: %s\n' "${0##*/}" "$*" >&2
	exit 2
}

jobs=${DUNE_JOBS:-4}
epoch=${SOURCE_DATE_EPOCH:-}
if [ -z "$epoch" ]; then
	epoch=$(git -C "$here" log -1 --format=%ct 2>/dev/null) || epoch=
	epoch=${epoch:-0}
fi

stage=

remove_stage() {
	if [ -n "$stage" ] && [ -d "$stage" ]; then
		rm -rf "$stage"
	fi
	stage=
}
trap remove_stage EXIT
trap 'remove_stage; exit 130' INT TERM

dune_command() {
	if [ -n "${DUNE:-}" ]; then
		printf '%s\n' "$DUNE"
	elif command -v dune >/dev/null 2>&1; then
		printf 'dune\n'
	elif command -v opam >/dev/null 2>&1; then
		printf 'opam exec --switch %s -- dune\n' "${OPAM_SWITCH:-jasmin-ide}"
	else
		refuse "no dune and no opam on PATH, so the server cannot be built"
	fi
}

run_dune() {
	workspace=$1
	build_dir=$2
	shift 2
	if [ -n "${HEAVY:-}" ]; then
		SOURCE_DATE_EPOCH=$epoch $HEAVY $(dune_command) build --root "$here" \
			--profile release --build-dir "$build_dir" \
			--workspace "$workspace" -j "$jobs" "$@"
	else
		SOURCE_DATE_EPOCH=$epoch $(dune_command) build --root "$here" \
			--profile release --build-dir "$build_dir" \
			--workspace "$workspace" -j "$jobs" "$@"
	fi
}

#
write_flags_workspace() {
	path=$1
	shift
	{
		printf '(lang dune 3.24)\n'
		printf '(env\n (_\n  (link_flags\n   ('
		for flag in "$@"; do
			printf '%s ' "$flag"
		done
		printf '))))\n'
	} >"$path"
}

