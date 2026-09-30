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

step_source() {
	commit=$(git -C "$src" rev-parse HEAD 2>/dev/null) || return 2
	[ -n "$commit" ] || return 2
	dirty=$(git -C "$src" status --porcelain 2>/dev/null) || return 2
	if [ -n "$dirty" ]; then
		warn "$src has uncommitted changes, so it is not the commit this run would name"
		return 2
	fi
	subject=$(git -C "$src" log -1 --format=%s 2>/dev/null) || return 2
	when=$(git -C "$src" log -1 --format=%aI 2>/dev/null) || return 2
	printf 'edge: source   %s\n' "$src"
	printf 'edge: commit   %s\n' "$commit"
	printf 'edge: date     %s\n' "$when"
	printf 'edge: subject  %s\n' "$subject"
}

tree=$work/src

step_tree() {
	mkdir -p "$tree" || return 2
	git -C "$src" archive --format=tar HEAD | tar -x -C "$tree" || return 2
	[ -f "$tree/compiler/Makefile" ] || {
		warn "no compiler/Makefile in the export of $commit"
		return 1
	}
	printf 'edge: export   %s\n' "$tree"
}

step_rocq() {
	if [ -n "$switch" ]; then
		printf 'edge: rocq     %s (opam switch %s)\n' "$rocq_version" "$switch"
	else
		printf 'edge: rocq     %s (PATH)\n' "$rocq_version"
	fi
}

make_prefix=
if [ -n "$switch" ]; then
	make_prefix="opam exec --switch $switch -- "
fi

step_cil() {
	cd "$tree" || return 2
	printf 'edge: $        %s%smake -C compiler CIL -j %s\n' "$runner " "$make_prefix" "$jobs"
	$runner $make_prefix make -C compiler CIL -j "$jobs"
	status=$?
	cd "$repo" || return 2
	[ "$status" -eq 0 ] || return "$status"
	[ -f "$tree/compiler/src/CIL/expr.ml" ] || {
		warn "no compiler/src/CIL/expr.ml after the extraction"
		return 1
	}
	printf 'edge: CIL      %s modules\n' "$(ls "$tree/compiler/src/CIL"/*.ml 2>/dev/null | wc -l | tr -d ' ')"
}

install_shim() {
	probe=$work/install-probe
	mkdir -p "$probe" || return 1
	: >"$probe/file" || return 1
	if install -m 0644 -t "$probe" "$probe/file" >/dev/null 2>&1; then
		rm -rf "$probe"
		return 0
	fi
	rm -rf "$probe"
	return 1
}

make_install_shim() {
	real=$(command -v install) || return 1
	mkdir -p "$work/bin" || return 1
	{
		printf '#!/bin/bash\n'
		printf '# GNU install -t DIR for a host whose install is BSD.\n'
		printf 'set -eu\n'
		printf 'out=\n'
		printf 'args=()\n'
		printf 'while [ $# -gt 0 ]; do\n'
		printf '  case $1 in\n'
		printf '  -t) out=$2; shift 2 ;;\n'
		printf '  -t*) out=${1#-t}; shift ;;\n'
		printf '  *) args+=("$1"); shift ;;\n'
		printf '  esac\n'
		printf 'done\n'
		printf 'if [ -n "$out" ]; then exec %s "${args[@]}" "$out"; fi\n' "$real"
		printf 'exec %s "${args[@]}"\n' "$real"
	} >"$work/bin/install" || return 1
	chmod +x "$work/bin/install" || return 1
	printf 'edge: install  %s takes no -t, so the dist step runs a shim\n' "$real"
}

dist_path=$PATH
if ! install_shim; then
	make_install_shim || {
		warn "the host's install has no -t, and no shim could be made"
		exit 2
	}
	dist_path=$work/bin:$PATH
fi

tarball=$tree/compiler/jasmin-compiler.tgz

step_dist() {
	cd "$tree" || return 2
	printf 'edge: $        make -C compiler dist\n'
	PATH=$dist_path make -C compiler dist
	status=$?
	cd "$repo" || return 2
	[ "$status" -eq 0 ] || return "$status"
	[ -f "$tarball" ] || {
		warn "no compiler/jasmin-compiler.tgz after the distribution step"
		return 1
	}
	printf 'edge: tarball  %s\n' "$tarball"
}

digest=

sha256_of() {
	if command -v sha256sum >/dev/null 2>&1; then
		sha256sum "$1" | cut -d' ' -f1
	elif command -v shasum >/dev/null 2>&1; then
		shasum -a 256 "$1" | cut -d' ' -f1
	else
		return 1
	fi
}

step_digest() {
	digest=$(sha256_of "$tarball") || {
		warn "neither sha256sum nor shasum is installed"
		return 2
	}
	[ -n "$digest" ] || return 1
	printf 'edge: sha256   %s\n' "$digest"
	printf 'edge: bytes    %s\n' "$(wc -c <"$tarball" | tr -d ' ')"
}

step_vendor() {
	printf 'edge: $        sh vendor/fetch.sh <tarball> %s\n' "$digest"
	sh vendor/fetch.sh "$tarball" "$digest" || return "$?"
	[ "$(cat vendor/jasmin/.source-sha256 2>/dev/null)" = "$digest" ] || {
		warn "vendor/jasmin/ is not assembled from $digest"
		return 1
	}
}

step_build() {
	JIDE_JASMIN_CHANNEL=edge
	export JIDE_JASMIN_CHANNEL
	printf 'edge: $        JIDE_JASMIN_CHANNEL=%s %s build --root . @install -j %s\n' \
		"$JIDE_JASMIN_CHANNEL" "$dune" "$jobs"
	$runner $dune build --root . @install -j "$jobs"
	status=$?
	[ "$status" -eq 0 ] || return "$status"
	binary=./_build/install/default/bin/jasmin-lsp
	[ -x "$binary" ] || {
		warn "no executable at $binary"
		return 1
	}
	version=$("$binary" --version 2>/dev/null) || version=
	printf 'edge: server   %s\n' "${version:-no version printed}"
	printf 'edge: binary   %s\n' "$binary"
}

step_parity() {
	printf 'edge: $        PARITY_JOBS=%s sh test/parity/parity.sh %s\n' "$jobs" "$tree"
	$runner env PARITY_JOBS="$jobs" sh test/parity/parity.sh "$tree"
}

step source step_source || stop source $?
step tree step_tree || stop tree $?
step rocq step_rocq || stop rocq $?
step cil step_cil || stop cil $?
step dist step_dist || stop dist $?
step digest step_digest || stop digest $?
step vendor step_vendor || stop vendor $?
step build step_build || stop build $?
step parity step_parity || stop parity $?

printf 'edge: upstream %s\n' "$commit"
printf 'edge: sha256   %s\n' "$digest"
printf 'edge: exit codes:'
while IFS='	' read -r step_name step_status step_elapsed; do
	printf ' %s=%s' "$step_name" "$step_status"
done <"$steps_log"
printf '\n'
printf 'edge: the edge channel is green at %s\n' "$commit"
