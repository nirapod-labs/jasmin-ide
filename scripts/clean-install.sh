#!/bin/sh

set -u

program=${0##*/}
platform=$(uname -s)
here=$(cd "$(dirname "$0")" && pwd)

usage() {
	cat >&2 <<'EOF'
usage: scripts/clean-install.sh --vsix <file> [options]

  --vsix <file>        the package to install (required)
  --label <name>       what to call this run in the record, for example
                       `darwin-arm64`; default: the package's file name
  --code <path>        the editor's command line; default: `code` on PATH
  --work <dir>         work in this directory instead of a fresh one
  --keep               leave the work directory behind when the run ends
  --no-window          stop after the install; record the files only
  --require-server     the package must carry the server, and it must be
                       executable; a package without one fails
  --quarantine         write the download attribute a browser would add onto a
                       copy of the package before installing it, so the record
                       shows what the platform does with a download
  --vscode-version <v> the editor release the window uses; default: the one
                       `scripts/clean-install/check-vscode.mjs` names
  --status             print the state of the four halves and exit; installs
                       nothing and reads no package

Exit status: 0 every check passed; 1 a check failed; 2 a bad argument or a
missing input file; 3 skipped, this host cannot run the check and the reason is
printed as `skip clean-install (<reason>)`; 0 also for `--status`.
EOF
	exit 2
}

fatal() {
	printf 'clean-install: FAIL %s\n' "$1" >&2
	exit 1
}

skipped() {
	printf 'skip clean-install (%s)\n' "$1"
	exit 3
}

vsix=
label=
code_cli=${CLEAN_INSTALL_CODE:-code}
work=
keep=0
window=1
require_server=0
quarantine=0
vscode_version=
status_only=0

record=
note() {
	printf 'clean-install: %s\n' "$1"
	[ -n "$record" ] && printf '%s\n' "$1" >>"$record"
}

status_of_halves() {
	note "UNKNOWN linux-x64: no run and no linux-x64 package; an x86-64 guest on this arm64 host is qemu emulation, not a native VM"
	note "UNKNOWN linux-arm64: no run and no linux-arm64 package; the fresh Ubuntu 24.04.4 aarch64 VM (2 CPU, 2 GiB) has no editor, Node or unzip"
	note "RESERVED darwin-arm64 on a clean Mac: a person's run on a Mac that has never had the extension"
	note "RESERVED zed release asset: the project is private and the build records no release digests, so the extension downloads nothing"
}

while [ $# -gt 0 ]; do
	case $1 in
	--vsix)
		[ $# -ge 2 ] || usage
		vsix=$2
		shift 2
		;;
	--label)
		[ $# -ge 2 ] || usage
		label=$2
		shift 2
		;;
	--code)
		[ $# -ge 2 ] || usage
		code_cli=$2
		shift 2
		;;
	--work)
		[ $# -ge 2 ] || usage
		work=$2
		shift 2
		;;
	--vscode-version)
		[ $# -ge 2 ] || usage
		vscode_version=$2
		shift 2
		;;
	--keep)
		keep=1
		shift
		;;
	--no-window)
		window=0
		shift
		;;
	--require-server)
		require_server=1
		shift
		;;
	--quarantine)
		quarantine=1
		shift
		;;
	--status)
		status_only=1
		shift
		;;
	-h | --help)
		usage
		;;
	*)
		printf '%s: unknown argument %s\n' "$program" "$1" >&2
		usage
		;;
	esac
done

if [ "$status_only" -eq 1 ]; then
	status_of_halves
	exit 0
fi

[ -n "$vsix" ] || usage
[ -f "$vsix" ] || {
	printf '%s: no package at %s\n' "$program" "$vsix" >&2
	exit 2
}

mode_of() {
	if [ "$platform" = Darwin ]; then
		stat -f '%Sp %Lp %N' "$1" 2>/dev/null
	else
		stat -c '%A %a %n' "$1" 2>/dev/null
	fi
}

mode_bits() {
	if [ "$platform" = Darwin ]; then
		stat -f '%Lp' "$1" 2>/dev/null
	else
		stat -c '%a' "$1" 2>/dev/null
	fi
}

is_executable() {
	[ -n "$(find "$1" -perm -u+x 2>/dev/null)" ]
}

digest_of() {
	if command -v sha256sum >/dev/null 2>&1; then
		sha256sum "$1" | cut -d' ' -f1
	else
		shasum -a 256 "$1" | cut -d' ' -f1
	fi
}

xattr_of() {
	xattr -l "$1" 2>/dev/null | tr '\n' ';' | sed 's/;$//'
}

winpath() {
	case $1 in
	/*) printf '%s' "$1" ;;
	*) printf '%s/%s' "$(pwd)" "$1" ;;
	esac
}

vsix=$(winpath "$vsix")
if [ -z "$work" ]; then
	work=$(mktemp -d "${TMPDIR:-/tmp}/jasmin-ide-clean-install.XXXXXX") || exit 2
	made_work=1
else
	mkdir -p "$work" || exit 2
	work=$(cd "$work" && pwd)
	made_work=0
fi
chmod 700 "$work"
record="$work/record.txt"
: >"$record"

cleanup() {
	if [ "$keep" -eq 0 ] && [ "$made_work" -eq 1 ]; then
		rm -rf "$work"
	else
		printf 'clean-install: work directory kept at %s\n' "$work"
	fi
}
trap cleanup EXIT HUP INT TERM

