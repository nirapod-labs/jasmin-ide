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

if [ -z "$label" ]; then
	label=${vsix##*/}
	label=${label%.vsix}
fi

note "package $vsix"
note "label $label"
note "host $platform $(uname -m), $(uname -r)"
note "date $(date -u +%FT%TZ)"
note "work $work"

note "size $(wc -c <"$vsix" | tr -d ' ') bytes"
note "sha256 $(digest_of "$vsix")"

install_package=$vsix
if [ "$quarantine" -eq 1 ]; then
	install_package="$work/quarantined-${vsix##*/}"
	cp "$vsix" "$install_package"
	if [ "$platform" = Darwin ]; then
		xattr -w com.apple.quarantine "0081;$(date +%s);Safari;" "$install_package" ||
			fatal "could not write the download attribute on $install_package"
		note "quarantine written onto $(basename "$install_package") (a simulated download; the package was built on this machine)"
	else
		note "quarantine not applicable on $platform (the platform has no download attribute)"
	fi
fi

if [ "$platform" = Darwin ]; then
	download_attribute=$(xattr -p com.apple.quarantine "$install_package" 2>/dev/null)
	note "xattr package $(basename "$install_package") $(xattr_of "$install_package" | sed 's/^$/none/')"
	if [ -n "$download_attribute" ]; then
		note "quarantine package yes ($download_attribute)"
	else
		note "quarantine package no (the package was built on this machine, not downloaded)"
	fi
else
	note "quarantine package not applicable on $platform (the platform has no download attribute)"
fi

command -v unzip >/dev/null 2>&1 || skipped "no unzip on PATH"
unpacked="$work/unpacked"
mkdir -p "$unpacked" || fatal "could not make $unpacked"
unzip -q "$install_package" -d "$unpacked" || fatal "unzip could not read $install_package"

[ -f "$unpacked/extension/package.json" ] ||
	fatal "$install_package holds no extension/package.json; it is not an editor package"

note "unpacked $(find "$unpacked" -type f | wc -l | tr -d ' ') file(s) into $unpacked"
find "$unpacked" -type f | LC_ALL=C sort | while IFS= read -r file; do
	note "mode unpacked $(mode_of "$file")"
done
if [ "$platform" = Darwin ]; then
	find "$unpacked" -type f | LC_ALL=C sort | while IFS= read -r file; do
		note "xattr unpacked $(basename "$file") $(xattr_of "$file" | sed 's/^$/none/')"
	done
fi

bundled="$unpacked/extension/server/jasmin-lsp"
if [ -f "$bundled" ]; then
	carries_server=1
	note "server bundled extension/server/jasmin-lsp $(mode_bits "$bundled"), $(digest_of "$bundled")"
	if ! is_executable "$bundled"; then
		fatal "the bundled server is not executable in the unpacked package ($(mode_bits "$bundled"))"
	fi
elif [ "$require_server" -eq 1 ]; then
	fatal "$install_package carries no extension/server/jasmin-lsp, and --require-server asks for one"
else
	carries_server=0
	note "server none in this package: the extension finds jasmin-lsp on PATH"
fi

extensions="$work/extensions"
user_data="$work/user-data"
mkdir -p "$extensions" "$user_data" || fatal "could not make the editor directories"

if ! command -v "$code_cli" >/dev/null 2>&1; then
	skipped "no editor command line: $code_cli is not on PATH"
fi

editor_version=$("$code_cli" --version 2>/dev/null | tr '\n' ' ')
[ -n "$editor_version" ] && note "editor $code_cli $editor_version"

note "install $code_cli --install-extension $(basename "$install_package") --force"
install_output=$("$code_cli" --install-extension "$install_package" --force \
	--extensions-dir "$extensions" --user-data-dir "$user_data" 2>&1)
install_status=$?
[ -n "$install_output" ] && printf '%s\n' "$install_output" | sed 's/^/  | /'
if [ "$install_status" -ne 0 ]; then
	note "install exit $install_status"
	fatal "the editor's command line could not install the package (exit $install_status)"
fi
note "install exit $install_status"

installed=$(find "$extensions" -mindepth 1 -maxdepth 1 -type d | LC_ALL=C sort)
count=$(printf '%s\n' "$installed" | grep -c . || true)
[ "$count" -eq 1 ] ||
	fatal "the install left $count directories in $extensions, not the one package it was given"
note "installed $installed"

find "$installed" -type f | LC_ALL=C sort | while IFS= read -r file; do
	note "mode installed $(mode_of "$file")"
done
if [ "$platform" = Darwin ]; then
	find "$installed" -type f | LC_ALL=C sort | while IFS= read -r file; do
		note "xattr installed $(basename "$file") $(xattr_of "$file" | sed 's/^$/none/')"
	done
fi

(cd "$unpacked/extension" && find . -type f | LC_ALL=C sort) >"$work/package-files.txt"
changed=0
checked=0
byte_checked=0
manifest_checked=0
while IFS= read -r file; do
	checked=$((checked + 1))
	relative=${file#./}
	source_file="$unpacked/extension/$relative"
	installed_file="$installed/$relative"
	if [ ! -f "$installed_file" ]; then
		note "missing installed $relative"
		changed=$((changed + 1))
		continue
	fi
	if [ "$relative" = package.json ]; then
		if command -v node >/dev/null 2>&1; then
			if node "$here/clean-install/manifest.mjs" "$source_file" "$installed_file"; then
				manifest_checked=1
			else
				changed=$((changed + 1))
			fi
		else
			note "manifest installed package.json not compared (no node on PATH)"
		fi
		continue
	fi
	byte_checked=$((byte_checked + 1))
	if [ "$(digest_of "$source_file")" != "$(digest_of "$installed_file")" ]; then
		note "changed installed $relative"
		changed=$((changed + 1))
	fi
done <"$work/package-files.txt"
[ "$changed" -eq 0 ] ||
	fatal "the installed tree differs from the package in $changed file(s)"
note "bytes $byte_checked file(s) of $checked byte-identical after the install"
[ "$manifest_checked" -eq 1 ] && note "manifest package.json installed with the same fields as the package"

extra=$(cd "$installed" && find . -type f | LC_ALL=C sort | while IFS= read -r file; do
	grep -qxF "$file" "$work/package-files.txt" || printf '%s\n' "${file#./}"
done)
if [ -n "$extra" ]; then
	note "extra installed $(printf '%s' "$extra" | tr '\n' ' ')"
fi

if [ "$carries_server" -eq 1 ]; then
	installed_server="$installed/server/jasmin-lsp"
	[ -f "$installed_server" ] || fatal "the install dropped server/jasmin-lsp"
	bits=$(mode_bits "$installed_server")
	note "exec-bit server/jasmin-lsp $bits $(if is_executable "$installed_server"; then echo executable; else echo 'not executable'; fi)"
	if [ "$platform" = Darwin ]; then
		note "xattr installed server/jasmin-lsp $(xattr_of "$installed_server" | sed 's/^$/none/')"
	fi
	is_executable "$installed_server" ||
		fatal "the installed server is not executable (mode $bits): the editor cannot start it"
	note "PASS exec-bit server/jasmin-lsp is executable (mode $bits)"
fi

if [ "$window" -eq 0 ]; then
	note "skip window (--no-window)"
	done_message="installed and checked, the editor was not started"
else
	checker="$here/clean-install/check-vscode.mjs"
	[ -f "$checker" ] || fatal "no window check at $checker"
	command -v node >/dev/null 2>&1 || skipped "no node on PATH: the window check cannot run"

	note "window node $checker --extension $installed"
	window_status=0
	if [ -n "$vscode_version" ]; then
		node "$checker" --extension "$installed" --work "$work/window" \
			--extensions-dir "$extensions" --user-data-dir "$user_data" \
			--version "$vscode_version" || window_status=$?
	else
		node "$checker" --extension "$installed" --work "$work/window" \
			--extensions-dir "$extensions" --user-data-dir "$user_data" || window_status=$?
	fi
	case $window_status in
	0) ;;
	3) skipped "the window check cannot run on this host, and said why above" ;;
	*) fatal "the window check failed (exit $window_status)" ;;
	esac

	note "PASS diagnostics and definition from the installed package"
	done_message="installed from $(basename "$vsix") and checked"
fi

note "the halves this run did not check:"
status_of_halves
note "DONE $label: $done_message"
