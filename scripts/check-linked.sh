#!/bin/bash
# Check that a release binary links nothing outside its platform's system
# libraries, and print its size.
#
#   scripts/check-linked.sh <binary> [macos|linux]
#
#
#
#

set -eu -o pipefail

usage() {
	printf 'usage: %s <binary> [macos|linux]\n' "${0##*/}" >&2
	exit 2
}

say() { printf '%s: %s\n' "${0##*/}" "$*"; }
die() {
	printf '%s: error: %s\n' "${0##*/}" "$*" >&2
	exit 2
}

magic_of() {
	od -An -tx1 -N4 "$1" | tr -d ' \n'
}

platform_of_host() {
	case $(uname -s) in
	Darwin) printf 'macos\n' ;;
	Linux) printf 'linux\n' ;;
	*) die "this host is $(uname -s), which has neither otool nor ldd" ;;
	esac
}

deps_of_macos() {
	listing=$("$1" -L "$2") || return 1
	printf '%s\n' "$listing" | tail -n +2 |
		awk '{
			if (substr($0, length($0), 1) == ":") next
			print $1
		}'
}

deps_of_linux() {
	listing=$("$1" "$2" 2>&1) || true
	case $listing in
	*"not a dynamic executable"* | *"statically linked"*)
		printf 'static\n'
		return 0
		;;
	esac
	printf '%s\n' "$listing" | awk '{
		if (match($0, /\(0x[0-9a-fA-F]+\)/)) {
			$0 = substr($0, 1, RSTART - 1)
		}
		if ($0 == "") next
		if (index($0, "=>") > 0) {
			path = $0
			sub(/^[^>]*=>[ \t]*/, "", path)
		} else {
			path = $1
		}
		n = split(path, parts, "/")
		print parts[n]
	}'
}

allowed_macos() {
	[ "$1" = "/usr/lib/libSystem.B.dylib" ]
}

allowed_linux() {
	case $1 in
	libc.so.6 | libc.musl-*.so.1 | ld-musl-*.so.1) return 0 ;;
	linux-vdso.so.1 | linux-gate.so.1) return 0 ;;
	esac
	return 1
}

is_glibc_loader() {
	case $1 in
	ld-linux*.so.*) return 0 ;;
	esac
	return 1
}

size_of() {
	bytes=$(wc -c <"$1" | tr -d ' ')
	mib=$(awk -v b="$bytes" 'BEGIN { printf "%.1f", b / 1048576 }')
	say "size $bytes bytes (${mib} MiB)"
}

main() {
	[ $# -ge 1 ] && [ $# -le 2 ] || usage
	binary=$1
	platform=${2:-$(platform_of_host)}

	case $platform in
	macos | linux) ;;
	*) die "unknown platform: $platform (macos or linux)" ;;
	esac

	[ -f "$binary" ] || die "no such file: $binary"
	case $binary in
	-*) die "a path that starts with a dash is refused: $binary" ;;
	esac

	say "$binary"
	size_of "$binary"

	magic=$(magic_of "$binary")
	case $platform:$magic in
	macos:feedface | macos:cefaedfe | macos:feedfacf | macos:cffaedfe | \
		macos:cafebabe | macos:bebafeca | macos:cafebabf | macos:bfbafeca) ;;
	linux:7f454c46) ;;
	*)
		die "not a $platform executable: the first four bytes are $magic"
		;;
		esac

	case $platform in
	macos)
		command -v otool >/dev/null 2>&1 ||
			die "no otool on PATH, so the dependencies of $binary cannot be read"
		deps=$(deps_of_macos otool "$binary") ||
			die "otool -L failed on $binary"
		;;
	linux)
		command -v ldd >/dev/null 2>&1 ||
			die "no ldd on PATH, so the dependencies of $binary cannot be read"
		deps=$(deps_of_linux ldd "$binary") ||
			die "ldd failed on $binary"
		;;
	esac

	if [ "$deps" = static ]; then
		say "static: the binary names no dynamic dependency"
		say "ok"
		return 0
	fi

	if [ -z "$deps" ]; then
		say "the listing is empty, which is not a static binary and not a linked one"
		say "fail: nothing was measured"
		return 1
	fi

	bad=0
	seen=0
	for dep in $deps; do
		seen=$((seen + 1))
		allowed=0
		if [ "$platform" = macos ]; then
			allowed_macos "$dep" && allowed=1
		else
			allowed_linux "$dep" && allowed=1
		fi
		if [ "$allowed" -eq 1 ]; then
			say "  allowed: $dep"
		else
			say "  not allowed: $dep"
			bad=$((bad + 1))
			if [ "$platform" = linux ] && is_glibc_loader "$dep"; then
				say "  this is a glibc dynamic build; the release binary is static against musl"
			fi
		fi
	done

	if [ "$bad" -ne 0 ]; then
		say "fail: $bad of $seen dependencies are not allowed on $platform"
		return 1
	fi

	say "ok: all $seen dependencies are allowed on $platform"
	return 0
}

main "$@"
