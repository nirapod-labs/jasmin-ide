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

