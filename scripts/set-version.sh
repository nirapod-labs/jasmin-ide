#!/bin/sh
#
#   scripts/set-version.sh <version>
#
#
#
#
#
#
#
#
#
#

set -u

FILES='controller/version.ml editor/code/package.json editor/zed/extension.toml editor/zed/Cargo.toml'

LOCK=editor/zed/Cargo.lock
LOCK_PACKAGE=jasmin_ide_zed

usage() {
	printf 'usage: %s <version>\n' "${0##*/}" >&2
	exit 2
}

say() { printf '%s: %s\n' "${0##*/}" "$*"; }
warn() { printf '%s: %s\n' "${0##*/}" "$*" >&2; }

well_formed() {
	case $1 in
	'' | *[!0-9.]*) return 1 ;;
	esac
	old_ifs=$IFS
	IFS=.
	set -- $1
	IFS=$old_ifs
	[ "$#" -eq 3 ] || return 1
	for part in "$@"; do
		case $part in
		'' | *[!0-9]*) return 1 ;;
		0) ;;
		[1-9]*) ;;
		*) return 1 ;;
		esac
	done
	return 0
}

read_expr() {
	case $1 in
	controller/version.ml) printf '%s\n' 's/^let version = "\([^"]*\)"$/\1/p' ;;
	editor/code/package.json) printf '%s\n' 's/^  "version": "\([^"]*\)",$/\1/p' ;;
	editor/zed/extension.toml | editor/zed/Cargo.toml) printf '%s\n' 's/^version = "\([^"]*\)"$/\1/p' ;;
	*)
		warn "not one of the four version files: $1"
		return 2
		;;
	esac
}

write_expr() {
	case $1 in
	controller/version.ml) printf '%s\n' "s/^let version = \"[^\"]*\"\$/let version = \"$2\"/" ;;
	editor/code/package.json) printf '%s\n' "s/^  \"version\": \"[^\"]*\",\$/  \"version\": \"$2\",/" ;;
	editor/zed/extension.toml | editor/zed/Cargo.toml) printf '%s\n' "s/^version = \"[^\"]*\"\$/version = \"$2\"/" ;;
	*)
		warn "not one of the four version files: $1"
		return 2
		;;
	esac
}

version_of() {
	expr=$(read_expr "$1") || return 2

	if [ ! -f "$1" ]; then
		warn "no such file: $1"
		return 2
	fi

	count=$(sed -n "$expr" "$1" | wc -l | tr -d ' ')
	case $count in
	1) ;;
	0)
		warn "$1 states no version: no line of it has the shape this script writes"
		return 2
		;;
	*)
		warn "$1 states more than one version: $count lines of it have that shape"
		return 2
		;;
	esac

	sed -n "$expr" "$1"
}

