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

lock_version() {
	awk -v package="$LOCK_PACKAGE" '
		/^\[\[package\]\]$/ { ours = 0; next }
		$0 == "name = \"" package "\"" { ours = 1; next }
		ours && /^version = "/ {
			version = $0
			sub(/^version = "/, "", version)
			sub(/"$/, "", version)
			print version
			exit
		}
	' "$LOCK"
}

write_file() {
	tmp=$(mktemp "${TMPDIR:-/tmp}/jasmin-set-version.XXXXXX") || exit 2
	if ! sed "$2" "$1" >"$tmp"; then
		rm -f "$tmp"
		warn "sed could not rewrite $1"
		exit 2
	fi
	if ! cat "$tmp" >"$1"; then
		rm -f "$tmp"
		warn "could not write $1"
		exit 2
	fi
	rm -f "$tmp"
}

set_lock() {
	old=$(lock_version) || exit 2
	if [ -z "$old" ]; then
		warn "$LOCK holds no $LOCK_PACKAGE package entry"
		exit 2
	fi
	if [ "$old" = "$new" ]; then
		say "already $new: $LOCK"
		return 0
	fi

	tmp=$(mktemp "${TMPDIR:-/tmp}/jasmin-set-version.XXXXXX") || exit 2
	if ! awk -v package="$LOCK_PACKAGE" -v version="$new" '
		/^\[\[package\]\]$/ { ours = 0 }
		$0 == "name = \"" package "\"" { ours = 1 }
		ours && /^version = "/ { print "version = \"" version "\""; ours = 0; next }
		{ print }
	' "$LOCK" >"$tmp"; then
		rm -f "$tmp"
		warn "awk could not rewrite $LOCK"
		exit 2
	fi
	if ! cat "$tmp" >"$LOCK"; then
		rm -f "$tmp"
		warn "could not write $LOCK"
		exit 2
	fi
	rm -f "$tmp"
	say "$old -> $new: $LOCK"
}

main() {
	[ "$#" -eq 1 ] || usage
	new=$1
	well_formed "$new" ||
		{ warn "not a version: '$new' (three dot-separated numbers, as in 0.1.0, with no leading zero)"; exit 2; }

	cd "$(dirname "$0")/.." || exit 2

	for file in $FILES; do
		version_of "$file" >/dev/null || exit 2
	done
	if [ ! -f "$LOCK" ]; then
		warn "no such file: $LOCK"
		exit 2
	fi
	if [ -z "$(lock_version)" ]; then
		warn "$LOCK holds no $LOCK_PACKAGE package entry"
		exit 2
	fi

	for file in $FILES; do
		old=$(version_of "$file") || exit 2
		if [ "$old" = "$new" ]; then
			say "already $new: $file"
			continue
		fi
		write_file "$file" "$(write_expr "$file" "$new")"
		say "$old -> $new: $file"
	done

	set_lock

	sh scripts/check-version.sh "$new"
	status=$?
	if [ "$status" -ne 0 ]; then
		exit "$status"
	fi
}

main "$@"
