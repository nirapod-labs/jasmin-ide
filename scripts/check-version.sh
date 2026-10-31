#!/bin/sh
# Check that the four files stating the version state one version:
# controller/version.ml, editor/code/package.json, editor/zed/extension.toml
# and editor/zed/Cargo.toml.
#
#   scripts/check-version.sh [<version>]
#
# The server reports its version to every client and the VS Code extension
# warns when its own differs, so files that drift apart show every user a
# warning. With <version> the four must also equal it. Exit status: 0 they
# agree, 1 they disagree or the version is malformed, 2 a file or its one
# version line cannot be read.

set -u

FILES='controller/version.ml editor/code/package.json editor/zed/extension.toml editor/zed/Cargo.toml'

usage() {
	printf 'usage: %s [<version>]\n' "${0##*/}" >&2
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

version_of() {
	case $1 in
	controller/version.ml)
		expr='s/^let version = "\([^"]*\)"$/\1/p'
		;;
	editor/code/package.json)
		expr='s/^  "version": "\([^"]*\)",$/\1/p'
		;;
	editor/zed/extension.toml | editor/zed/Cargo.toml)
		expr='s/^version = "\([^"]*\)"$/\1/p'
		;;
	*)
		warn "not one of the four version files: $1"
		return 2
		;;
	esac

	if [ ! -f "$1" ]; then
		warn "no such file: $1"
		return 2
	fi

	count=$(sed -n "$expr" "$1" | wc -l | tr -d ' ')
	case $count in
	1) ;;
	0)
		warn "$1 states no version: no line of it has the shape this check reads"
		return 2
		;;
	*)
		warn "$1 states more than one version: $count lines of it have that shape"
		return 2
		;;
	esac

	sed -n "$expr" "$1"
}

main() {
	[ "$#" -le 1 ] || usage

	cd "$(dirname "$0")/.." || exit 2

	expected=${1:-}

	reference=$expected
	if [ -z "$reference" ]; then
		reference=$(version_of controller/version.ml) || exit 2
	fi

	bad=0
	for file in $FILES; do
		version=$(version_of "$file") || exit 2
		[ "$version" = "$reference" ] || bad=$((bad + 1))
	done

	status=0
	if [ "$bad" -ne 0 ]; then
		if [ -n "$expected" ]; then
			say "fail: the four files are not $expected"
		else
			say "fail: the four files do not agree"
		fi
		status=1
	elif ! well_formed "$reference"; then
		say "fail: the four files agree on '$reference', which is not three dot-separated numbers"
		status=1
	fi

	if [ "$status" -eq 0 ]; then
		say "$reference: $FILES"
		exit 0
	fi

	for file in $FILES; do
		version=$(version_of "$file") || exit 2
		if [ "$version" = "$reference" ]; then
			printf '  %s: %s\n' "$file" "$version"
		else
			printf '  %s: %s (differs)\n' "$file" "$version"
		fi
	done
	say "set the four together with scripts/set-version.sh <version>"
	exit 1
}

main "$@"
