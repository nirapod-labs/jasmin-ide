#!/bin/bash

set -eu
if (set -o pipefail) 2>/dev/null; then set -o pipefail; fi

usage() {
	printf 'usage: %s <directory>\n' "${0##*/}" >&2
	exit 2
}

say() { printf '%s: %s\n' "${0##*/}" "$*"; }

sha256_of() {
	if command -v sha256sum >/dev/null 2>&1; then
		sha256sum "$1" | cut -d' ' -f1
	elif command -v shasum >/dev/null 2>&1; then
		shasum -a 256 "$1" | cut -d' ' -f1
	else
		printf '%s: error: neither sha256sum nor shasum is installed\n' \
			"${0##*/}" >&2
		exit 2
	fi
}

is_hex64() {
	case $1 in
	*[!0-9a-f]* | '') return 1 ;;
	esac
	[ "${#1}" -eq 64 ]
}

is_plain_name() {
	case $1 in
	'' | -*) return 1 ;;
	*/*) return 1 ;;
	..) return 1 ;;
	esac
	return 0
}

main() {
	[ $# -eq 1 ] || usage
	directory=$1
	[ -d "$directory" ] || usage
	list=$directory/SHA256SUMS
	[ -f "$list" ] || {
		printf '%s: error: no such list: %s\n' "${0##*/}" "$list" >&2
		exit 2
	}

	checked=0
	failed=0
	names=
	while IFS= read -r line || [ -n "$line" ]; do
		case $line in
		'') continue ;;
		esac
		digest=${line%% *}
		rest=${line#"$digest"}
		case $rest in
		' '*) ;;
		*)
			printf 'FAILED %s: the digest is not followed by a space\n' "$line"
			failed=$((failed + 1))
			continue
			;;
		esac
		rest=${rest# }
		marker=${rest%"${rest#?}"}
		name=${rest#?}
		if ! is_hex64 "$digest"; then
			printf 'FAILED %s: the digest is not 64 lowercase hex digits\n' "$name"
			failed=$((failed + 1))
			continue
		fi
		case $marker in
		' ' | '*') ;;
		*)
			printf 'FAILED %s: the mode character is neither a space nor an asterisk\n' "$name"
			failed=$((failed + 1))
			continue
			;;
		esac
		if ! is_plain_name "$name"; then
			printf 'FAILED %s: not a plain file name\n' "$name"
			failed=$((failed + 1))
			continue
		fi
		case $names in
		*"
$name
"*)
			printf 'FAILED %s: named twice\n' "$name"
			failed=$((failed + 1))
			continue
			;;
		esac
		names="$names
$name
"
		if [ ! -f "$directory/$name" ]; then
			printf 'FAILED %s: the list names a file the directory does not hold\n' "$name"
			failed=$((failed + 1))
			continue
		fi
		actual=$(sha256_of "$directory/$name") || exit 1
		if [ "$actual" = "$digest" ]; then
			printf 'ok %s\n' "$name"
			checked=$((checked + 1))
		else
			printf 'FAILED %s: wanted %s, the file is %s\n' "$name" "$digest" "$actual"
			failed=$((failed + 1))
		fi
	done <"$list"

	unlisted=0
	files=$(find "$directory" -mindepth 1 -maxdepth 1 -type f \
		! -name SHA256SUMS ! -name '*.sha256' -exec basename {} \; | LC_ALL=C sort)
	if [ -n "$files" ]; then
		while IFS= read -r name; do
			[ -n "$name" ] || continue
			case $names in
			*"
$name
"*) ;;
			*)
				printf 'FAILED %s: in the directory and not in the list\n' "$name"
				unlisted=$((unlisted + 1))
				;;
			esac
		done <<EOF
$files
EOF
	fi

	if [ "$failed" -gt 0 ] || [ "$unlisted" -gt 0 ]; then
		say "$((failed + unlisted)) problems: $failed from the list, $unlisted file(s) the list omits" >&2
		exit 1
	fi
	say "verified $checked files"
	return 0
}

main "$@"
