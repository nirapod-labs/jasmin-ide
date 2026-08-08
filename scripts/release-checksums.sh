#!/bin/bash

set -eu -o pipefail

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

main() {
	[ $# -eq 1 ] || usage
	directory=$1
	[ -d "$directory" ] ||
		{
			printf '%s: error: no such directory: %s\n' "${0##*/}" "$directory" >&2
			exit 2
		}

	list=$directory/SHA256SUMS
	staged=$list.staged
	rm -f "$staged"

	files=$(find "$directory" -mindepth 1 -maxdepth 1 -type f \
		! -name SHA256SUMS ! -name 'SHA256SUMS.staged' ! -name '*.sha256' \
		-exec basename {} \; | LC_ALL=C sort)

	if [ -z "$files" ]; then
		printf '%s: error: %s holds no release file, so a checksum list would be empty\n' \
			"${0##*/}" "$directory" >&2
		exit 1
	fi

	subdirs=$(find "$directory" -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)
	if [ -n "$subdirs" ]; then
		printf '%s: error: %s holds a subdirectory (%s), which a flat checksum list cannot name\n' \
			"${0##*/}" "$directory" "$(printf '%s' "$subdirs" | tr '\n' ' ')"
		exit 1
	fi

	count=0
	printf '%s\n' "$files" | while IFS= read -r name; do
		[ -n "$name" ] || continue
		digest=$(sha256_of "$directory/$name") || exit 1
		printf '%s  %s\n' "$digest" "$name"
	done >"$staged"

	count=$(wc -l <"$staged" | tr -d ' ')
	want=$(printf '%s\n' "$files" | grep -c . || true)
	if [ "$count" -ne "$want" ]; then
		rm -f "$staged"
		printf '%s: error: wrote %s of %s lines, so the list would be incomplete\n' \
			"${0##*/}" "$count" "$want" >&2
		exit 1
	fi

	if ! (cd "$directory" && { command -v sha256sum >/dev/null 2>&1 && sha256sum -c "$(basename "$staged")"; } >/dev/null 2>&1 ||
		(cd "$directory" && shasum -a 256 -c "$(basename "$staged")" >/dev/null 2>&1)); then
		rm -f "$staged"
		printf '%s: error: the written list does not verify against %s\n' \
			"${0##*/}" "$directory" >&2
		exit 1
	fi

	mv "$staged" "$list"
	say "wrote $list: $count files"
	printf '%s\n' "$files" | sed 's/^/  /'
}

main "$@"
