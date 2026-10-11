#!/bin/bash

set -eu -o pipefail

usage() {
	printf 'usage: %s <directory>\n' "${0##*/}" >&2
	exit 2
}

say() { printf '%s: %s\n' "${0##*/}" "$*"; }

refuse() {
	printf '%s: error: %s\n' "${0##*/}" "$*" >&2
	exit 1
}

have_sha256sum() { command -v sha256sum >/dev/null 2>&1; }

sha256_of() {
	if have_sha256sum; then
		sha256sum "$1" | cut -d' ' -f1
	elif command -v shasum >/dev/null 2>&1; then
		shasum -a 256 "$1" | cut -d' ' -f1
	else
		printf '%s: error: neither sha256sum nor shasum is installed\n' \
			"${0##*/}" >&2
		exit 2
	fi
}

verify_in() {
	if have_sha256sum &&
		(cd "$1" && sha256sum -c "$2") >/dev/null 2>&1; then
		return 0
	fi
	command -v shasum >/dev/null 2>&1 &&
		(cd "$1" && shasum -a 256 -c "$2") >/dev/null 2>&1
}

main() {
	[ $# -eq 1 ] || usage
	directory=$1
	[ -d "$directory" ] ||
		{
			printf '%s: error: no such directory: %s\n' "${0##*/}" "$directory" >&2
			exit 2
		}

	subdirs=$(find "$directory" -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)
	if [ -n "$subdirs" ]; then
		refuse "$directory holds a subdirectory ($(printf '%s' "$subdirs" | tr '\n' ' ')), which a flat sidecar cannot name"
	fi

	files=$(find "$directory" -mindepth 1 -maxdepth 1 -type f \
		! -name SHA256SUMS ! -name '*.sha256' ! -name '*.staged' \
		-exec basename {} \; | LC_ALL=C sort)

	if [ -z "$files" ]; then
		refuse "$directory holds no release file, so there is no sidecar to write"
	fi

	printf '%s\n' "$files" | while IFS= read -r name; do
		[ -n "$name" ] || continue
		sidecar=$name.sha256
		digest=$(sha256_of "$directory/$name") || exit 1
		printf '%s  %s\n' "$digest" "$name" >"$directory/$sidecar.staged"
		if ! verify_in "$directory" "$sidecar.staged"; then
			rm -f "$directory/$sidecar.staged"
			printf '%s: error: the sidecar for %s does not verify, so it was not written\n' \
				"${0##*/}" "$name" >&2
			exit 1
		fi
		mv "$directory/$sidecar.staged" "$directory/$sidecar"
	done
	written=0
	if counted=$(printf '%s\n' "$files" | grep -c .); then
		written=$counted
	fi

	say "wrote $written sidecars in $directory"
	printf '%s\n' "$files" | sed 's/^/  /'
}

main "$@"
