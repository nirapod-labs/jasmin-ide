#!/bin/sh
# Write or verify the checksums of a release directory.
#
#   sh scripts/checksums.sh write <directory>
#   sh scripts/checksums.sh verify <directory>
#
# write puts `<name>.sha256` beside every file and one SHA256SUMS over all of
# them, then verifies what it wrote. verify reads SHA256SUMS back: every line
# well formed, every file it names present and matching, no name twice, and
# no file of the directory left out, since a user cannot check a file the
# list omits. The directory is flat: a list of plain names cannot name a file
# in a subdirectory.
#
# Exit status: 0 when every check holds, 1 when one does not, 2 on a usage
# error or a directory with no list to verify.

set -u
name=checksums.sh

sha256() {
	if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1"; else shasum -a 256 "$1"; fi | cut -d' ' -f1
}

# The release files: everything but the list and the sidecars.
files() {
	find "$1" -mindepth 1 -maxdepth 1 -type f ! -name SHA256SUMS ! -name '*.sha256' -exec basename {} \; | LC_ALL=C sort
}

write() {
	sub=$(find "$1" -mindepth 1 -maxdepth 1 -type d -exec basename {} \; | tr '\n' ' ')
	[ -z "$sub" ] || { echo "$name: $1 holds a subdirectory (${sub% }), which a flat list cannot name" >&2; exit 1; }
	list=$(files "$1")
	[ -n "$list" ] || { echo "$name: $1 holds no release file" >&2; exit 1; }
	for file in $list; do
		line="$(sha256 "$1/$file")  $file"
		echo "$line" >"$1/$file.sha256"
		echo "$line"
	done >"$1/SHA256SUMS"
	verify "$1" >/dev/null || { echo "$name: what was written does not verify" >&2; exit 1; }
	echo "$name: wrote $1/SHA256SUMS and $(echo "$list" | wc -l | tr -d ' ') sidecars"
}

verify() {
	[ -f "$1/SHA256SUMS" ] || { echo "$name: no list: $1/SHA256SUMS" >&2; exit 2; }
	failed=0 ok=0 seen=' '
	while IFS= read -r line || [ -n "$line" ]; do
		[ -n "$line" ] || continue
		digest=${line%% *} file=${line#* }
		case $file in ' '* | '*'*) file=${file#?} ;; *) file= ;; esac
		problem=
		case $digest in *[!0-9a-f]* | '') problem='the digest is not 64 lowercase hex digits' ;; esac
		[ -n "$problem" ] || [ ${#digest} -eq 64 ] || problem='the digest is not 64 lowercase hex digits'
		case $file in '' | -* | */* | ..) problem=${problem:-'not a plain file name'} ;; esac
		case $seen in *" $file "*) problem=${problem:-'named twice'} ;; esac
		# Only a well-formed line names a file as listed.
		[ -n "$problem" ] || seen="$seen$file "
		if [ -z "$problem" ] && [ ! -f "$1/$file" ]; then
			problem='the list names a file the directory does not hold'
		elif [ -z "$problem" ] && [ "$(sha256 "$1/$file")" != "$digest" ]; then
			problem="wanted $digest, the file is $(sha256 "$1/$file")"
		fi
		if [ -n "$problem" ]; then
			echo "FAILED ${file:-$line}: $problem"
			failed=$((failed + 1))
		else
			echo "ok $file"
			ok=$((ok + 1))
		fi
	done <"$1/SHA256SUMS"
	for file in $(files "$1"); do
		case $seen in *" $file "*) ;; *)
			echo "FAILED $file: in the directory and not in the list"
			failed=$((failed + 1))
			;;
		esac
	done
	[ "$failed" -eq 0 ] || { echo "$name: $failed problem(s)" >&2; exit 1; }
	echo "$name: verified $ok files"
}

case ${1:-}:${2:-} in
write:?* | verify:?*) [ -d "$2" ] || { echo "$name: no directory: $2" >&2; exit 2; }; "$1" "$2" ;;
*) echo "usage: $name write|verify <directory>" >&2; exit 2 ;;
esac
