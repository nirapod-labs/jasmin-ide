#!/bin/sh
# The two pieces every script in this directory needs: writing one protocol
# frame, and taking a server's stdout apart again.
#
# usage: . "$(dirname "$0")/frames.sh"

# frame <body>: write one frame to stdout, its body's byte count then the body.
frame() {
	body=$1
	bytes=$(printf '%s' "$body" | wc -c | tr -d ' ')
	printf 'Content-Length: %s\r\n\r\n%s' "$bytes" "$body"
}

# split_frames <stdout-file> <directory>: write each frame's body to
# <directory>/frame.<n>, one file per frame, and print the number of frames.
# A stream that does not decode is reported, and the count then fails the
# caller's own check instead of being silently short.
split_frames() {
	source=$1
	target=$2
	mkdir -p "$target" || return 1
	size=$(wc -c <"$source" | tr -d ' ')
	cr=$(printf '\r')
	offset=0
	frames=0
	while [ "$offset" -lt "$size" ]; do
		length=
		header_done=0
		while [ "$header_done" -eq 0 ]; do
			line=$(tail -c "+$((offset + 1))" "$source" | head -n 1)
			line_bytes=$(printf '%s' "$line" | wc -c | tr -d ' ')
			offset=$((offset + line_bytes + 1))
			text=${line%"$cr"}
			if [ -z "$text" ]; then
				header_done=1
			else
				case $text in
				"Content-Length: "*) length=${text#Content-Length: } ;;
				esac
			fi
		done
		case ${length:-} in
		'' | *[!0-9]*)
			printf 'split_frames: frame %s has no usable Content-Length\n' \
				"$((frames + 1))" >&2
			return 1
			;;
		esac
		frames=$((frames + 1))
		dd if="$source" bs=1 skip="$offset" count="$length" 2>/dev/null \
			>"$target/frame.$frames"
		written=$(wc -c <"$target/frame.$frames" | tr -d ' ')
		if [ "$written" -ne "$length" ]; then
			printf 'split_frames: frame %s holds %s bytes, its header says %s\n' \
				"$frames" "$written" "$length" >&2
			return 1
		fi
		offset=$((offset + length))
	done
	printf '%s\n' "$frames"
}
