#!/bin/sh
# Write one protocol frame per line of a session file: the body's byte count,
# a blank line, then the body exactly as the line holds it.
#
# A line is framed as it is, JSON or not, so a line that is not JSON is how a
# test sends a body the reader has to refuse.
#
# usage: frames.sh <session-file>

while IFS= read -r body || [ -n "$body" ]; do
  printf 'Content-Length: %s\r\n\r\n%s' \
    "$(printf '%s' "$body" | wc -c | tr -d ' ')" "$body"
done <"$1"
