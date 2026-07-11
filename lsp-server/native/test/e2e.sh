#!/bin/sh
# End-to-end check of the built server over pipes.
#
# The server is started once and fed one complete session on stdin:
# initialize, initialized, a didOpen with a file that does not parse,
# shutdown and exit. What is checked afterwards is what the protocol cares
# about at the process boundary: the exit is 0, and stdout carries nothing
# but well-formed protocol frames. Every byte of stdout is consumed as one
# Content-Length header block plus exactly that many bytes of body, the
# session produces the two replies it must (initialize and shutdown) and no
# other frame, and the server's own logging goes to stderr, not to stdout.
#
# usage: e2e.sh <path-to-server-binary>
set -u

server=${1:?usage: e2e.sh <path-to-server-binary>}

work=$(mktemp -d "${TMPDIR:-/tmp}/jasmin-lsp-e2e.XXXXXX") || {
  printf 'cannot create a temporary directory\n' >&2
  exit 2
}
trap 'rm -rf "$work"' EXIT

failures=0
fail() {
  failures=$((failures + 1))
  printf 'FAIL %s\n' "$1"
}

cr=$(printf '\r')

# One frame: the length of the body in bytes, then the body.
frame() {
  body=$1
  bytes=$(printf '%s' "$body" | wc -c | tr -d ' ')
  printf 'Content-Length: %s\r\n\r\n%s' "$bytes" "$body"
}

{
  frame '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}'
  frame '{"jsonrpc":"2.0","method":"initialized","params":{}}'
  frame '{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/broken.jazz","languageId":"jasmin-crypto","version":1,"text":"fn broken( -> { return }"}}}'
  frame '{"jsonrpc":"2.0","id":2,"method":"shutdown"}'
  frame '{"jsonrpc":"2.0","method":"exit"}'
} >"$work/in"

"$server" <"$work/in" >"$work/out" 2>"$work/err" &
server_pid=$!

# A server that never returns would hang the build. A background loop kills
# it after half a minute, so the missing exit status reports the hang as a
# failure; the loop ends by itself once the server is gone, which keeps the
# shell from reporting a killed job in the middle of the results.
(
  seconds=0
  while [ "$seconds" -lt 30 ]; do
    kill -0 "$server_pid" 2>/dev/null || break
    sleep 1
    seconds=$((seconds + 1))
  done
  kill -9 "$server_pid" 2>/dev/null
) >/dev/null 2>&1 &

wait "$server_pid"
status=$?

[ "$status" -eq 0 ] || {
  fail "the server exited with status $status"
  sed 's/^/  stderr: /' "$work/err" >&2
}

# Walk stdout frame by frame. The header block is every line up to the first
# empty one; the body is the byte count the Content-Length line names.
size=$(wc -c <"$work/out" | tr -d ' ')
offset=0
frames=0

while [ "$offset" -lt "$size" ]; do
  length=
  header_done=0
  while [ "$header_done" -eq 0 ]; do
    line=$(tail -c "+$((offset + 1))" "$work/out" | head -n 1)
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
      fail "frame $((frames + 1)): no usable Content-Length in the header block"
      break
      ;;
  esac
  body=$(dd if="$work/out" bs=1 skip="$offset" count="$length" 2>/dev/null)
  body_bytes=$(printf '%s' "$body" | wc -c | tr -d ' ')
  if [ "$body_bytes" -eq "$length" ]; then :; else
    fail "frame $((frames + 1)): the body is $body_bytes bytes, the header says $length"
    break
  fi
  offset=$((offset + length))
  frames=$((frames + 1))
  case $body in
    "{"*"}") ;;
    *) fail "frame $frames: the body is not a JSON object" ;;
  esac
  case $frames in
    1)
      case $body in
        *'"id":1'*) ;;
        *) fail "frame 1: stdout carries no reply to the initialize request" ;;
      esac
      case $body in
        *'"result"'*) ;;
        *) fail "frame 1: the initialize reply carries no result" ;;
      esac
      ;;
    2)
      case $body in
        *'"id":2'*) ;;
        *) fail "frame 2: stdout carries no reply to the shutdown request" ;;
      esac
      ;;
    *) fail "frame $frames: unexpected extra frame on stdout" ;;
  esac
done

if [ "$offset" -lt "$size" ]; then
  fail "stdout: $((size - offset)) trailing byte(s) after the last frame"
fi

[ "$frames" -eq 2 ] || fail "stdout: $frames frame(s), expected 2"

grep -q 'initialized' "$work/err" ||
  fail "stderr: the server never logged that it initialized"

if [ "$failures" -eq 0 ]; then
  printf 'end-to-end server test passed\n'
else
  printf '%d end-to-end check(s) failed\n' "$failures"
  exit 1
fi
