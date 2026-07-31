#!/bin/sh
# End-to-end check of the built server over pipes.
#
# The server is started once and fed one complete session on stdin:
# initialize, initialized, a didOpen with a file that does not parse, shutdown
# and exit. What is checked afterwards is what the protocol cares about at the
# process boundary: the exit is 0, stdout carries nothing but well-formed
# protocol frames and no extra byte, and the server's own logging goes to
# stderr, not to stdout.
#
# The session waits after the didOpen because the server analyses a document
# a debounce delay after its last change: the loop waits for input no longer
# than that delay, and the wake-up at the end of the wait is what analyses the
# document. Three frames follow: the initialize reply, the publication, and
# the shutdown reply, in that order. The diagnostics themselves are checked in
# diagnostics.sh.
#
# usage: e2e.sh <path-to-server-binary>
set -u

. "$(dirname "$0")/frames.sh"

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

session() {
	frame '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}'
	frame '{"jsonrpc":"2.0","method":"initialized","params":{}}'
	frame '{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/broken.jazz","languageId":"jasmin-crypto","version":1,"text":"fn broken( -> { return }"}}}'
	sleep 1
	frame '{"jsonrpc":"2.0","id":2,"method":"shutdown"}'
	frame '{"jsonrpc":"2.0","method":"exit"}'
}

# The session writes into a pipe of its own, so its sleep paces what the
# server reads: a session written to a file first would arrive all at once,
# and a file the session is still appending to would end in end of input
# before the last frame was written.
mkfifo "$work/in" || {
	printf 'cannot make a pipe for the session\n' >&2
	exit 2
}
session >"$work/in" &
session_pid=$!

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

# The session should have finished when the server did; a writer left behind
# by a server that died early is killed here rather than at the end of the
# script, where the shell would report it.
kill "$session_pid" 2>/dev/null
wait "$session_pid" 2>/dev/null

if [ "$status" -ne 0 ]; then
	fail "the server exited with status $status"
	sed 's/^/  stderr: /' "$work/err" >&2
fi

# Every byte of stdout must belong to a frame: split_frames reports a header
# block it cannot read, and the count below reports a missing frame.
if split_frames "$work/out" "$work" >"$work/count"; then
	frames=$(cat "$work/count")
else
	fail "stdout does not split into frames"
	frames=0
fi

if [ "$frames" -eq 3 ]; then :; else
	fail "stdout: $frames frame(s), expected 3"
fi

body() { cat "$work/frame.$1"; }

case $(body 1) in
*'"id":1'*) ;;
*) fail "frame 1: stdout carries no reply to the initialize request" ;;
esac
case $(body 1) in
*'"result"'*) ;;
*) fail "frame 1: the initialize reply carries no result" ;;
esac
case $(body 2) in
*'"method":"textDocument/publishDiagnostics"'*) ;;
*) fail "frame 2: the publication does not name its method" ;;
esac
case $(body 3) in
*'"id":2'*) ;;
*) fail "frame 3: stdout carries no reply to the shutdown request" ;;
esac

n=1
while [ "$n" -le "$frames" ]; do
	case $(body "$n") in
	"{"*"}") ;;
	*) fail "frame $n: the body is not a JSON object" ;;
	esac
	n=$((n + 1))
done

grep -q 'initialized' "$work/err" ||
	fail "stderr: the server never logged that it initialized"

if [ "$failures" -eq 0 ]; then
	printf 'end-to-end server test passed\n'
else
	printf '%d end-to-end check(s) failed\n' "$failures"
	exit 1
fi
