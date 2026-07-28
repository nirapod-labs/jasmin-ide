#!/bin/sh
# End-to-end checks of the publish path, over the built server and pipes.
#
# A document that was opened or changed is analysed once no change has arrived
# for the debounce delay: the loop waits for input but no longer than that
# delay, so the wake-up at the end of the wait is what analyses the document.
# The default delay is 250 ms, so a second is several times the wait.
#
# Five sessions, one server each:
#
#   open   a didOpen of a file that does not parse publishes one diagnostic,
#          for the document's version, before the reply to the next request;
#   fix    a didChange that repairs the file publishes an empty list, for the
#          new version;
#   burst  two changes inside the wait publish once, for the last version;
#   close  a didClose publishes an empty list;
#   silent the session stops after the change, so no packet follows to end the
#          wait: the debounce alone has to publish the diagnostic.
#
# usage: diagnostics.sh <path-to-server-binary>
set -u

. "$(dirname "$0")/frames.sh"

server=${1:?usage: diagnostics.sh <path-to-server-binary>}

work=$(mktemp -d "${TMPDIR:-/tmp}/jasmin-lsp-diagnostics.XXXXXX") || {
	printf 'cannot create a temporary directory\n' >&2
	exit 2
}
trap 'rm -rf "$work"' EXIT

failures=0
fail() {
	failures=$((failures + 1))
	printf 'FAIL %s\n' "$1"
}

broken="fn broken( -> { return }"
fixed='fn broken() {
}
'

broken_path=$work/broken.jazz
printf '%s' "$broken" >"$broken_path"
broken_uri=file://$broken_path

initialize_frame='{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}'
initialized_frame='{"jsonrpc":"2.0","method":"initialized","params":{}}'
shutdown_frame='{"jsonrpc":"2.0","id":2,"method":"shutdown"}'
exit_frame='{"jsonrpc":"2.0","method":"exit"}'

open_frame() {
	uri=$1
	text=$2
	version=$3
	printf '{"jsonrpc":"2.0","method":"textDocument/didOpen","params":'
	printf '{"textDocument":{"uri":"%s","languageId":"jasmin-crypto",' "$uri"
	printf '"version":%s,"text":"%s"}}}' "$version" "$text"
}

change_frame() {
	uri=$1
	text=$2
	version=$3
	printf '{"jsonrpc":"2.0","method":"textDocument/didChange","params":'
	printf '{"textDocument":{"uri":"%s","version":%s},' "$uri" "$version"
	printf '"contentChanges":[{"text":"%s"}]}}' "$text"
}

close_frame() {
	printf '{"jsonrpc":"2.0","method":"textDocument/didClose","params":'
	printf '{"textDocument":{"uri":"%s"}}}' "$1"
}

# The four sessions. Each writes frames to stdout, waiting where a wait is the
# point, and the runner feeds it to one server.
session_open() {
	frame "$initialize_frame"
	frame "$initialized_frame"
	frame "$(open_frame "$broken_uri" "$broken" 1)"
	sleep 1
	frame "$shutdown_frame"
	frame "$exit_frame"
}

session_fix() {
	frame "$initialize_frame"
	frame "$initialized_frame"
	frame "$(open_frame "$broken_uri" "$broken" 1)"
	sleep 1
	frame "$(change_frame "$broken_uri" "$fixed" 2)"
	sleep 1
	frame "$shutdown_frame"
	frame "$exit_frame"
}

session_burst() {
	frame "$initialize_frame"
	frame "$initialized_frame"
	frame "$(open_frame "$broken_uri" "$broken" 1)"
	sleep 1
	frame "$(change_frame "$broken_uri" "$broken;" 2)"
	frame "$(change_frame "$broken_uri" "$broken;;" 3)"
	sleep 1
	frame "$shutdown_frame"
	frame "$exit_frame"
}

session_close() {
	frame "$initialize_frame"
	frame "$initialized_frame"
	frame "$(open_frame "$broken_uri" "$broken" 1)"
	sleep 1
	frame "$(close_frame "$broken_uri")"
	sleep 1
	frame "$shutdown_frame"
	frame "$exit_frame"
}

# The session that asks for nothing after the change: the pipe closes and the
# server ends on end of input, which is exit status 1 because no shutdown was
# asked for. Nothing but the debounce can have published the diagnostic.
session_silent() {
	frame "$initialize_frame"
	frame "$initialized_frame"
	frame "$(open_frame "$broken_uri" "$broken" 1)"
	sleep 1
}

# run <name> <session> [<expected-exit-status>]: one server, one session, split
# into frames. The session writes into a pipe of its own, so its sleeps pace
# what the server reads and a pause in the session is a pause in the server's
# input: a session written to a file first would arrive all at once and no
# debounce would ever see the wait it is meant to survive. A server that never
# returns would hang the build, so a background loop kills it after half a
# minute and the missing exit status reports the hang as a failure.
run() {
	name=$1
	want=${3:-0}
	inlet=$work/$name.in
	mkfifo "$inlet" || {
		fail "$name: cannot make a pipe for the session"
		return 1
	}
	"$server" <"$inlet" >"$work/$name.out" 2>"$work/$name.err" &
	pid=$!
	"$2" >"$inlet" 2>/dev/null &
	writer=$!
	(
		seconds=0
		while [ "$seconds" -lt 30 ]; do
			kill -0 "$pid" 2>/dev/null || break
			sleep 1
			seconds=$((seconds + 1))
		done
		kill -9 "$pid" 2>/dev/null
	) >/dev/null 2>&1 &
	wait "$pid"
	status=$?
	kill "$writer" 2>/dev/null
	wait "$writer" 2>/dev/null
	if [ "$status" -ne "$want" ]; then
		fail "$name: the server exited with status $status, not $want"
		sed 's/^/  stderr: /' "$work/$name.err" >&2
	fi
	if split_frames "$work/$name.out" "$work/$name" >"$work/$name.count"; then
		:
	else
		fail "$name: stdout does not split into frames"
		printf '0\n' >"$work/$name.count"
	fi
}

frames_of() { cat "$work/$1.count"; }

frame_body() { cat "$work/$1/frame.$2"; }

# publishes <name>: how many of the frames are diagnostics publications.
publishes() {
	name=$1
	total=$(frames_of "$name")
	n=1
	found=0
	while [ "$n" -le "$total" ]; do
		case $(frame_body "$name" "$n") in
		*"textDocument/publishDiagnostics"*) found=$((found + 1)) ;;
		esac
		n=$((n + 1))
	done
	printf '%s\n' "$found"
}

# publish_body <name> <n>: the body of the nth publication, on one line.
publish_body() {
	name=$1
	wanted=$2
	total=$(frames_of "$name")
	n=1
	seen=0
	while [ "$n" -le "$total" ]; do
		body=$(frame_body "$name" "$n")
		case $body in
		*"textDocument/publishDiagnostics"*)
			seen=$((seen + 1))
			if [ "$seen" -eq "$wanted" ]; then
				printf '%s\n' "$body"
				return 0
			fi
			;;
		esac
		n=$((n + 1))
	done
	return 1
}

# expect <description> <text> <fragment>: the text contains the fragment.
expect() {
	case $2 in
	*"$3"*) return 0 ;;
	*)
		fail "$1: no $3 in: $2"
		return 1
		;;
	esac
}

# reject <description> <text> <fragment>: the text does not contain it.
reject() {
	case $2 in
	*"$3"*) fail "$1: unexpected $3 in: $2" ;;
	esac
}

expect_count() {
	if [ "$2" -eq "$3" ]; then :; else
		fail "$1: expected $3, found $2"
	fi
}

run open session_open
frames=$(frames_of open)
expect_count "open: three frames, the publication between the two replies" \
	"$frames" 3
expect "open: the initialize reply comes first" "$(frame_body open 1)" '"id":1'
expect "open: the shutdown reply comes last" "$(frame_body open 3)" '"id":2'
expect_count "open: one publication" "$(publishes open)" 1
publication=$(publish_body open 1)
expect "open: the file is named" "$publication" "\"uri\":\"$broken_uri\""
expect "open: the document's version" "$publication" '"version":1'
expect "open: one diagnostic" "$publication" '"diagnostics":[{'
expect "open: the compiler's code" "$publication" '"code":"parse"'
expect "open: an error" "$publication" '"severity":1'
expect "open: the message" "$publication" '"message":"unexpected token"'
if grep -q 'initialized' "$work/open.err"; then :; else
	fail "open: stderr never says the server initialized"
fi

run fix session_fix
expect_count "fix: four frames" "$(frames_of fix)" 4
expect_count "fix: two publications" "$(publishes fix)" 2
expect "fix: the first describes version 1" "$(publish_body fix 1)" \
	'"version":1'
expect "fix: the first holds the diagnostic" "$(publish_body fix 1)" \
	'"diagnostics":[{'
expect "fix: the second describes version 2" "$(publish_body fix 2)" \
	'"version":2'
expect "fix: the second is empty" "$(publish_body fix 2)" '"diagnostics":[]'

run burst session_burst
expect_count "burst: four frames" "$(frames_of burst)" 4
expect_count "burst: one publication for the open and one for the two changes" \
	"$(publishes burst)" 2
expect "burst: the first describes version 1" "$(publish_body burst 1)" \
	'"version":1'
expect "burst: the two changes publish the last version" \
	"$(publish_body burst 2)" '"version":3'
expect "burst: the two changes publish the diagnostic of the last text" \
	"$(publish_body burst 2)" '"diagnostics":[{'
n=1
total=$(frames_of burst)
while [ "$n" -le "$total" ]; do
	reject "burst: the middle version is never published" \
		"$(frame_body burst "$n")" '"version":2'
	n=$((n + 1))
done

run close session_close
expect_count "close: four frames" "$(frames_of close)" 4
expect_count "close: the open publishes and the close clears" "$(publishes close)" 2
expect "close: the open holds the diagnostic" "$(publish_body close 1)" \
	'"diagnostics":[{'
expect "close: the clear is empty" "$(publish_body close 2)" '"diagnostics":[]'
expect "close: the clear names the last version" "$(publish_body close 2)" \
	'"version":1'

run silent session_silent 1
expect_count "silent: the initialize reply and the publication" \
	"$(frames_of silent)" 2
expect_count "silent: one publication, with no packet after the change" \
	"$(publishes silent)" 1
publication=$(publish_body silent 1)
expect "silent: the file is named" "$publication" "\"uri\":\"$broken_uri\""
expect "silent: the document's version" "$publication" '"version":1'
expect "silent: one diagnostic" "$publication" '"diagnostics":[{'
expect "silent: the message" "$publication" '"message":"unexpected token"'

if [ "$failures" -eq 0 ]; then
	printf 'end-to-end diagnostics test passed\n'
else
	printf '%d end-to-end diagnostic check(s) failed\n' "$failures"
	exit 1
fi
