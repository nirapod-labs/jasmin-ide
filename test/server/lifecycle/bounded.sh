#!/bin/sh
# Run the server for one session with a wall-clock bound.
#
# usage: bounded.sh <seconds> <command> [args...]
#
# Standard input, output and error pass through, and the command's exit
# status is this script's. A command still running after <seconds> is killed,
# so a server that stops reading fails the test instead of stalling the
# suite.

set -u

if [ "$#" -lt 2 ]; then
  printf 'usage: bounded.sh <seconds> <command> [args...]\n' >&2
  exit 2
fi

limit=$1
shift

# An asynchronous command is given /dev/null as its standard input, so the
# session is kept on another descriptor and handed over explicitly.
exec 3<&0

"$@" <&3 &
pid=$!

# The watchdog exits normally when it is stopped: a job that dies from a
# signal makes bash, which is /bin/sh on macOS, print "Terminated" on this
# script's standard error at a moment that depends on timing.
(
  trap 'exit 0' TERM
  seconds=0
  while [ "$seconds" -lt "$limit" ]; do
    kill -0 "$pid" 2>/dev/null || break
    sleep 1
    seconds=$((seconds + 1))
  done
  kill -9 "$pid" 2>/dev/null
) >/dev/null 2>&1 &
watchdog=$!

wait "$pid"
status=$?
kill "$watchdog" 2>/dev/null
wait "$watchdog" 2>/dev/null
exit "$status"
