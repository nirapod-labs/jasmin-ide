#!/bin/sh
# Prove the driver refuses while a build holds a slot, and leave no stand-in
# build behind however this run ends.
#
# A build beside a measurement makes every number meaningless, so
# `bench/run.sh` refuses while one runs. Proving that refusal needs a build
# that is really there, and a real build would take minutes and change the
# tree, so this script starts one stand-in build instead: a `sleep` whose
# command line reads `dune ... build`, which is what the driver's process
# probe matches, started through the slot runner, so a real `build.lock.N` is
# held in the runner's directory for as long as the stand-in lives. It then
#
#   1. shows the driver refusing, with that slot held;
#   2. runs the test step in the slot the stand-in holds — the stand-in is a
#      sleep, so it takes the slot and nothing else, not any CPU — which is
#      what a test that read the host's own lock directory would fail on;
#   3. stops the stand-in and proves the slot is free again.
#
# The stand-in must not outlive this script. One left behind holds its slot,
# and every build on the host waits behind it until a person notices, so
# every end of this script — the normal one, a failed command, and a signal —
# goes through the same cleanup: it ends the command, ends the stand-in, waits
# for both, and checks the lock is gone. A run that cannot show the slot free
# exits non-zero and says what is still holding it.
#
# usage: HEAVY=<slot runner> JIDE_LOCKS=<its lock directory> \
#          sh test/bench/held-run.sh [command...]
#
#   HEAVY=/path/to/run-heavy.sh JIDE_LOCKS=/tmp/our-locks \
#     sh test/bench/held-run.sh
#
# HEAVY is a command, the variable the Makefile passes to its heavy steps, and
# it is required: without a runner nothing holds a slot. JIDE_LOCKS names the
# directory that runner keeps `build.lock.N` in; this script does not guess
# it. DUNE names the dune command, as it does in the Makefile. With no command
# the test step runs, so the proof is that the step passes with a build beside
# it. HELD_RUN_WAIT bounds the wait for a free slot (120 s) and
# HELD_RUN_HOLD is how long the stand-in lives if nothing stops it (3600 s).

set -u

root=$(cd -- "$(dirname -- "$0")/../.." && pwd) || exit 2
runner=${HEAVY:-}
dune=${DUNE:-dune}
locks=${JIDE_LOCKS:-}

# How long the stand-in may wait for a slot, and how long it lives if nothing
# stops it. The second number is the point: a stand-in that is not stopped
# holds its slot for as long as it runs.
wait_seconds=${HELD_RUN_WAIT:-120}
hold_seconds=${HELD_RUN_HOLD:-3600}

wrapper=
stand_in=
lock=
slot=
command_pid=

refuse() {
	printf 'held-run: %s\n' "$*" >&2
	exit 2
}

[ -n "$runner" ] || refuse "no slot runner: set HEAVY to the command that holds the slots (the Makefile passes it to its heavy steps)"
[ -n "$locks" ] || refuse "no lock directory: set JIDE_LOCKS to the directory the runner keeps its build.lock.N in"
[ -f "$root/bench/run.sh" ] || refuse "cannot find the driver at $root/bench/run.sh"

cd "$root" || refuse "cannot enter $root"

tmp=$(mktemp -d "${TMPDIR:-/tmp}/jasmin-held-run.XXXXXX") || refuse "cannot make a temporary directory"

# end <pid>: stop one process and wait for it to be gone. TERM first, because
# a shell that holds a slot releases it from its own EXIT and TERM traps and
# never gets to them under KILL; KILL only when TERM did not land.
end() {
	pid=$1
	[ -n "$pid" ] || return 0
	kill -TERM "$pid" 2>/dev/null || return 0
	i=0
	while [ "$i" -lt 50 ] && kill -0 "$pid" 2>/dev/null; do
		sleep 0.1
		i=$((i + 1))
	done
	kill -KILL "$pid" 2>/dev/null
	return 0
}

children() {
	ps -A -o pid=,ppid= | awk -v parent="$1" '$2 == parent { print $1 }'
}

# end_tree <pid>: what end does, for a process and everything under it. A
# build is a tree — a shell, dune, and the compilers dune started — and ending
# its root alone leaves the rest running against a slot that has been given
# back.
end_tree() {
	pid=$1
	[ -n "$pid" ] || return 0
	for child in $(children "$pid"); do end_tree "$child"; done
	end "$pid"
}

