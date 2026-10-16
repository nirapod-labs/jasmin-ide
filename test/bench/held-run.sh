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

# finish: the one end of every path. Nothing below returns before the
# stand-in is gone and the lock it held is free.
finish() {
	status=$1
	# No second run of this, and no second signal through it: a TERM that
	# arrived while the cleanup was ending the stand-in would otherwise stop
	# the cleanup and leave exactly what it exists to remove.
	trap - EXIT
	trap '' INT TERM HUP

	# The command first: a step that is still running would keep building in
	# a slot this script is about to give back.
	[ -n "$command_pid" ] && end_tree "$command_pid"

	if [ -n "$wrapper" ]; then
		# The sleep under the stand-in is a child of it, so the sleep goes
		# first: a shell killed on its own leaves its child running.
		if [ -n "$stand_in" ]; then
			end_tree "$stand_in"
		else
			# The stand-in was never identified, so end what the runner has
			# under it instead; that is what the runner is waiting for.
			for child in $(children "$wrapper"); do end_tree "$child"; done
		fi
		# The runner releases the slot in its EXIT trap, so this waits for the
		# release rather than for the process alone. It is bounded: a runner
		# still waiting for a child this script could not find must not hold
		# the cleanup for the stand-in's whole life.
		i=0
		while [ "$i" -lt 100 ] && kill -0 "$wrapper" 2>/dev/null; do
			sleep 0.1
			i=$((i + 1))
		done
		# Its own TERM trap releases the slot, which is why KILL is not used
		# here even when the wait ran out.
		kill -TERM "$wrapper" 2>/dev/null
		i=0
		while [ "$i" -lt 50 ] && kill -0 "$wrapper" 2>/dev/null; do
			sleep 0.1
			i=$((i + 1))
		done
		kill -KILL "$wrapper" 2>/dev/null
		wait "$wrapper" 2>/dev/null
	fi

	# The lock is a directory the runner removes; give a release that has just
	# started a moment to finish before calling it stuck.
	i=0
	while [ -n "$lock" ] && [ -d "$lock" ] && [ "$i" -lt 100 ]; do
		sleep 0.1
		i=$((i + 1))
	done

	rm -rf "$tmp"

	if [ -n "$lock" ] && [ -d "$lock" ]; then
		printf 'held-run: %s is still held after the stand-in was stopped\n' "$lock" >&2
		printf 'held-run: ask the runner for its --status, end the owner if it is dead, and say so\n' >&2
		exit 1
	fi
	if [ -n "$stand_in" ] && kill -0 "$stand_in" 2>/dev/null; then
		printf 'held-run: the stand-in pid %s is still running\n' "$stand_in" >&2
		exit 1
	fi
	if [ -n "$lock" ]; then
		printf 'held-run: pid %s held %s, and both are gone\n' "$stand_in" "$lock"
	fi
	exit "$status"
}

trap 'finish $?' EXIT
trap 'finish 130' INT
trap 'finish 143' TERM
trap 'finish 129' HUP

# The stand-in. `exec -a dune` gives the process the command name `dune`, and
# the `build` word beside it is what the driver's probe matches. A shell that
# runs a single command replaces itself with that command, and the name would
# leave the process table with it, so the shell keeps a second command to run
# after the sleep: the name stays until the sleep is ended.
$runner /bin/sh -c "exec -a dune /bin/sh -c 'sleep $hold_seconds; :' build" &
wrapper=$!

# The runner takes a free slot and writes an owner file into it, so the lock
# that belongs to this run is the one whose owner names the runner process
# this script started. A host whose slots are all busy never gets one, and
# this run then holds nothing and says so rather than waiting forever.
i=0
while [ -z "$lock" ] && [ "$i" -lt "$((wait_seconds * 10))" ]; do
	for candidate in "$locks"/build.lock.*; do
		[ -d "$candidate" ] || continue
		owner=$(cat "$candidate/owner" 2>/dev/null) || continue
		case $owner in
		"pid $wrapper "*) lock=$candidate ;;
		esac
	done
	if [ -z "$lock" ]; then
		kill -0 "$wrapper" 2>/dev/null || refuse "the runner ended without taking a slot"
		sleep 0.1
	fi
	i=$((i + 1))
done
[ -n "$lock" ] || refuse "no slot in ${wait_seconds}s: the host's slots are busy, so this run held nothing"
slot=${lock##*.}

