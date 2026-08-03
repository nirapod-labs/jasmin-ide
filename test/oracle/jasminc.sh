#!/bin/sh
# Run the reference Jasmin compiler the parity checks compare against, and
# print its version.
#
#   test/oracle/jasminc.sh [<path-to-jasminc>]
#
# With a path, that binary is run. Without one, the compiler is the one a
# virtual machine keeps, run through `colima ssh`:
#
#   JASMINC_PROFILE   the colima profile to use, default `jide`
#   JASMINC_VM_PATH   the compiler inside that machine, default
#                     `$HOME/scratch/oracle/jasminc`
#   JASMINC_TIMEOUT   seconds to wait for that machine, default 20
#
# The version is the binary's own `-version` output and is all this script
# writes to stdout (the option is spelled with one dash: upstream's `jasminc`
# does not take `--version`). When there is no compiler to run, it says what
# it looked for on stderr and exits 2. A host without a compiler is a normal
# state: see docs/oracle.md.

set -u

profile=${JASMINC_PROFILE:-jide}
vm_path=${JASMINC_VM_PATH:-\$HOME/scratch/oracle/jasminc}
limit=${JASMINC_TIMEOUT:-20}
case $limit in
'' | *[!0-9]*) limit=20 ;;
esac

say() { printf '%s\n' "test/oracle/jasminc.sh: $*" >&2; }
die() {
	say "error: $*"
	exit 2
}

usage() {
	say "usage: test/oracle/jasminc.sh [<path-to-jasminc>]"
	exit 2
}

# run_limited <seconds> <command> [args...]
#
# Run a command with a time limit, and return 124 when the limit expired
# instead of that command's own status. GNU's `timeout` is not on macOS and
# this script runs on both, so the limit is kept here: the command runs in
# the background, and a watchdog kills it when the time is up. A machine
# that stopped answering is a skip with a message, never a wait: this script
# is called from `make ci`.
#
# The command's stdout goes through a file rather than straight out, because
# the caller captures it with `$(...)`: a process the command leaves behind
# would otherwise hold the pipe open and the capture would wait for that
# process however the command itself ended.
run_limited() {
	seconds=$1
	shift
	mark=${TMPDIR:-/tmp}/jasminc-oracle.$$
	output=$mark.out
	rm -f "$mark" "$output"
	"$@" >"$output" &
	child=$!
	(
		sleep "$seconds"
		if kill -0 "$child" 2>/dev/null; then
			: >"$mark"
			kill "$child" 2>/dev/null
		fi
	) >/dev/null 2>&1 &
	watchdog=$!
	wait "$child"
	status=$?
	kill "$watchdog" 2>/dev/null
	wait "$watchdog" 2>/dev/null
	if [ -f "$mark" ]; then
		rm -f "$mark" "$output"
		return 124
	fi
	cat "$output"
	rm -f "$output"
	return "$status"
}

case $# in
0)
	command -v colima >/dev/null 2>&1 ||
		die "no path was given and colima is not on PATH, so there is no compiler to run"
	run_limited "$limit" colima status --profile "$profile" >/dev/null 2>&1
	status=$?
	[ "$status" -eq 124 ] &&
		die "no path was given and the colima profile '$profile' did not answer within ${limit}s; start it, or pass the compiler's path"
	[ "$status" -eq 0 ] ||
		die "no path was given and the colima profile '$profile' is not running; start it, or pass the compiler's path"
	out=$(run_limited "$limit" colima ssh --profile "$profile" -- sh -c "\"$vm_path\" -version")
	status=$?
	[ "$status" -eq 124 ] &&
		die "the colima profile '$profile' did not answer within ${limit}s; start it, or pass the compiler's path"
	[ "$status" -eq 0 ] ||
		die "the colima profile '$profile' has no compiler at $vm_path; copy one there, or pass its path"
	;;
1)
	[ -f "$1" ] || die "no such file: $1"
	[ -x "$1" ] || die "not executable: $1"
	out=$("$1" -version) || die "$1 -version failed"
	;;
*) usage ;;
esac

[ -n "$out" ] || die "the compiler printed no version"
printf '%s\n' "$out"
