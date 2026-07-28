#!/bin/sh

set -u

usage() {
	printf 'usage: %s <step> <input> <command>\n' "${0##*/}" >&2
}

now() {
	t=$(date +%s%N 2>/dev/null)
	case $t in
	'' | *[!0-9]*) echo $(($(date +%s) * 10)) ;;
	*) echo $((t / 100000000)) ;;
	esac
}

seconds() {
	printf '%d.%ds' $(($1 / 10)) $(($1 % 10))
}

run_step() {
	step=$1
	input=$2
	cmd=$3

	if [ ! -e "$input" ]; then
		printf 'skip %s (not present)\n' "$step"
		return 0
	fi

	start=$(now)
	sh -c "$cmd"
	status=$?
	elapsed=$(($(now) - start))

	if [ "$status" -eq 0 ]; then
		printf 'ok %s %s\n' "$step" "$(seconds "$elapsed")"
	else
		printf 'fail %s %s (exit %s)\n' "$step" "$(seconds "$elapsed")" "$status"
	fi
	return "$status"
}

main() {
	if [ $# -ne 3 ]; then
		usage
		return 2
	fi
	cd "$(dirname "$0")/.." || return 2
	run_step "$@"
}

main "$@"
