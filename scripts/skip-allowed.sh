#!/bin/sh
set -u

usage() {
	printf 'usage: %s <step> <reason>\n' "${0##*/}" >&2
	exit 2
}

[ $# -eq 2 ] || usage
step=$1
reason=$2

allowed=$(printf ' %s ' "${JASMIN_ALLOWED_SKIPS:-}" | tr ',' ' ')

case "$allowed" in
*" $step "*)
	printf 'skip %s (%s; allowed by JASMIN_ALLOWED_SKIPS)\n' "$step" "$reason"
	exit 0
	;;
esac

printf 'fail %s (%s): the step did not run, so its checks were not measured; set JASMIN_ALLOWED_SKIPS=%s to allow this skip, and the gate log will say it was allowed\n' \
	"$step" "$reason" "$step"
exit 1
