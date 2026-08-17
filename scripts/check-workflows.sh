#!/bin/sh
#
#
#
#
#
#
#
#
#

set -u

prog=${0##*/}
private=0
dir=

usage() {
	printf 'usage: %s [--private] [<workflow directory>]\n' "$prog" >&2
	exit 2
}

for argument in "$@"; do
	case $argument in
	--private) private=1 ;;
	-h | --help) usage ;;
	-*) printf '%s: unknown option: %s\n' "$prog" "$argument" >&2; usage ;;
	*)
		[ -z "$dir" ] || usage
		dir=$argument
		;;
	esac
done

here=$(unset CDPATH && cd -- "$(dirname -- "$0")/.." && pwd) || exit 2
cd -- "$here" || exit 2

[ -n "$dir" ] || dir=.github/workflows
if [ ! -d "$dir" ]; then
	printf '%s: no workflow directory: %s\n' "$prog" "$dir" >&2
	exit 2
fi

workflows=$(find "$dir" -maxdepth 1 -type f \( -name '*.yml' -o -name '*.yaml' \) |
	LC_ALL=C sort)
if [ -z "$workflows" ]; then
	printf '%s: no workflow files in %s\n' "$prog" "$dir" >&2
	exit 2
fi

tmp=$(mktemp -d "${TMPDIR:-/tmp}/check-workflows.XXXXXX") || exit 2
cleanup() { rm -rf "$tmp"; }
trap cleanup EXIT HUP INT TERM

facts=$tmp/facts
: >"$facts"
found=$tmp/found
: >"$found"

#

if python3 -c 'import yaml' >/dev/null 2>&1; then
	mode=yaml
else
	mode=structural
fi

extract_yaml() {
	python3 - "$1" <<'PYTHON' >>"$facts"
import sys
import yaml

def emit(*fields):
    sys.stdout.write("\t".join(str(f) for f in fields) + "\n")

def scalar(node):
    return node.value if isinstance(node, yaml.ScalarNode) else None

def text(node, when_missing="nokey"):
    if node is None:
        return when_missing
    return scalar(node) if scalar(node) is not None else when_missing

