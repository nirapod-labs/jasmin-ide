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

def pairs(node):
    """The (key, value) pairs of a mapping, with a repeated key reported."""
    if not isinstance(node, yaml.MappingNode):
        return []
    seen = set()
    out = []
    for key_node, value_node in node.value:
        key = scalar(key_node)
        if key is None:
            continue
        if key in seen:
            emit("dupkey", path, key_node.start_mark.line + 1, key)
        seen.add(key)
        out.append((key, value_node))
    return out

def get(node, name):
    for key, value in pairs(node):
        if key == name:
            return value
    return None

def command_lines(node):
    """One fact per line of a `run:` value, each with its line number."""
    first = node.start_mark.line + 1
    if getattr(node, "style", None) in ("|", ">"):
        first += 1
    for offset, line in enumerate(node.value.split("\n")):
        emit("runline", path, first + offset, line)

path = sys.argv[1]
try:
    with open(path, "r", encoding="utf-8") as handle:
        documents = list(yaml.compose_all(handle, Loader=yaml.SafeLoader))
except yaml.YAMLError as error:
    emit("unchecked", path, "not valid YAML: %s" % error)
    raise SystemExit(0)
except OSError as error:
    emit("unchecked", path, "cannot be read: %s" % error)
    raise SystemExit(0)

if len(documents) != 1 or documents[0] is None:
    emit("unchecked", path, "not exactly one YAML document")
    raise SystemExit(0)

root = documents[0]
if not isinstance(root, yaml.MappingNode):
    emit("unchecked", path, "not a mapping at the top level")
    raise SystemExit(0)

for key, value in pairs(root):
    if key == "permissions":
        emit("topperm", path, text(get(value, "contents")))
    elif key == "env":
        for name, entry in pairs(value):
            emit("env", path, name, text(entry, ""))
    elif key == "jobs":
        for job, body in pairs(value):
            emit("job", path, job)
            for name, entry in pairs(get(body, "permissions")):
                emit("jobperm", path, job, name, text(entry))
            environment = get(body, "environment")
            if environment is not None:
                emit("jobenv", path, job, text(environment))
            steps = get(body, "steps")
            if not isinstance(steps, yaml.SequenceNode):
                continue
            for step in steps.value:
                if not isinstance(step, yaml.MappingNode):
                    continue
                uses = get(step, "uses")
                if uses is not None and isinstance(uses, yaml.ScalarNode):
                    emit("uses", path, uses.start_mark.line + 1, uses.value)
                run = get(step, "run")
                if run is not None and isinstance(run, yaml.ScalarNode):
                    command_lines(run)
PYTHON
}

