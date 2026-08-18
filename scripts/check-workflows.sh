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

extract_structural() {
	awk '
	function trim(s) { sub(/^[ \t]+/, "", s); sub(/[ \t]+$/, "", s); return s }
	function indent_of(s) { match(s, /^[ ]*/); return RLENGTH }
	function key_of(s) { sub(/:.*/, "", s); return trim(s) }
	function value_of(s, field) {
		sub("^" field ":[ ]*", "", s)
		if (s ~ /^["\x27]/) s = substr(s, 2)
		if (s ~ /["\x27]$/) s = substr(s, 1, length(s) - 1)
		return s
	}
	{
		line = $0
		sub(/\r$/, "", line)
		if (trim(line) == "" || trim(line) ~ /^#/) next
		ind = indent_of(line)
		text = trim(line)

		if (in_run) {
			if (ind > run_indent) { print "runline\t" FILENAME "\t" NR "\t" text; next }
			in_run = 0
		}

		keyed = key_of(text)
		value = text
		sub(/^[^:]*:[ ]*/, "", value)
		head = substr(value, 1, 1)
		followed = 0
		if (keyed == "permissions" || keyed == "env" || keyed == "jobs") followed = 1
		if (keyed == "steps" || keyed == "environment") followed = 1
		flow = 0
		if (followed && (head == "{" || head == "[")) flow = 1
		tail = substr(text, 3, 1)
		item = substr(text, 1, 2) == "- "
		if (item && (tail == "{" || tail == "[")) flow = 1
		anchored = index(text, ": &") > 0 || index(text, ": *") > 0
		if (item && (tail == "&" || tail == "*")) anchored = 1
		if (flow || anchored) {
			print "unchecked\t" FILENAME "\tflow style, an anchor or an alias, which this reader does not follow"
			next
		}

		if (ind == 0) {
			in_env = 0; in_permissions = 0; in_jobs = 0; job = ""; job_perm = 0
			top = key_of(text)
			if (top == "permissions") in_permissions = 1
			else if (top == "env") in_env = 1
			else if (top == "jobs") in_jobs = 1
			next
		}

		if (in_permissions && ind == 2 && key_of(text) == "contents") {
			print "topperm\t" FILENAME "\t" value_of(text, "contents")
			next
		}
		if (in_env && ind == 2) {
			print "env\t" FILENAME "\t" key_of(text) "\t" value_of(text, key_of(text))
			next
		}
		if (!in_jobs) next

		if (ind == 2) {
			job = key_of(text)
			print "job\t" FILENAME "\t" job
			job_perm = 0
			next
		}
		if (job == "") next

		if (job_perm) {
			if (ind > job_perm) {
				key = key_of(text)
				print "jobperm\t" FILENAME "\t" job "\t" key "\t" value_of(text, key)
				next
			}
			job_perm = 0
		}
		if (ind == 4 && key_of(text) == "permissions") { job_perm = ind; next }
		if (ind == 4 && key_of(text) == "environment") {
			value = value_of(text, "environment")
			print "jobenv\t" FILENAME "\t" job "\t" (value == "" ? "nokey" : value)
			next
		}
		if (text ~ /^uses:/) {
			print "uses\t" FILENAME "\t" NR "\t" value_of(text, "uses")
			next
		}
		if (text ~ /^run:/) {
			value = value_of(text, "run")
			if (value ~ /^[|>]/) { in_run = 1; run_indent = ind }
			else print "runline\t" FILENAME "\t" NR "\t" value
			next
		}
	}
	' "$1"
}

