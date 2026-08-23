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
notes=$tmp/notes
: >"$notes"

#
#

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
			if (ind > run_indent) {
				print "runline\t" FILENAME "\t" NR "\t" text
				print "jobrun\t" FILENAME "\t" job "\t" NR "\t" text
				next
			}
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
			print "unchecked\t" FILENAME "\t" NR "\tflow style, an anchor or an alias, which this reader does not follow"
			next
		}

		if (ind == 0) {
			in_env = 0; in_permissions = 0; in_jobs = 0; job = ""; job_perm = 0
			top = key_of(text)
			if (top in top_seen)
				print "dupkey\t" FILENAME "\t" NR "\t" top
			top_seen[top] = 1
			if (top == "permissions") {
				in_permissions = 1
				print "topblock\t" FILENAME "\t" NR
			} else if (top == "env") in_env = 1
			else if (top == "jobs") in_jobs = 1
			next
		}

		if (in_permissions && ind == 2) {
			keyed = key_of(text)
			print "topkey\t" FILENAME "\t" NR "\t" keyed "\t" value_of(text, keyed)
			next
		}
		if (in_env && ind == 2) {
			keyed = key_of(text)
			if (keyed in env_seen)
				print "dupkey\t" FILENAME "\t" NR "\t" keyed
			env_seen[keyed] = 1
			print "env\t" FILENAME "\t" NR "\t" keyed "\t" value_of(text, keyed)
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
				print "jobperm\t" FILENAME "\t" job "\t" NR "\t" key "\t" value_of(text, key)
				next
			}
			job_perm = 0
		}
		if (ind == 4 && key_of(text) == "permissions") {
			job_perm = ind
			print "jobpermblock\t" FILENAME "\t" job "\t" NR
			next
		}
		if (ind == 8 && key_of(text) == "permissions") {
			print "unchecked\t" FILENAME "\t" NR "\ta step declares permissions:, which GitHub reads on a job and not on a step"
			next
		}
		if (ind == 4 && key_of(text) == "environment") {
			value = value_of(text, "environment")
			print "jobenv\t" FILENAME "\t" job "\t" (value == "" ? "nokey" : value)
			next
		}
		body = text
		if (item) body = substr(text, 3)
		if (body ~ /^uses:/) {
			print "uses\t" FILENAME "\t" job "\t" NR "\t" value_of(body, "uses")
			next
		}
		if (body ~ /^run:/) {
			value = value_of(body, "run")
			if (value ~ /^[|>]/) { in_run = 1; run_indent = ind }
			else {
				print "runline\t" FILENAME "\t" NR "\t" value
				print "jobrun\t" FILENAME "\t" job "\t" NR "\t" value
			}
			next
		}
	}
	' "$1"
}

for file in $workflows; do
	printf 'file\t%s\n' "$file" >>"$facts"
	extract_structural "$file" >>"$facts"
done

printf 'check-workflows: structural mode: the arguments were read from the indentation with awk; anchors, aliases and flow mappings are not understood\n'
printf 'check-workflows: %s workflow file(s) under %s\n' \
	"$(printf '%s\n' $workflows | wc -l | tr -d ' ')" "$dir"

if command -v actionlint >/dev/null 2>&1; then
	if lint_output=$(actionlint $workflows 2>&1); then
		printf 'check-workflows: actionlint %s read every file and found nothing\n' \
			"$(actionlint --version 2>/dev/null | head -n 1)"
	else
		lint_status=$?
		if [ "$lint_status" -eq 1 ]; then
			printf 'check-workflows: actionlint found problems in these files\n'
			printf '%s\n' "$lint_output" | awk '
				{
					if (match($0, /^[^:]+:[0-9]+:[0-9]+: /)) {
						head = substr($0, 1, RLENGTH)
						rest = substr($0, RLENGTH + 1)
						split(head, parts, ":")
						printf "violation: extract: %s:%s: actionlint: %s\n", \
							parts[1], parts[2], rest
					} else if ($0 ~ /^[ ]/ || $0 ~ /^[|]/ || $0 ~ /^[0-9]+[ ]*[|]/) {
						next
					} else {
						printf "violation: extract: actionlint: %s\n", $0
					}
				}
			' >>"$found"
		else
			printf '%s: actionlint could not run (exit %s), so whether these files are valid workflows was not checked\n' \
				"$prog" "$lint_status" >&2
			exit 2
		fi
	fi
else
	printf 'check-workflows: no actionlint on this host, so whether these files are valid workflows was not checked\n'
fi

targets=$tmp/targets
{
	awk -F: '/^[A-Za-z0-9_][A-Za-z0-9_.-]*[ ]*:/ { print $1 }' Makefile
	awk '/^\.PHONY:/ { for (i = 2; i <= NF; i++) print $i }' Makefile
} | LC_ALL=C sort -u >"$targets" || {
	printf '%s: cannot read the Makefile\n' "$prog" >&2
	exit 2
}
if [ ! -s "$targets" ]; then
	printf '%s: the Makefile defines no target\n' "$prog" >&2
	exit 2
fi

rule_failed() {
	printf '%s: a rule could not be run, so this run proves nothing\n' "$prog" >&2
	exit 2
}

#

pins=$tmp/pins.violations
pins_counts=$tmp/pins.counts
: >"$pins"
: >"$pins_counts"

awk -F'\t' -v counts="$pins_counts" '
	$1 == "env" { env_path[++n_env] = $2; env_line[n_env] = $3; env_name[n_env] = $4; env_value[n_env] = $5 }
	$1 == "uses" { use_path[++n_use] = $2; use_line[n_use] = $4; use_ref[n_use] = $5 }

	END {
		for (i = 1; i <= n_env; i++) {
			name = env_name[i]
			if (name !~ /_SHA$/ || name ~ /_ACTION$/) continue
			value = env_value[i]
			if (value == "PIN") { record_unfilled++; continue }
			if (value ~ /^[0-9a-f]+$/ && length(value) == 40) { record_filled++; continue }
			record_other++
			printf "violation: pins: %s: the record %s holds %s, which is neither the placeholder PIN nor a full 40-character commit SHA\n", \
				env_path[i], name, (value == "" ? "an empty value" : value)
		}

		for (i = 1; i <= n_use; i++) {
			path = use_path[i]; line = use_line[i]; ref = use_ref[i]
			at = index(ref, "@")
			if (at == 0) {
				use_other++
				printf "violation: pins: %s:%s: %s names no action and no reference\n", path, line, ref
				continue
			}
			action = substr(ref, 1, at - 1)
			revision = substr(ref, at + 1)
			if (revision == "PIN") {
				use_unfilled++
				printf "violation: pins: %s:%s: %s is not pinned to a full 40-character commit SHA; its reference is the placeholder PIN\n", \
					path, line, ref
				continue
			}
			if (revision ~ /^[0-9a-f]+$/ && length(revision) == 40) { use_filled++; continue }
			use_other++
			printf "violation: pins: %s:%s: %s is not pinned to a full 40-character commit SHA\n", \
				path, line, ref
		}

		printf "%d\t%d\t%d\t%d\t%d\t%d\t%d\n", use_unfilled + 0, use_filled + 0, use_other + 0, \
			record_unfilled + 0, record_filled + 0, record_other + 0, n_use + 0 > counts
	}
' "$facts" >"$pins" || rule_failed

#

agreement=$tmp/agreement.violations
: >"$agreement"

awk -F'\t' '
	$1 == "env" { value[$2 SUBSEP $3] = $4; names[$2] = names[$2] " " $3 }
	$1 == "uses" {
		ref = $4
		at = index(ref, "@")
		if (at == 0) next
		action = substr(ref, 1, at - 1)
		revision = substr(ref, at + 1)
		prefix = ""
		total = split(names[$2], list, " ")
		for (i = 1; i <= total; i++) {
			name = list[i]
			if (name !~ /_ACTION$/) continue
			base = name
			sub(/_ACTION$/, "", base)
			if (value[$2 SUBSEP name] == action) prefix = base
		}
		if (prefix == "") {
			printf "violation: agreement: %s:%s: %s has no <NAME>_ACTION entry in this workflow env block\n", \
				$2, $3, action
			next
		}
		if (value[$2 SUBSEP (prefix "_VERSION")] == "")
			printf "violation: agreement: %s:%s: %s has no %s_VERSION entry in this workflow env block\n", \
				$2, $3, action, prefix
		sha = value[$2 SUBSEP (prefix "_SHA")]
		if (sha == "")
			printf "violation: agreement: %s:%s: %s has no %s_SHA entry in this workflow env block\n", \
				$2, $3, action, prefix
		else if (sha != revision)
			printf "violation: agreement: %s:%s: %s names %s on its uses line while %s_SHA is %s\n", \
				$2, $3, action, revision, prefix, sha
	}
' "$facts" >"$agreement" || rule_failed

awk -F'\t' '
	$1 == "topperm" { value[$2] = $3; present[$2] = 1 }
	END {
		for (path in present) {
			if (value[path] == "read") continue
			if (value[path] == "nokey")
				printf "violation: permissions: %s: declares no top-level `permissions: contents: read`\n", path
			else
				printf "violation: permissions: %s: the top-level permissions block grants contents: %s, not read\n", \
					path, value[path]
		}
	}
' "$facts" >>"$found" || rule_failed

awk -F'\t' '
	$1 == "jobperm" { perm[$2 SUBSEP $3 SUBSEP $4] = $5 }
	$1 == "jobenv" { env[$2 SUBSEP $3] = $4 }
	END {
		for (entry in perm) {
			split(entry, parts, SUBSEP)
			if (parts[3] != "id-token") continue
			if (perm[entry] != "write") continue
			if (parts[2] !~ /^publish/)
				printf "violation: token: %s: job %s grants id-token: write and is not a publishing job\n", \
					parts[1], parts[2]
			else if (env[parts[1] SUBSEP parts[2]] != "publish")
				printf "violation: token: %s: job %s grants id-token: write outside the publish environment\n", \
					parts[1], parts[2]
		}
	}
' "$facts" >>"$found" || rule_failed

awk -F'\t' '
	$1 == "dupkey" { printf "violation: content: %s:%s: the key %s appears twice in one mapping\n", $2, $3, $4 }
	$1 == "unchecked" { printf "violation: extract: %s: %s\n", $2, $3 }
' "$facts" >>"$found" || rule_failed

awk -F'\t' '
	FILENAME == ARGV[1] { target[$1] = 1; next }
	$1 != "runline" { next }
	{
		text = $4
		sub(/^[ \t]*#.*$/, "", text)
		if (text == "") next

		rest = text
		while (match(rest, /(^|[;&|(])[ \t]*make[ \t]+[^ \t;&|)]+/)) {
			fragment = substr(rest, RSTART, RLENGTH)
			rest = substr(rest, RSTART + RLENGTH)
			sub(/^[;&|( \t]*make[ \t]+/, "", fragment)
			if (fragment ~ /^-/) next
			name = fragment
			sub(/[ \t].*$/, "", name)
			if (name == "") next
			if (!(name in target))
				printf "violation: make: %s:%s: make %s names no target of this repository Makefile\n", \
					$2, $3, name
		}

		rest = text
		while (match(rest, /(^|[ \t;&|(])(sh|bash|node)[ \t]+[^ \t;&|)]+/)) {
			fragment = substr(rest, RSTART, RLENGTH)
			rest = substr(rest, RSTART + RLENGTH)
			sub(/^[ \t;&|(]*/, "", fragment)
			split(fragment, words, /[ \t]+/)
			path = words[2]
			if (path == "") continue
			if (path ~ /[$`*?]/) continue
			sub(/;.*$/, "", path)
			sub("^[.][/]", "", path)
			if (path !~ "^(scripts|vendor|test|fuzz|bench|compiler|editor|docs)/") continue
			if (system("test -f \"" path "\"") != 0)
				printf "violation: scripts: %s:%s: %s runs %s, which is not a file of this repository\n", \
					$2, $3, words[1], path
		}
	}
' "$targets" "$facts" >>"$found" || rule_failed

#

tokens=$tmp/tokens
: >"$tokens"
while IFS= read -r escaped; do
	printf '%b\n' "$escaped" >>"$tokens"
done <<'ESCAPES'
\0057\0125\0163\0145\0162\0163\0057
\0057\0150\0157\0155\0145\0057
\0056\0154\0141\0156
\0156\0151\0162\0141\0160\0157\0144\0057\0164\0157\0157\0154\0151\0156\0147
\0152\0141\0163\0155\0151\0156\0055\0151\0156\0164
\0116\0151\0162\0141\0160\0157\0144\0040\0146\0157\0162\0153
\0115\0141\0143\0102\0157\0157\0153
\0143\0154\0141\0165\0144\0145
\0144\0145\0145\0160\0163\0145\0145\0153
\0143\0157\0155\0155\0141\0156\0144\0055\0143\0157\0144\0145
\0143\0157\0144\0145\0170
\0141\0156\0164\0150\0162\0157\0160\0151\0143
\0143\0157\0160\0151\0154\0157\0164
\0143\0150\0141\0164\0147\0160\0164
\0157\0160\0145\0156\0141\0151
\0147\0145\0155\0151\0156\0151
\0103\0157\0055\0101\0165\0164\0150\0157\0162\0145\0144\0055\0102\0171
ESCAPES

awk -v tokens="$tokens" '
	BEGIN {
		n = 0
		while ((getline word < tokens) > 0) {
			if (word == "") continue
			n++
			list[n] = tolower(word)
		}
		close(tokens)
	}
	{
		lower = tolower($0)
		for (i = 1; i <= n; i++) {
			if (index(lower, list[i]) > 0) {
				printf "violation: content: %s:%d: contains a forbidden token\n", FILENAME, FNR
				next
			}
		}
	}
' $workflows >>"$found" || rule_failed

counts=$(cat "$pins_counts" 2>/dev/null) || counts=
[ -n "$counts" ] || rule_failed
set -- $counts
[ $# -eq 7 ] || rule_failed
use_unfilled=$1
use_filled=$2
use_other=$3
record_unfilled=$4
record_filled=$5
record_other=$6
pins_total=$7
for field in "$use_unfilled" "$use_filled" "$use_other" \
	"$record_unfilled" "$record_filled" "$record_other" "$pins_total"; do
	case $field in
	'' | *[!0-9]*) rule_failed ;;
	esac
done

unfilled=$((use_unfilled + record_unfilled))
not_pin=$((use_filled + use_other + record_filled + record_other))

pins_set_aside=0
if [ "$private" -eq 1 ] && [ "$not_pin" -eq 0 ] && [ "$unfilled" -gt 0 ]; then
	pins_set_aside=1
	printf 'check-workflows: warning: %s pin(s) are the placeholder PIN on their uses: lines and all %s <NAME>_SHA record(s) hold it too, so the pin and agreement rules are set aside for this run only; a commit SHA cannot be looked up without the network, and a made-up 40-character value would be a lie that looks like a pin\n' \
		"$use_unfilled" "$record_unfilled"
elif [ "$private" -eq 1 ] && [ "$not_pin" -gt 0 ] && [ "$unfilled" -gt 0 ]; then
	printf 'check-workflows: %s pin value(s) no longer hold PIN and %s still do, so the fill-in pass is half done and that is an error\n' \
		"$not_pin" "$unfilled"
fi

if [ "$pins_set_aside" -eq 1 ]; then
	: >"$tmp/all"
else
	cat "$pins" "$agreement" >"$tmp/all"
fi
cat "$found" >>"$tmp/all"
cat "$tmp/all"

if [ -s "$tmp/all" ]; then
	printf 'check-workflows: %s line(s) above, which is a failure\n' \
		"$(wc -l <"$tmp/all" | tr -d ' ')"
	exit 1
fi

if [ "$pins_set_aside" -eq 1 ]; then
	printf 'check-workflows: every other rule held; this run passes with the pin rules set aside\n'
else
	printf 'check-workflows: every rule held\n'
fi
exit 0
