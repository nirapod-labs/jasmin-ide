#!/bin/sh
# Check the GitHub workflows without running them.
#
#   sh scripts/check-workflows.sh [<directory>]
#
# The workflows do not run while the repository is private, so what a run
# would prove is checked by reading them: actionlint, when installed, for the
# syntax and the expressions; then the rules below. Every violation is one
# line, `<rule>: <file>:<line>: <what>`, and any violation fails the run.
#
#   pins         an action is pinned to a 40-character commit SHA, or every
#                action still holds the placeholder PIN that the first public
#                release replaces; a tag, a branch or a mix of the two fails.
#   permissions  each workflow sets `contents: read` at its top level and
#                grants no write there, so a write is always a job's own.
#   make         every `make <target>` a step runs is a target of the Makefile.
#   files        every script a step runs with sh, bash or node is in the tree.
#   content      no local path and no name of an AI tool.

set -u
dir=${1:-.github/workflows}
cd "$(dirname "$0")/.." || exit 2
ls "$dir"/*.yml >/dev/null 2>&1 || { echo "check-workflows: no workflow in $dir" >&2; exit 2; }

out=$(mktemp) || exit 2
trap 'rm -f "$out"' EXIT

if command -v actionlint >/dev/null 2>&1; then
	actionlint "$dir"/*.yml >>"$out" 2>&1 || true
else
	echo "check-workflows: actionlint is not installed; syntax not checked"
fi

targets=$(sed -n 's/^\([A-Za-z][A-Za-z0-9_.-]*\):.*/\1/p' Makefile)

for file in "$dir"/*.yml; do
	awk -v file="$file" -v targets=" $(echo $targets) " '
		function bad(rule, what) { printf "%s: %s:%d: %s\n", rule, file, FNR, what }
		/^permissions:/ { top = 1; seen = 1; next }
		top && /^[^ ]/ { top = 0 }
		top && /^  contents: read/ { read = 1 }
		top && /: write/ { bad("permissions", "a write granted to every job") }
		{ line = $0; sub(/[ \t]#.*/, "", line) }
		line ~ /uses: / && line !~ /uses: \.\// && line !~ /docker:\/\// {
			ref = line; sub(/.*@/, "", ref); sub(/[ \t].*/, "", ref)
			if (ref == "PIN") placeholder++
			else if (ref ~ /^[0-9a-f]{40}$/) pinned++
			else bad("pins", "not pinned to a commit: " ref)
			if (placeholder && pinned) bad("pins", "some actions pinned and some still PIN")
		}
		function word(at) { w = substr(line, at); sub(/[ \t].*/, "", w); return w }
		match(line, /(^|[ ;&|(])make [a-z]/) && line !~ /make -C/ {
			t = word(RSTART + RLENGTH - 1)
			if (index(targets, " " t " ") == 0) bad("make", "no Makefile target " t)
		}
		match(line, /(^|[ ;&|(])(sh|bash|node) [A-Za-z.]/) {
			p = word(RSTART + RLENGTH - 1)
			if (p !~ /\$/ && system("test -f \"" p "\"") != 0) bad("files", "no file " p)
		}
		/\/Users\/|\/home\/[a-z]|[Cc]laude|[Cc]odex|[Aa]nthropic/ { bad("content", "a local path or a tool name") }
		END {
			if (!seen || !read) { FNR = 1; bad("permissions", "no top-level contents: read") }
		}' "$file" >>"$out"
done

if [ -s "$out" ]; then
	cat "$out"
	echo "check-workflows: $(wc -l <"$out" | tr -d ' ') violation(s)"
	exit 1
fi

if [ "$pins_set_aside" -eq 1 ]; then
	printf 'check-workflows: every other rule held; this run passes with the pin rules set aside\n'
else
	printf 'check-workflows: every rule held\n'
fi
exit 0
