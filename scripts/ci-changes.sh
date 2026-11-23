#!/bin/sh
set -eu

out=${GITHUB_OUTPUT:-/dev/stdout}
base=${BASE:-}
docs='^(docs/|changes/|\.github/ISSUE_TEMPLATE/)|\.md$'

say() {
	echo "code=$1" | tee -a "$out"
	exit 0
}

[ "${GITHUB_EVENT_NAME:-}" = workflow_dispatch ] && say true
[ "${GITHUB_REF:-}" = refs/heads/main ] && say true
[ -z "$base" ] || [ "$base" = 0000000000000000000000000000000000000000 ] && say true
git cat-file -e "$base^{commit}" 2>/dev/null ||
	git fetch --quiet --depth=1 origin "$base" || say true

files=$(git diff --name-only "$base" HEAD) || say true
[ -n "$files" ] || say false
printf '%s\n' "$files" | grep -Evq "$docs" && say true
say false
