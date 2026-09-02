#!/bin/sh

set -u

cd "$(dirname "$0")/.." || exit 1

available=1
cache=editor/code/.vscode-test
for directory in "$cache"/vscode-*; do
	[ -d "$directory" ] || continue
	if [ -d "$directory/Visual Studio Code.app" ] ||
		[ -x "$directory/code" ] ||
		[ -x "$directory/bin/code" ]; then
		available=0
		break
	fi
done

if [ "$available" -ne 0 ]; then
	url=${JIDE_VSCODE_PROBE_URL:-https://update.code.visualstudio.com/api/releases/stable}
	if command -v curl >/dev/null 2>&1; then
		if curl -fsSI --max-time 15 -o /dev/null "$url" >/dev/null 2>&1; then
			available=0
		fi
	fi
fi

exit "$available"
