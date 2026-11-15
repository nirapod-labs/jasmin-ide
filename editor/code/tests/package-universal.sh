#!/bin/sh
# Package the universal target end to end, and read the archive back.
#
#   usage: tests/package-universal.sh
#
# The universal package is the one for every platform: the extension with no
# server inside it, for a user who installed `jasmin-lsp` through opam or from
# a release asset. It is the one package that can be built on any host from
# the sources alone, so it is the package this test builds.
#
# `vsce` is the only tool that writes a `.vsix`. It is a pinned development
# dependency, and it is not installed on a host that has not run
# `pnpm install --frozen-lockfile`; a test that failed there would say the
# packaging is broken when nothing was ever run. So a host without
# `node_modules/.bin/vsce` prints why it skipped and passes, and the same is
# true of a host without a built `dist/extension.js`, which is what `vsce`
# packages.
#
# What the test checks, in the archive itself rather than in the file listing:
#   - the four files the extension cannot start without are in it;
#   - there is no `server/` directory anywhere;
#   - there is no source map, no source, no test, no packaging script, no
#     `node_modules` and no `.vscode-test`;
#   - the archive declares no `TargetPlatform`, which is what makes it the
#     package for every platform.
#
# The archive is read with the reader `package-check.mjs` uses, so a package
# that passes here is read the same way by the packaging check.
#
# Exit status: 0 when the package is correct or was skipped, 1 when it is not.
set -u

cd "$(dirname "$0")/.." || exit 2

vsce=node_modules/.bin/vsce
bundle=dist/extension.js

if [ ! -x "$vsce" ]; then
	printf 'skip package-universal (vsce is not installed; run pnpm install --frozen-lockfile)\n'
	exit 0
fi

if [ ! -f "$bundle" ]; then
	printf 'skip package-universal (no %s; run pnpm run bundle)\n' "$bundle"
	exit 0
fi

printf 'vsce %s\n' "$("$vsce" --version 2>/dev/null | tail -1)"

out=$(mktemp -d "${TMPDIR:-/tmp}/jasmin-ide-universal.XXXXXX") || exit 1
trap 'rm -rf "$out"' EXIT INT TERM

if ! "$vsce" package --no-dependencies --out "$out/jasmin-ide.vsix" >"$out/package.log" 2>&1; then
	cat "$out/package.log" >&2
	printf 'fail package-universal (vsce package exited non-zero)\n' >&2
	exit 1
fi

node --input-type=module -e '
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { readZip } from "./scripts/lib/vsix.mjs";

const archive = readZip(await readFile(process.argv[1]));
const paths = archive.paths();

for (const required of [
  "extension/dist/extension.js",
  "extension/language-configuration.json",
  "extension/package.json",
  "extension/syntaxes/jasmin.tmLanguage.json"
]) {
  assert.ok(paths.includes(required), `the package has no ${required}`);
}

const server = paths.filter((path) => path.startsWith("extension/server/"));
assert.deepEqual(server, [], `the universal package carries ${server.join(", ")}`);

for (const path of paths) {
  for (const pattern of [/\.map$/, /\.ts$/, /^extension\/(src|scripts|tests?|fixtures|node_modules|\.vscode-test)\//]) {
    assert.ok(!pattern.test(path), `the package carries ${path}, which matches ${pattern}`);
  }
}

const manifest = archive.text("extension.vsixmanifest");
assert.ok(!manifest.includes("TargetPlatform="), "the universal package declares a target platform");

const size = (await readFile(process.argv[1])).length;
console.log(`universal package: ${paths.length} entries, no server/, ${size} bytes`);
for (const path of paths.filter((path) => path.startsWith("extension/")).sort()) {
  console.log(`  ${path}`);
}
' "$out/jasmin-ide.vsix" || {
	printf 'fail package-universal (the archive does not hold what the contract says)\n' >&2
	exit 1
}

printf 'ok package-universal\n'
