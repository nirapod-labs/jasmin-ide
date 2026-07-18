import assert from "node:assert/strict";
import { access, mkdir } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const artifactsDirectory = new URL("../.artifacts/", import.meta.url);
const output = new URL("../.artifacts/jasmin-ide.vsix", import.meta.url);
const vsceCli = new URL("../node_modules/.bin/vsce", import.meta.url);

// The files the published extension cannot work without: the bundled client
// entry point, the language configuration and the TextMate grammar.
const requiredEntries = [
  "dist/extension.js",
  "language-configuration.json",
  "package.json",
  "syntaxes/jasmin.tmLanguage.json"
];

await mkdir(artifactsDirectory, {recursive: true});

const listing = spawnSync(fileURLToPath(vsceCli), ["ls", "--no-dependencies"], {
  encoding: "utf8"
});

if (listing.status !== 0) {
  process.stderr.write(listing.stderr ?? "");
  process.exit(listing.status ?? 1);
}

const listed = listing.stdout
  .split(/\r?\n/u)
  .map((entry) => entry.trim())
  .filter((entry) => entry !== "");

for (const entry of requiredEntries) {
  assert.ok(listed.includes(entry), `vsce ls does not list ${entry}`);
}

console.log(`vsce ls lists ${listed.length} files, including the bundle and the grammar`);

const result = spawnSync(
  fileURLToPath(vsceCli),
  ["package", "--no-dependencies", "--out", fileURLToPath(output)],
  {stdio: "inherit"}
);

if (result.status !== 0) {
  process.exit(result.status ?? 1);
}

await access(output);
