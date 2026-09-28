#!/usr/bin/env node

import { readFile } from "node:fs/promises";
import { isDeepStrictEqual } from "node:util";

const [packagedPath, installedPath] = process.argv.slice(2);

if (packagedPath === undefined || installedPath === undefined) {
  process.stderr.write("usage: node scripts/clean-install/manifest.mjs <packaged> <installed>\n");
  process.exit(2);
}

async function load(path) {
  let text;
  try {
    text = await readFile(path, "utf8");
  } catch (error) {
    process.stderr.write(`manifest: cannot read ${path}: ${error.message}\n`);
    process.exit(2);
  }
  try {
    return JSON.parse(text);
  } catch (error) {
    process.stderr.write(`manifest: ${path} is not JSON: ${error.message}\n`);
    process.exit(2);
  }
}

const packaged = await load(packagedPath);
const installed = await load(installedPath);

const metadata = installed.__metadata;
delete installed.__metadata;

const keys = new Set([...Object.keys(packaged), ...Object.keys(installed)]);
const differences = [];
for (const key of [...keys].sort()) {
  if (!isDeepStrictEqual(packaged[key], installed[key])) {
    differences.push(
      `  ${key}: packaged ${JSON.stringify(packaged[key])}, installed ${JSON.stringify(installed[key])}`
    );
  }
}

if (differences.length > 0) {
  process.stdout.write(`manifest: the installed manifest differs from the package in ${differences.length} field(s)\n`);
  process.stdout.write(`${differences.join("\n")}\n`);
  process.exit(1);
}

process.stdout.write(
  `manifest: installed manifest matches the package in ${keys.size} field(s)` +
    `${metadata === undefined ? ", with no metadata" : ", with the editor's own metadata"}\n`
);
