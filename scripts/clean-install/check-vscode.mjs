#!/usr/bin/env node

import { cp, mkdir, mkdtemp, rm, stat } from "node:fs/promises";
import { createRequire } from "node:module";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

const scriptDirectory = fileURLToPath(new URL(".", import.meta.url));
const repositoryRoot = fileURLToPath(new URL("../..", import.meta.url));
const fixtureDirectory = join(scriptDirectory, "fixture");
const suitePath = join(scriptDirectory, "suite.js");

const defaultVersion = "1.145.2";

function usage(message) {
  process.stderr.write(`check-vscode: ${message}\n`);
  process.stderr.write(
    "usage: node scripts/clean-install/check-vscode.mjs --extension <dir> --work <dir> " +
      "[--extensions-dir <dir>] [--user-data-dir <dir>] [--version <v>] [--cache <dir>] " +
      "[--timeout <seconds>]\n"
  );
  process.exit(2);
}

function skip(reason) {
  process.stdout.write(`skip clean-install (${reason})\n`);
  process.exit(3);
}

const options = {
  extension: undefined,
  work: undefined,
  extensionsDirectory: undefined,
  userDataDirectory: undefined,
  version: defaultVersion,
  cache: process.env.JASMIN_IDE_VSCODE_CACHE,
  timeoutSeconds: 900
};

const arguments_ = process.argv.slice(2);
for (let index = 0; index < arguments_.length; index += 1) {
  const name = arguments_[index];
  const value = arguments_[index + 1];
  switch (name) {
    case "--extension":
      options.extension = value;
      index += 1;
      break;
    case "--work":
      options.work = value;
      index += 1;
      break;
    case "--extensions-dir":
      options.extensionsDirectory = value;
      index += 1;
      break;
    case "--user-data-dir":
      options.userDataDirectory = value;
      index += 1;
      break;
    case "--version":
      options.version = value;
      index += 1;
      break;
    case "--cache":
      options.cache = value;
      index += 1;
      break;
    case "--timeout":
      options.timeoutSeconds = Number(value);
      index += 1;
      break;
    default:
      usage(`unknown argument ${name}`);
  }
}

if (options.extension === undefined) {
  usage("no --extension given");
}
if (options.work === undefined) {
  usage("no --work given");
}

const installed = await stat(join(options.extension, "package.json")).catch(() => undefined);
if (installed === undefined) {
  usage(`${options.extension} holds no package.json; it is not an installed extension`);
}

