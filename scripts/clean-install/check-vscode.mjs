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

function loadTestModule() {
  const tried = [];
  const anchors = [];
  if (process.env.JASMIN_VSCODE_TEST_MODULE !== undefined) {
    anchors.push(process.env.JASMIN_VSCODE_TEST_MODULE);
  }
  anchors.push(join(repositoryRoot, "editor", "code", "node_modules", "@vscode", "test-electron"));
  anchors.push(join(repositoryRoot, "editor", "code", "package.json"));

  for (const anchor of anchors) {
    tried.push(anchor);
    try {
      const load = createRequire(anchor);
      return load(anchor.endsWith("package.json") ? "@vscode/test-electron" : anchor);
    } catch (error) {
    }
  }
  try {
    const load = createRequire(join(repositoryRoot, "package.json"));
    return load("@vscode/test-electron");
  } catch (error) {
    return { missing: true, tried };
  }
}

const testModule = loadTestModule();
if (testModule.missing === true) {
  skip(
    "no @vscode/test-electron: run `pnpm install --frozen-lockfile` in editor/code, " +
      "or name an installed copy with JASMIN_VSCODE_TEST_MODULE " +
      `(tried ${testModule.tried.join(", ")})`
  );
}

const work = options.work;
const workspace = join(work, "workspace");
await mkdir(work, {recursive: true});
await cp(fixtureDirectory, workspace, {recursive: true});

const socketLimit = 103;
const requestedProfile = options.userDataDirectory ?? join(work, "profile");
let profile = requestedProfile;
let madeProfile = false;
if (requestedProfile.length + 32 > socketLimit) {
  profile = await mkdtemp(join(tmpdir(), "jide-window-"));
  madeProfile = true;
  process.stdout.write(
    `check-vscode: profile ${requestedProfile} is too long for the editor's socket ` +
      `(${socketLimit} characters), so the window uses ${profile}\n`
  );
}
await mkdir(profile, {recursive: true});

const extensionsDirectory = options.extensionsDirectory ?? join(work, "extensions");
const cache = options.cache ?? join(work, "vscode");

const extraArguments = (process.env.JASMIN_IDE_VSCODE_LAUNCH_ARGS ?? "").split(" ").filter((entry) => entry !== "");

process.stdout.write(`check-vscode: editor ${options.version}, installed ${options.extension}\n`);
process.stdout.write(`check-vscode: workspace ${workspace}\n`);

let failure;
const watchdog = setTimeout(() => {
  process.stdout.write(`check-vscode: FAIL the window did not finish within ${options.timeoutSeconds}s\n`);
  process.exit(1);
}, options.timeoutSeconds * 1000);
watchdog.unref();

try {
  await testModule.runTests({
    version: options.version,
    cachePath: cache,
    extensionDevelopmentPath: options.extension,
    extensionTestsPath: suitePath,
    extensionTestsEnv: {
      JASMIN_IDE_CLEAN_INSTALL_EXTENSION: options.extension,
      JASMIN_IDE_CLEAN_INSTALL_WORKSPACE: workspace
    },
    launchArgs: [
      workspace,
      "--disable-extensions",
      "--disable-workspace-trust",
      "--user-data-dir",
      profile,
      "--extensions-dir",
      extensionsDirectory,
      ...extraArguments
    ]
  });
} catch (error) {
  failure = error;
}
clearTimeout(watchdog);

if (failure !== undefined) {
  process.stdout.write(
    `check-vscode: FAIL the window did not finish: ${failure instanceof Error ? failure.message : String(failure)}\n`
  );
  process.exit(1);
}

if (madeProfile) {
  await rm(profile, {recursive: true, force: true});
}

process.stdout.write("check-vscode: PASS diagnostics and definition from the installed package\n");
