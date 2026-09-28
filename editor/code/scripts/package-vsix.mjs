#!/usr/bin/env node
// Builds one `.vsix` for one platform, with that platform's server inside it.
//
//   node scripts/package-vsix.mjs <platform> <binary>
//   node scripts/package-vsix.mjs universal
//
// The platforms are the ones in `scripts/lib/platforms.mjs`: `darwin-arm64`,
// `linux-x64`, `linux-arm64`, `alpine-x64`, `alpine-arm64` and `universal`. A
// bundled platform takes the server executable built for it, copies it into
// the extension directory as `server/jasmin-lsp` with mode 0755, and writes
// `server/SHA256SUMS` with its digest. `universal` takes no binary: it is the
// package with no server, which finds `jasmin-lsp` on `PATH`.
//
// Before `vsce` runs, the extension's own lookup is run against the extension
// directory it is about to package, so the package is checked with the same
// resolution the extension performs at activation. A package whose lookup
// does not find its server would install and then report that no server is
// installed, and the file listing would not show it.
//
// `vsce` writes the archive to `.artifacts/`, and the script prints its path,
// its size and its SHA-256. The staged `server/` is removed afterwards, so the
// extension directory is left the way it was found.
//
// The script refuses an unknown platform, a missing binary, a binary that is
// not executable, and a `server/` already in the tree: the first three are the
// contract, and the last keeps one package's server out of another's.
import { spawnSync } from "node:child_process";
import { mkdir, readFile, rm, stat } from "node:fs/promises";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { digestOfBytes } from "./lib/digest.mjs";
import { packageFileName, platformByName, platforms } from "./lib/platforms.mjs";
import {
  clearStagedServer,
  hasStagedServer,
  inspectStagedServer,
  stageServer
} from "./lib/stage.mjs";

const editorDirectory = fileURLToPath(new URL("..", import.meta.url));
const artifactsDirectory = join(editorDirectory, ".artifacts");
const vsceCli = join(editorDirectory, "node_modules", ".bin", "vsce");

const platformNames = platforms.map((platform) => platform.name).join(", ");

function usage(message) {
  process.stderr.write(`package-vsix: ${message}\n`);
  process.stderr.write("usage: node scripts/package-vsix.mjs <platform> [<binary>]\n");
  process.stderr.write(`platforms: ${platformNames}\n`);
  process.exit(2);
}

/** Ends the run with a message that says what is wrong. */
function fail(message) {
  process.stderr.write(`package-vsix: ${message}\n`);
  process.exit(1);
}

/** Reports whether a platform's package is meant to bundle a server. */
function needsServer(platform) {
  return platform.vsceTarget !== undefined;
}

const [platformName, binaryArgument, ...rest] = process.argv.slice(2);

if (platformName === undefined) {
  usage("no platform given");
}
if (rest.length > 0) {
  usage(`unexpected argument ${rest[0]}`);
}

const platform = platformByName(platformName);
if (platform === undefined) {
  usage(`unknown platform "${platformName}"`);
}

const manifest = JSON.parse(await readFile(join(editorDirectory, "package.json"), "utf8"));

// A `server/` that is already there is refused rather than replaced: it is
// written per package, so a stale one is a package of another platform
// waiting to be shipped under this platform's name. Nothing is deleted, so
// the run leaves the caller's directory the way it found it.
if (await hasStagedServer(editorDirectory)) {
  fail(
    "server/ is already in the extension directory. " +
      "It is written per package, so remove it before building another one."
  );
}

if (needsServer(platform)) {
  if (binaryArgument === undefined) {
    usage(`${platform.name} needs the server executable built for it`);
  }

  const binary = await stat(binaryArgument).catch(() => undefined);
  if (binary === undefined) {
    fail(`no file at ${binaryArgument}; build the server for ${platform.name} first`);
  }
  if (!binary.isFile()) {
    fail(`${binaryArgument} is not a regular file`);
  }
  if ((binary.mode & 0o111) === 0) {
    fail(`${binaryArgument} is not executable; a server with no execute bit cannot start`);
  }

  const staged = await stageServer(editorDirectory, binaryArgument);
  process.stdout.write(`staged ${platform.name} server at ${staged.serverPath} with mode 0755\n`);
  process.stdout.write(`digest ${staged.digest} written to ${staged.digestPath}\n`);

  const problems = await inspectStagedServer(editorDirectory);
  if (problems.length > 0) {
    await clearStagedServer(editorDirectory);
    fail(`the staged package does not hold a usable server:\n  ${problems.join("\n  ")}`);
  }
} else if (binaryArgument !== undefined) {
  usage("universal takes no server; it is the package that finds jasmin-lsp on PATH");
}

await mkdir(artifactsDirectory, {recursive: true});

const output = join(artifactsDirectory, packageFileName(platform, manifest.version));
await rm(output, {force: true});

const vsceArguments = ["package", "--no-dependencies", "--out", output];
if (platform.vsceTarget !== undefined) {
  vsceArguments.push("--target", platform.vsceTarget);
}

const vsce = spawnSync(vsceCli, vsceArguments, {cwd: editorDirectory, encoding: "utf8"});

if (vsce.error !== undefined) {
  await clearStagedServer(editorDirectory);
  fail(`could not run ${vsceCli}: ${vsce.error.message}`);
}
process.stdout.write(vsce.stdout ?? "");
process.stderr.write(vsce.stderr ?? "");
if (vsce.status !== 0) {
  await clearStagedServer(editorDirectory);
  fail(`${vsceCli} exited with ${vsce.status}`);
}

const packaged = await readFile(output);
process.stdout.write(`vsix ${output}\n`);
process.stdout.write(`sha256 ${digestOfBytes(packaged)}\n`);
process.stdout.write(`size ${packaged.length}\n`);

await clearStagedServer(editorDirectory);
