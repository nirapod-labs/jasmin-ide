/**
 * The packaging contract, exercised without an editor, a network or `vsce`.
 *
 * `scripts/package-vsix.mjs` copies a server executable into the extension
 * directory, gives it mode 0755 and writes `server/SHA256SUMS` before `vsce`
 * runs. Three things make that package usable, and each of them can be wrong
 * on its own:
 *
 *   - the executable has to sit where the extension's lookup reads it, at
 *     `server/jasmin-lsp` inside the installation;
 *   - it has to carry the execute bit, or VS Code starts a file it cannot;
 *   - the digest file has to name it and record the digest of the bytes that
 *     are packaged, or the file says nothing about the executable beside it.
 *
 * This file stages a fake binary in a temporary directory and checks all
 * three, then breaks each one and checks that the check notices. The staging
 * code is the code the packaging script runs, so a package that passes here
 * passes the part of the contract that can be measured without `vsce`.
 *
 * The one thing this cannot check is what `vsce` puts in the archive: that is
 * `scripts/package-check.mjs`, which reads the `.vsix` back.
 */
import assert from "node:assert/strict";
import { chmod, mkdir, mkdtemp, readFile, rm, stat, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { digestOfBytes, digestFileContents, parseDigestFile } from "../scripts/lib/digest.mjs";
import {
  bundledPlatforms,
  digestFileName,
  packageFileName,
  packagedDigestPath,
  packagedServerPath,
  platformByName,
  platforms,
  serverDirectory,
  serverExecutable
} from "../scripts/lib/platforms.mjs";
import {
  bundledDigestPath,
  bundledServerPath,
  clearStagedServer,
  hasStagedServer,
  inspectStagedServer,
  stagedResolution,
  stagedServerMode,
  stageServer
} from "../scripts/lib/stage.mjs";

/** A temporary directory that is removed when the test ends. */
async function temporaryDirectory(t) {
  const directory = await mkdtemp(join(tmpdir(), "jasmin-ide-package-"));
  t.after(() => rm(directory, {recursive: true, force: true}));
  return directory;
}

/** A fake server executable: a shell script, so it can be run as well as read. */
async function fakeServer(directory, name = "jasmin-lsp-fake") {
  const path = join(directory, name);
  await writeFile(path, "#!/bin/sh\nprintf 'jasmin-lsp\\n'\n", "utf8");
  await chmod(path, 0o755);
  return path;
}

test("every platform the extension is packaged for is named once", () => {
  assert.deepEqual(
    platforms.map((platform) => platform.name),
    ["darwin-arm64", "linux-x64", "linux-arm64", "alpine-x64", "alpine-arm64", "universal"]
  );

  for (const platform of bundledPlatforms) {
    assert.equal(platform.vsceTarget, platform.name, `${platform.name} names another target`);
  }

  assert.equal(platformByName("universal").vsceTarget, undefined);
  assert.equal(platformByName("linux-x86_64"), undefined, "the release asset's spelling is not a target");
  assert.equal(platformByName("win32-x64"), undefined);
});

test("a platform package names the server and the digest at the path the lookup reads", () => {
  for (const platform of bundledPlatforms) {
    assert.equal(packagedServerPath(platform), "server/jasmin-lsp");
    assert.equal(packagedDigestPath(platform), "server/SHA256SUMS");
  }
});

test("the package file name carries the target, and the universal one does not", () => {
  assert.equal(packageFileName(platformByName("darwin-arm64"), "0.1.0"), "jasmin-ide-darwin-arm64-0.1.0.vsix");
  assert.equal(packageFileName(platformByName("alpine-arm64"), "0.1.0"), "jasmin-ide-alpine-arm64-0.1.0.vsix");
  assert.equal(packageFileName(platformByName("universal"), "0.1.0"), "jasmin-ide-0.1.0.vsix");
});

test("staging a server puts it where the extension's lookup finds it", async (t) => {
  const directory = await temporaryDirectory(t);
  const binary = await fakeServer(directory);
  const extension = join(directory, "extension");

  const staged = await stageServer(extension, binary);

  assert.equal(staged.serverPath, bundledServerPath(extension));
  assert.equal(staged.serverPath, join(extension, "server", "jasmin-lsp"));
  assert.equal(staged.digestPath, bundledDigestPath(extension));

  const resolution = stagedResolution(extension);
  assert.deepEqual(resolution, {kind: "found", path: staged.serverPath, source: "extension"});
});

test("staging a server gives it mode 0755, whatever the source had", async (t) => {
  const directory = await temporaryDirectory(t);
  const binary = await fakeServer(directory);
  // A binary that is executable only for its owner is a file the package
  // cannot rely on: VS Code runs the extension as the installing user, but
  // the archive is read by whatever extracts it.
  await chmod(binary, 0o700);

  const extension = join(directory, "extension");
  const staged = await stageServer(extension, binary);

  const mode = (await stat(staged.serverPath)).mode & 0o777;
  assert.equal(mode, stagedServerMode);
  assert.equal(stagedServerMode, 0o755);
});

test("staging a server writes a digest file that matches the packaged bytes", async (t) => {
  const directory = await temporaryDirectory(t);
  const binary = await fakeServer(directory);
  const extension = join(directory, "extension");

  const staged = await stageServer(extension, binary);

  assert.equal(staged.digest, digestOfBytes(await readFile(binary)));

  const text = await readFile(staged.digestPath, "utf8");
  assert.equal(text, digestFileContents([[serverExecutable, staged.digest]]));

  // The form `sha256sum` writes and its `--check` mode reads.
  assert.match(text, /^[0-9a-f]{64} {2}jasmin-lsp\n$/u);

  const entries = parseDigestFile(text);
  assert.deepEqual([...entries], [[serverExecutable, staged.digest]]);
});

test("a staged server passes the check the packaging script runs", async (t) => {
  const directory = await temporaryDirectory(t);
  const binary = await fakeServer(directory);
  const extension = join(directory, "extension");

  await stageServer(extension, binary);

  assert.deepEqual(await inspectStagedServer(extension), []);
});

test("the check notices a digest file that records another digest", async (t) => {
  const directory = await temporaryDirectory(t);
  const binary = await fakeServer(directory);
  const extension = join(directory, "extension");
  await stageServer(extension, binary);

  // Another file's digest, which is what a package built from a rebuilt
  // server would carry if the digest file were written from the wrong file.
  const other = digestOfBytes(Buffer.from("not the server\n"));
  await writeFile(bundledDigestPath(extension), digestFileContents([[serverExecutable, other]]));

  const problems = await inspectStagedServer(extension);
  assert.equal(problems.length, 1);
  assert.match(problems[0], /records/u);
  assert.match(problems[0], /but the file is/u);
});

test("the check notices a missing digest file", async (t) => {
  const directory = await temporaryDirectory(t);
  const binary = await fakeServer(directory);
  const extension = join(directory, "extension");
  await stageServer(extension, binary);

  await rm(bundledDigestPath(extension));

  const problems = await inspectStagedServer(extension);
  assert.deepEqual(problems, [`${serverDirectory}/${digestFileName} is missing`]);
});

