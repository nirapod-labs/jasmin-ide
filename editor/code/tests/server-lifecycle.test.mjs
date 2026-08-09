/**
 * The lifecycle of the server the extension starts, exercised without VS Code.
 *
 * `extension.ts` cannot be imported outside the editor: it needs the editor's
 * API. The parts that decide what process is started do not: `server-lookup.ts`
 * finds the executable and `server-start.ts` builds the launch plan (command,
 * arguments, transport, `initialize` options). Both are imported here as they
 * are, so this test starts the same executable with the same arguments the
 * extension starts.
 *
 * The settings come from the extension's own manifest, so the argument list
 * and the initialize options are the ones a default installation sends rather
 * than values copied into the test. Over stdio the test then runs the four
 * lifecycle steps of the protocol, framing every message the way the
 * repository's own client and tests do, and checks the replies.
 *
 * What remains outside this test is the editor's half: the client
 * capabilities VS Code supplies and the `LanguageClient` object that carries
 * them. That half needs a window and is in `tests/vscode/suite.js`.
 */
import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { readFileSync, statSync } from "node:fs";
import { join } from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";
import { resolveServerPath } from "../src/server-lookup.ts";
import { planServerLaunch } from "../src/server-start.ts";

const editorDirectory = fileURLToPath(new URL("..", import.meta.url));
const repositoryRoot = fileURLToPath(new URL("../../..", import.meta.url));
const builtServer = join(repositoryRoot, "_build", "default", "lsp-server", "native", "main.exe");

const manifest = JSON.parse(readFileSync(join(editorDirectory, "package.json"), "utf8"));

/** One setting's default, as the extension's manifest declares it. */
function defaultSetting(name) {
  const properties = manifest.contributes.configuration.properties;
  const property = properties[`jasminIde.${name}`];
  assert.ok(property, `the manifest declares no jasminIde.${name} setting`);
  return property.default;
}

const settings = {
  // The first place the lookup tries is the setting, so a machine that has
  // the executable elsewhere points this at it instead of moving the binary.
  serverPath: process.env.JASMIN_LSP ?? builtServer,
  serverArgs: defaultSetting("server.args"),
  diagnosticsDelay: defaultSetting("diagnostics.delay"),
  arch: defaultSetting("arch"),
  include: defaultSetting("include")
};

/** Reports whether a path is an existing regular file, as the extension does. */
function isFile(candidate) {
  try {
    return statSync(candidate, {throwIfNoEntry: false})?.isFile() === true;
  } catch {
    return false;
  }
}

/** One protocol message, framed as the protocol's base specification frames it. */
function frame(message) {
  const body = Buffer.from(JSON.stringify(message), "utf8");
  return Buffer.concat([Buffer.from(`Content-Length: ${body.length}\r\n\r\n`, "ascii"), body]);
}

/** Settles with the promise, or fails the test if it takes longer than the budget. */
function withTimeout(promise, timeoutMs, message) {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => reject(new Error(message)), timeoutMs);
    timer.unref();
    promise.then(
      (value) => {
        clearTimeout(timer);
        resolve(value);
      },
      (error) => {
        clearTimeout(timer);
        reject(error);
      }
    );
  });
}

