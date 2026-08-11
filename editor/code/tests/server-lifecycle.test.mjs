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

/**
 * Reads protocol frames from a stream. A frame is one header block, ended by a
 * blank line, naming its body's length in bytes, followed by exactly that many
 * bytes. Anything else is a failure, so a stray log line on stdout is caught
 * rather than ignored.
 */
function frameReader(stream) {
  let buffered = Buffer.alloc(0);
  const arrived = [];
  const waiting = [];
  let failure;
  let ended = false;

  function settle() {
    while (waiting.length > 0 && (arrived.length > 0 || failure !== undefined || ended)) {
      const waiter = waiting.shift();
      if (arrived.length > 0) {
        waiter.resolve(arrived.shift());
      } else {
        waiter.reject(failure ?? new Error("the server closed its output before the frame arrived"));
      }
    }
  }

  function take() {
    for (;;) {
      const blank = buffered.indexOf("\r\n\r\n");
      if (blank < 0) {
        return;
      }
      const header = buffered.subarray(0, blank).toString("ascii");
      const named = /^Content-Length: (\d+)$/u.exec(header);
      if (named === null) {
        failure = new Error(`stdout carries a frame with no usable header: ${JSON.stringify(header)}`);
        return;
      }
      const length = Number(named[1]);
      const start = blank + 4;
      if (buffered.length < start + length) {
        return;
      }
      const body = buffered.subarray(start, start + length).toString("utf8");
      buffered = buffered.subarray(start + length);
      try {
        arrived.push(JSON.parse(body));
      } catch {
        failure = new Error(`stdout carries a frame whose body is not JSON: ${JSON.stringify(body)}`);
        return;
      }
    }
  }

  stream.on("data", (chunk) => {
    buffered = Buffer.concat([buffered, chunk]);
    take();
    settle();
  });
  stream.on("end", () => {
    ended = true;
    settle();
  });

  return {
    /** The next frame, or a failed test if none arrives in time. */
    next(what, timeoutMs = 15000) {
      if (failure !== undefined) {
        return Promise.reject(failure);
      }
      if (arrived.length > 0) {
        return Promise.resolve(arrived.shift());
      }
      return new Promise((resolve, reject) => {
        const timer = setTimeout(() => {
          reject(new Error(`timed out after ${timeoutMs} ms waiting for ${what}`));
        }, timeoutMs);
        waiting.push({
          resolve: (value) => {
            clearTimeout(timer);
            resolve(value);
          },
          reject: (error) => {
            clearTimeout(timer);
            reject(error);
          }
        });
        settle();
      });
    },
    /** The bytes left over when the stream ended must be none. */
    leftovers() {
      return buffered.length;
    }
  };
}

test("the manifest's defaults resolve to the built server and an empty argument list", () => {
  const resolution = resolveServerPath({
    settingPath: settings.serverPath,
    extensionPath: editorDirectory,
    // The setting is the first place the lookup tries; the bundled copy and
    // `PATH` are covered by the lookup's own tests.
    pathLookup: () => undefined,
    isFile
  });

  assert.equal(
    resolution.kind,
    "found",
    `no server executable at ${settings.serverPath}: build it (make build) or set JASMIN_LSP`
  );
  assert.equal(resolution.source, "setting");

  const launch = planServerLaunch(resolution.path, settings);
  assert.equal(launch.command, settings.serverPath);
  assert.deepEqual(
    [...launch.args],
    [],
    "the client passes the server.args setting, and its default is no arguments"
  );
  assert.equal(launch.transport, "stdio");
  assert.deepEqual(Object.keys(launch.initializationOptions).sort(), [
    "arch",
    "diagnosticsDelay",
    "include"
  ]);
  assert.equal(launch.initializationOptions.arch, "x86-64");
  assert.equal(launch.initializationOptions.diagnosticsDelay, 250);
  assert.deepEqual(launch.initializationOptions.include, {});
});

