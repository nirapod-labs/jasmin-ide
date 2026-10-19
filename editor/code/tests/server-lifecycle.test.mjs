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
 * them. That half needs a window and is in `test/vscode/suite.js`.
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
    "diagnostics",
    "include"
  ]);
  assert.equal(launch.initializationOptions.arch, "x86-64");
  assert.deepEqual(launch.initializationOptions.diagnostics, {delay: 250});
  assert.deepEqual(launch.initializationOptions.include, {});
});

test("the server the client starts completes initialize, initialized, shutdown and exit", {timeout: 60000}, async () => {
  const resolution = resolveServerPath({
    settingPath: settings.serverPath,
    extensionPath: editorDirectory,
    pathLookup: () => undefined,
    isFile
  });
  assert.equal(
    resolution.kind,
    "found",
    `no server executable at ${settings.serverPath}: build it (make build) or set JASMIN_LSP`
  );

  const launch = planServerLaunch(resolution.path, settings);
  const child = spawn(launch.command, [...launch.args], {stdio: ["pipe", "pipe", "pipe"]});

  const frames = frameReader(child.stdout);
  let errors = "";
  child.stderr.setEncoding("utf8");
  child.stderr.on("data", (chunk) => {
    errors += chunk;
  });

  const exited = new Promise((resolve) => {
    // `close` rather than `exit`: every byte of output has been delivered by
    // then, so the frame count below is the whole session.
    child.on("close", (code, signal) => resolve({code, signal}));
  });

  try {
    // `initialize`: the four values the plan builds, plus the fields every
    // client sends. The capabilities are the editor's, not this extension's,
    // so an empty set stands in for them here.
    child.stdin.write(
      frame({
        jsonrpc: "2.0",
        id: 1,
        method: "initialize",
        params: {
          processId: process.pid,
          clientInfo: {name: "Jasmin IDE", version: manifest.version},
          rootUri: null,
          capabilities: {},
          initializationOptions: launch.initializationOptions
        }
      })
    );

    const initialized = await frames.next("the reply to initialize");
    assert.equal(initialized.id, 1, "the reply to initialize does not carry its id");
    assert.equal(initialized.error, undefined, `initialize failed: ${JSON.stringify(initialized.error)}`);
    assert.ok(initialized.result, "the reply to initialize carries no result");
    assert.equal(initialized.result.capabilities.textDocumentSync, 1, "the server does not offer full sync");
    assert.ok(initialized.result.serverInfo, "the reply carries no serverInfo");
    assert.equal(initialized.result.serverInfo.name, "jasmin-lsp");

    // `initialized` is a notification: the server must answer nothing. The
    // next frame this test reads is the reply to shutdown, and its id proves
    // no reply to the notification slipped in before it.
    child.stdin.write(frame({jsonrpc: "2.0", method: "initialized", params: {}}));

    // `shutdown`: a request, answered with a null result.
    child.stdin.write(frame({jsonrpc: "2.0", id: 2, method: "shutdown"}));

    const shutdown = await frames.next("the reply to shutdown");
    assert.equal(shutdown.id, 2, "the server answered a notification, or answered shutdown out of order");
    assert.equal(shutdown.error, undefined, `shutdown failed: ${JSON.stringify(shutdown.error)}`);
    assert.equal(shutdown.result, null, "a shutdown reply carries a null result");

    // `exit`: a notification, after which the process ends with the code the
    // protocol gives a server that was shut down properly.
    child.stdin.write(frame({jsonrpc: "2.0", method: "exit"}));

    const ending = await withTimeout(exited, 15000, "the server did not exit after exit");
    assert.equal(ending.signal, null, `the server was killed by ${ending.signal}`);
    assert.equal(ending.code, 0, `the server exited with ${ending.code}; stderr: ${errors}`);

    assert.equal(frames.leftovers(), 0, "stdout carries bytes after the last frame");
    assert.ok(
      errors.includes("initialized"),
      `the server logged no initialization; stderr: ${JSON.stringify(errors)}`
    );
  } finally {
    child.kill("SIGKILL");
  }
});
