// The checks that need a VS Code window. `test/vscode/run.mjs` starts the
// window and points the extension at the built server through the
// `jasminIde.server.path` setting.
//
// What is checked here is the editor's half of the protocol lifecycle: VS Code
// activates the extension, its language client starts the real `jasmin-lsp`,
// the client reaches the running state (initialize answered and initialized
// sent), a `.jazz` file from the repository fixtures opens as the language,
// the restart command stops that client and starts another one, and that stop
// is a clean shutdown and exit.
//
// The evidence is the extension's own output channel. The extension, the
// language client and the server's stderr all write to it, and none of them
// can be reached from a test otherwise: the client object is private to the
// extension. Every line the channel receives is recorded, both in memory for
// the assertions below and in `JASMIN_IDE_VSCODE_CAPTURE`, which `run.mjs`
// reads after the window has closed. The server writes its own markers there,
// prefixed `[jasmin-lsp]`: `initialized` when it answers an initialize
// request, `shutdown requested` when it answers a shutdown, and `exiting` when
// it handles the exit notification with a shutdown behind it.
const assert = require("node:assert/strict");
const { execFile } = require("node:child_process");
const { appendFileSync } = require("node:fs");
const { join } = require("node:path");
const { promisify } = require("node:util");
const vscode = require("vscode");

const execute = promisify(execFile);

const extensionId = "nirapod-labs.jasmin-ide";
const languageId = "jasmin-crypto";
const commands = ["jasminIde.restartServer", "jasminIde.showServerLog"];

const running = "The Jasmin language server is running.";
const initialized = "[jasmin-lsp] initialized";
const shutdownRequested = "[jasmin-lsp] shutdown requested";
const exiting = "[jasmin-lsp] exiting";

// Where every captured line goes, so that a check after the window has closed
// can still see it. Without the variable the run is a plain window check.
const capturePath = process.env.JASMIN_IDE_VSCODE_CAPTURE;

/** Every line captured so far, in the order the channels received them. */
const captured = [];

/** One captured line: recorded for the assertions and appended for `run.mjs`. */
function record(channel, level, text) {
  const entry = {channel, level, text};
  captured.push(entry);
  if (capturePath !== undefined) {
    appendFileSync(capturePath, `${JSON.stringify(entry)}\n`, "utf8");
  }
}

/**
 * Wraps every output channel the extension creates, so the lines written to it
 * are recorded. The proxy is handed to the extension as the channel: it
 * forwards every call to the real one, so the window behaves as it would
 * without the capture.
 *
 * The one value it does not forward is `logLevel`, which reads as `trace`.
 * The language client turns message tracing on only when the channel's level
 * is `trace`, and the trace is the client's own record of the messages it
 * sent. The channel's real level stays at whatever VS Code set it to.
 */
function captureOutputChannels() {
  const create = vscode.window.createOutputChannel;

  const createWrapped = function (name, options) {
    const channel = create.call(vscode.window, name, options);
    const proxy = new Proxy(channel, {
      get(target, property) {
        if (property === "logLevel") {
          return vscode.LogLevel.Trace;
        }
        if (property === "onDidChangeLogLevel") {
          return () => ({dispose() {}});
        }
        const value = Reflect.get(target, property, target);
        if (typeof value !== "function") {
          return value;
        }
        return (...args) => {
          const text = args
            .filter((argument) => typeof argument === "string" || typeof argument === "number")
            .map(String)
            .join(" ");
          record(String(name), String(property), text);
          return value.apply(target, args);
        };
      }
    });
    record("suite", "capture", `output channel ${name} captured`);
    return proxy;
  };

  try {
    vscode.window.createOutputChannel = createWrapped;
  } catch (error) {
    record("suite", "capture-failed", String(error));
    throw new Error(`the output channel capture could not be installed: ${String(error)}`);
  }
}

captureOutputChannels();

/** Every captured line joined by newlines, for the marker assertions. */
function transcript() {
  return captured.map((entry) => entry.text).join("\n");
}

/** How many captured lines hold the given text. */
function count(text) {
  return captured.filter((entry) => entry.text.includes(text)).length;
}

/** Waits until the predicate holds for the transcript, or fails. */
async function waitFor(predicate, timeoutMs, message) {
  const deadline = Date.now() + timeoutMs;
  for (;;) {
    if (predicate(transcript())) {
      return;
    }
    if (Date.now() > deadline) {
      throw new Error(`${message} (waited ${timeoutMs} ms)`);
    }
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
}

/**
 * The `jasmin-lsp` processes whose parent is this extension host, read from
 * `ps` because the extension keeps its client private. The command line is
 * matched against the path the run configured, so a server built under
 * another name is found too; `-ww` keeps `ps` from cutting the line short.
 */
async function serverProcesses(serverPath) {
  const { stdout } = await execute("ps", ["-Aww", "-o", "pid,ppid,command"]);
  const rows = stdout.split("\n").slice(1);

  return rows
    .map((row) => /^\s*(\d+)\s+(\d+)\s+(.*)$/u.exec(row))
    .filter((match) => match !== null && Number(match[2]) === process.pid)
    .map((match) => ({pid: Number(match[1]), command: match[3]}))
    .filter((entry) => entry.command.includes(serverPath));
}

/** Waits until the extension host has a server child, and returns it. */
async function waitForServer(serverPath, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  for (;;) {
    const found = await serverProcesses(serverPath);
    if (found.length > 0) {
      return found;
    }
    if (Date.now() > deadline) {
      throw new Error(
        `no child of the extension host (${process.pid}) runs ${serverPath} after ${timeoutMs} ms`
      );
    }
    await new Promise((resolve) => setTimeout(resolve, 250));
  }
}

async function run() {
  const extension = vscode.extensions.getExtension(extensionId);
  assert.ok(extension, `${extensionId} is not in the test window`);
  assert.equal(
    extension.isActive,
    false,
    "the extension was active before the suite ran, so its output channel was never captured"
  );

  await extension.activate();
  assert.equal(extension.isActive, true, "the extension activated but is not active");

  // The language contributes and the commands, read the way the API returns
  // them: `getLanguages` is a plain array today and was a thenable before, so
  // both are accepted.
  const languages = Array.from(await vscode.languages.getLanguages());
  assert.ok(languages.includes(languageId), `the ${languageId} language is not registered`);

  const registered = await vscode.commands.getCommands(true);
  for (const command of commands) {
    assert.ok(registered.includes(command), `the ${command} command is not registered`);
  }

  const configured = vscode.workspace.getConfiguration("jasminIde").get("server.path");
  assert.ok(typeof configured === "string" && configured !== "", "the run is not configured with a server path");

  // The extension logs this line only after `LanguageClient.start()` resolved,
  // which is after the initialize reply and after the initialized
  // notification. A client that failed either step logs the failure instead
  // and never this line.
  await waitFor(
    (text) => text.includes(running),
    60000,
    `the extension never reported the language client running; transcript:\n${transcript()}`
  );
  record("suite", "state", "the language client reported the running state");

  const started = await waitForServer(configured, 30000);
  assert.ok(started.length > 0, `the client did not start ${configured}`);
  for (const entry of started) {
    record("suite", "server-pid", `pid ${entry.pid}`);
  }

  // The protocol's own markers, from the server's stderr, which the client
  // pipes into the same output channel.
  await waitFor(
    (text) => text.includes(initialized),
    30000,
    `the server never reported that it answered an initialize request; transcript:\n${transcript()}`
  );

  // A `.jazz` file from the repository's fixtures, opened the way a person
  // opens one: the client selects it and sends it to the server.
  const fixture = join(__dirname, "..", "..", "fixtures", "rotate-columns.jazz");
  const document = await vscode.workspace.openTextDocument(fixture);
  assert.equal(document.languageId, languageId, `a .jazz file does not open as ${languageId}`);
  await vscode.window.showTextDocument(document);
  record("suite", "state", `opened ${fixture} as ${document.languageId}`);

  // A settings change makes the client re-read its trace setting, so the
  // client's own record of the messages it sends covers the stop below.
  await vscode.workspace
    .getConfiguration("jasminIde")
    .update("diagnostics.delay", 400, vscode.ConfigurationTarget.Workspace);

  // The restart command stops the running client and starts another one, so
  // both halves of the lifecycle run once more, both observed.
  await vscode.commands.executeCommand("jasminIde.restartServer");
  await waitFor(
    () => count(running) >= 2,
    60000,
    `the restarted language client never reported the running state; transcript:\n${transcript()}`
  );

  const restarted = await waitForServer(configured, 30000);
  assert.ok(restarted.length > 0, "the server is not running after the restart command");
  for (const entry of restarted) {
    record("suite", "server-pid", `pid ${entry.pid}`);
  }

  const text = transcript();
  const occurrences = {
    initialized: count(initialized),
    running: count(running),
    shutdown: count(shutdownRequested),
    exiting: count(exiting)
  };

  assert.ok(
    occurrences.initialized >= 2,
    `the server answered ${occurrences.initialized} initialize request(s), expected 2 (one per client)`
  );
  assert.ok(occurrences.shutdown >= 1, `the server was never asked to shut down; transcript:\n${text}`);
  assert.ok(occurrences.exiting >= 1, `the server was never sent an exit notification; transcript:\n${text}`);

  // The order is what makes the pair a lifecycle rather than two coincidences:
  // the first initialize came before the first running report, and the shutdown
  // came before the exit. A shutdown ahead of the exit is also what makes the
  // server's own exit code 0 rather than 1.
  const order = captured.map((entry) => entry.text);
  const first = (needle) => order.findIndex((line) => line.includes(needle));
  assert.ok(
    first(initialized) < first(running),
    "the server's initialize marker does not come before the client's running report"
  );
  assert.ok(
    first(shutdownRequested) >= 0 && first(shutdownRequested) < first(exiting),
    "the exit notification does not come after a shutdown request"
  );

  const summary = {
    initialized: occurrences.initialized,
    running: occurrences.running,
    shutdown: occurrences.shutdown,
    exiting: occurrences.exiting,
    restart: restarted.length
  };
  record("suite", "summary", JSON.stringify(summary));
}

module.exports = {run};
