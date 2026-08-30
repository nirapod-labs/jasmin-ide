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

