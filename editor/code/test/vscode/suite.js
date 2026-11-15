// The checks that need a VS Code window. `test/vscode/run.mjs` starts the
// window, points the extension at the built server through the
// `jasminIde.server.path` setting, and writes a small Jasmin project into the
// throw-away workspace.
//
// What is checked here is the editor's half of the protocol lifecycle: VS Code
// activates the extension, its language client starts the real `jasmin-lsp`,
// the client reaches the running state (initialize answered and initialized
// sent), a `.jazz` file from the repository fixtures opens as the language,
// the restart command stops that client and starts another one, and that stop
// is a clean shutdown and exit.
//
// The window also runs the two editing-help checks, over the small project:
// completion at a `:` trigger (an item list, empty or not, and no error) and a
// rename (prepareRename on the declared function name returns a range, and a
// rename on the `fn` keyword is refused). Both are requests the client only
// sends for capabilities the server advertises, so a server that advertises
// neither gets an explicit SKIP naming what is missing; the runner prints the
// skips and counts them, so a green run never claims they were measured.
//
// The evidence is the extension's own output channel. The extension, the
// language client and the server's stderr all write to it, and none of them
// can be reached from a test otherwise: the client object is private to the
// extension. Every line the channel receives is recorded, both in memory for
// the assertions below and in `JASMIN_IDE_VSCODE_CAPTURE`, which `run.mjs`
// reads after the window has closed. The server writes its own markers there,
// prefixed `[jasmin-lsp]`: `initialized` when it answers an initialize
// request, `shutdown requested` when it answers a shutdown, and `exiting` when
// it handles the exit notification with a shutdown behind it. The client's
// verbose trace adds the requests it sent and the initialize result it read,
// which is where the editing-help checks take the server's capabilities from.
const assert = require("node:assert/strict");
const { execFile } = require("node:child_process");
const { renameSync, writeFileSync } = require("node:fs");
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

// The editor commands and the client's trace lines the editing-help checks
// use. `vscode.prepareRename` is the command the rename UI runs before it
// offers an input box; the other two are the documented execution commands.
const completionCommand = "vscode.executeCompletionItemProvider";
const prepareRenameCommand = "vscode.prepareRename";
const renameCommand = "vscode.executeDocumentRenameProvider";
const completionRequest = "Sending request 'textDocument/completion";
const prepareRenameRequest = "Sending request 'textDocument/prepareRename";
const renameRequest = "Sending request 'textDocument/rename";

// Where every captured line goes, so that a check after the window has closed
// can still see it. Without the variable the run is a plain window check.
const capturePath = process.env.JASMIN_IDE_VSCODE_CAPTURE;

// The level of the one record that says the suite is over, whatever the
// outcome. `run.mjs` waits for it before it decides anything, so a window that
// closed with the suite still in flight is told apart from one that finished
// and failed: the first has no terminal line, the second has one that carries
// the failure's own words.
const terminalLevel = "terminal";

// The level of the record written just before a wait begins. It names the
// condition the suite was waiting for, so a run that never gets past a wait
// leaves the unfinished condition in the capture instead of only in a message
// the window's stderr swallowed.
const waitingLevel = "waiting";

/** Every line captured so far, in the order the channels received them. */
const captured = [];

/** The capture file as it will be written next, one JSON line per record. */
const captureLines = [];

/**
 * One captured line: recorded for the assertions and written for `run.mjs`.
 *
 * The file is replaced in one step (a staged sibling renamed over it, which
 * `renameSync` does atomically), never appended to. A reader therefore sees
 * either the previous complete file or the next complete file, and can never
 * catch a half-written line: the reader used to append and then split on
 * newlines, so a read that raced a partial write could see a record with no
 * trailing newline and `JSON.parse` it as truncated JSON.
 */
function record(channel, level, text) {
  const entry = {channel, level, text};
  captured.push(entry);
  if (capturePath !== undefined) {
    captureLines.push(`${JSON.stringify(entry)}\n`);
    const staged = `${capturePath}.staged`;
    writeFileSync(staged, captureLines.join(""), "utf8");
    renameSync(staged, capturePath);
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

/**
 * Waits until the predicate holds for the transcript, or fails.
 *
 * The condition's name is recorded before the first poll, so a window that is
 * torn down mid-wait leaves the condition it was waiting for in the capture:
 * the run's log then says what never arrived instead of ending at whatever the
 * last unrelated line happened to be.
 */
async function waitFor(predicate, timeoutMs, message, waitingFor) {
  const deadline = Date.now() + timeoutMs;
  if (waitingFor !== undefined) {
    record("suite", waitingLevel, waitingFor);
  }
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

/** One editing-help check's outcome, recorded for `run.mjs` to print. */
function recordCheck(check, status, detail) {
  record("suite", "m3-check", JSON.stringify({check, status, detail}));
}

/** Records a failing check and fails the window with the same words. */
function failCheck(check, detail) {
  recordCheck(check, "FAIL", detail);
  throw new Error(`${check}: ${detail}`);
}

/** The message of anything thrown. */
function messageOf(error) {
  return error instanceof Error ? error.message : String(error);
}

/**
 * The capabilities the server advertised in its `initialize` reply, read from
 * the client's own verbose trace of that reply. The client registers a
 * feature only for a capability the reply carries, so this record decides
 * whether an editing-help check can be measured at all: no completion
 * provider for `textDocument/completion`, no rename provider for
 * `textDocument/rename`, and no `prepareProvider` for
 * `textDocument/prepareRename`.
 */
function serverCapabilities() {
  const reply = captured.find((entry) => entry.text.includes("Received response 'initialize - ("));
  assert.ok(reply, "the client recorded no initialize reply, so the server's capabilities are unknown");

  const marker = reply.text.indexOf("Result:");
  assert.ok(marker >= 0, "the client's initialize reply carries no result: the run's trace is not verbose");

  let result;
  try {
    result = JSON.parse(reply.text.slice(marker + "Result:".length));
  } catch (error) {
    assert.fail(`the client's initialize reply could not be read as JSON: ${messageOf(error)}`);
  }
  return result.capabilities ?? {};
}

/** The word at a position, or an empty string when there is none. */
function wordAt(document, position) {
  const range = document.getWordRangeAtPosition(position);
  return range === undefined ? "" : document.getText(range);
}

/**
 * The three positions the editing-help checks use, found by their text in the
 * small project's entry file: just past the `:` of a memory access's type
 * annotation (the completion trigger), inside the declared name `load` (the
 * rename target), and inside the `fn` keyword on the same line (the rename
 * that must be refused). Each word is read back, so a project edit that moves
 * the positions fails here instead of testing something else.
 */
function editingHelpPositions(document) {
  const lines = document.getText().split("\n");
  const annotated = lines.findIndex((line) => line.includes(":u32"));
  assert.ok(annotated >= 0, "the editing-help project has no :u32 annotation for the completion check");
  const declared = lines.findIndex((line) => /\bfn\s+load\b/u.test(line));
  assert.ok(declared >= 0, "the editing-help project has no fn load declaration for the rename check");

  const positions = {
    completion: new vscode.Position(annotated, lines[annotated].indexOf(":u32") + 1),
    name: new vscode.Position(declared, lines[declared].indexOf("load") + 1),
    keyword: new vscode.Position(declared, lines[declared].indexOf("fn") + 1)
  };

  assert.equal(wordAt(document, positions.name), "load", "the rename position is not inside the name `load`");
  assert.equal(wordAt(document, positions.keyword), "fn", "the refusal position is not inside the keyword `fn`");
  return positions;
}

/**
 * Completion at the `:` of a memory access's type annotation. The check
 * passes when the editor command returns a list, empty or not, without an
 * error, and the client's trace shows the `textDocument/completion` request
 * it sent to the server: the request is the client passing the editor's ask
 * through, and the list is the server's answer.
 */
async function checkCompletion(document, position, capabilities) {
  const check = "completion";
  if (capabilities.completionProvider === undefined) {
    recordCheck(
      check,
      "SKIP",
      "the server's initialize reply carries no completionProvider, so the client registers no completion provider and sends no textDocument/completion request"
    );
    return;
  }

  const before = count(completionRequest);
  let result;
  try {
    result = await vscode.commands.executeCommand(completionCommand, document.uri, position, ":");
  } catch (error) {
    failCheck(check, `vscode.executeCompletionItemProvider rejected: ${messageOf(error)}`);
  }

  if (count(completionRequest) <= before) {
    failCheck(check, "the command returned but the client sent no textDocument/completion request");
  }

  const items = Array.isArray(result) ? result : result?.items;
  if (!Array.isArray(items)) {
    failCheck(check, `the command returned no completion list: ${JSON.stringify(result)}`);
  }

  recordCheck(
    check,
    "PASS",
    `textDocument/completion answered with ${items.length} item(s) at ${position.line + 1}:${position.character + 1} for the ":" trigger; an empty list would be accepted`
  );
}

/**
 * Rename on the small project: `prepareRename` on the declared function name
 * must return the range of that name, and a rename on the `fn` keyword must
 * be refused with an error rather than an empty edit. Both are the client
 * passing `textDocument/prepareRename` and `textDocument/rename` through; the
 * trace lines it writes are part of the check.
 */
async function checkRename(document, positions, capabilities) {
  const check = "rename";
  const provider = capabilities.renameProvider;
  if (provider === undefined) {
    recordCheck(
      check,
      "SKIP",
      "the server's initialize reply carries no renameProvider, so the client registers no rename provider and sends neither textDocument/prepareRename nor textDocument/rename"
    );
    return;
  }
  if (typeof provider !== "object" || provider === null || provider.prepareProvider !== true) {
    recordCheck(
      check,
      "SKIP",
      "the server's renameProvider does not declare prepareProvider, so the client cannot send textDocument/prepareRename and VS Code falls back to its own word range; the refusal half is not measured either"
    );
    return;
  }

  const beforePrepare = count(prepareRenameRequest);
  let location;
  try {
    location = await vscode.commands.executeCommand(prepareRenameCommand, document.uri, positions.name);
  } catch (error) {
    // A server whose features decline the query refuses with "rename is
    // unavailable" rather than answering with an empty form. That is not a
    // client failure, so it is recorded as a SKIP with the server's own
    // reason; the skip allowlist does not name rename, so the run still
    // fails, and the log says why.
    const reason = messageOf(error);
    if (/rename is unavailable/i.test(reason)) {
      recordCheck(
        check,
        "SKIP",
        `the server advertises renameProvider and refuses every rename, with its own reason: ${reason}`
      );
      return;
    }
    failCheck(check, `vscode.prepareRename rejected at the declaration of "load": ${reason}`);
  }
  if (count(prepareRenameRequest) <= beforePrepare) {
    failCheck(check, "the client sent no textDocument/prepareRename request");
  }

  const range = location?.range;
  if (range === undefined) {
    failCheck(check, `vscode.prepareRename returned no range: ${JSON.stringify(location)}`);
  }
  const covered = document.getText(
    new vscode.Range(range.start.line, range.start.character, range.end.line, range.end.character)
  );
  if (covered !== "load") {
    failCheck(check, `the prepareRename range covers ${JSON.stringify(covered)}, not the declared name "load"`);
  }

  // With prepareProvider the client asks textDocument/prepareRename first, and
  // a server that refuses there is never sent textDocument/rename; either
  // request reaching the server and being refused is the refusal.
  const beforeRename = count(renameRequest) + count(prepareRenameRequest);
  let edit;
  let refusal;
  try {
    edit = await vscode.commands.executeCommand(renameCommand, document.uri, positions.keyword, "renamed");
  } catch (error) {
    refusal = messageOf(error);
  }
  if (count(renameRequest) + count(prepareRenameRequest) <= beforeRename) {
    failCheck(check, "the client sent no textDocument/prepareRename or textDocument/rename request for the keyword");
  }
  if (refusal === undefined) {
    failCheck(check, `renaming the keyword "fn" did not refuse: the command returned ${JSON.stringify(edit)}`);
  }
  if (refusal.trim() === "") {
    failCheck(check, "the server refused the keyword rename with an empty message");
  }

  recordCheck(
    check,
    "PASS",
    `textDocument/prepareRename returned ${range.start.line + 1}:${range.start.character + 1}-${range.end.line + 1}:${range.end.character + 1} for "load"; ` +
      `the rename of the keyword "fn" was refused with ${JSON.stringify(refusal)}`
  );
}

/**
 * The `jasmin-lsp` processes whose parent is this extension host, read from
 * `ps` because the extension keeps its client private. The command line is
 * matched against the path the run configured, so a server built under
 * another name is found too; `-ww` keeps `ps` from cutting the line short.
 *
 * A process is listed only while it exists, so the call is also how the suite
 * tells a server it started from one that is already gone: a restart records
 * the server it started, waits for that server to exit, and only then records
 * the next one. Without that wait the second record could be the first server
 * again, which is how a run came to report one server process while two of
 * them had run.
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

/**
 * Waits until a child of the extension host runs the server under a pid that is
 * not one of `previous`, and returns those processes.
 *
 * This is the condition a restart actually establishes. Waiting for "some
 * server is running" is satisfied the instant it is asked, because the server
 * the restart is replacing can still be in `ps` while it exits — that is how a
 * run came to record the old pid twice and report one server process while two
 * had run. Waiting for "no server is running" is wrong the other way: the
 * replacement may already have started. Only a pid absent from the first set
 * proves the second server, and it is what the restart is being checked for.
 */
async function waitForReplacementServer(serverPath, previous, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  let seen = [];
  for (;;) {
    seen = (await serverProcesses(serverPath)).filter((entry) => !previous.includes(entry.pid));
    if (seen.length > 0) {
      return seen;
    }
    if (Date.now() > deadline) {
      const still = await serverProcesses(serverPath);
      throw new Error(
        `no server under a new pid ran ${serverPath} within ${timeoutMs} ms of the restart; ` +
          `the pid(s) that were running: ${still.map((entry) => entry.pid).join(", ") || "none"}; ` +
          `the pid(s) seen before the restart: ${previous.join(", ") || "none"}`
      );
    }
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
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
  // The `waiting` text deliberately does not quote the marker it waits for:
  // `count` reads every captured line, so quoting a marker in a record the
  // suite writes itself would count as the marker having arrived.
  await waitFor(
    (text) => text.includes(running),
    60000,
    `the extension never reported the language client running; transcript:\n${transcript()}`,
    "the language client reporting it reached the running state"
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
    `the server never reported that it answered an initialize request; transcript:\n${transcript()}`,
    'the server writing its own "[jasmin-lsp] initialized" marker'
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

  // The editing-help checks, over the small project the runner wrote into the
  // workspace. They run on the first client, before the restart below, so the
  // restart keeps its place as the last thing the window does. The server's
  // capabilities come from the initialize reply the client recorded, and the
  // document is waited for: the checks are about requests the client sends
  // for an open document.
  const capabilities = serverCapabilities();
  const folder = vscode.workspace.workspaceFolders?.[0];
  assert.ok(folder, "the window has no workspace folder, so the editing-help project is unreachable");
  const projectPath = join(folder.uri.fsPath, "main.jazz");
  const project = await vscode.workspace.openTextDocument(projectPath);
  assert.equal(project.languageId, languageId, `the editing-help project does not open as ${languageId}`);
  await vscode.window.showTextDocument(project);
  // The condition is the fact itself — the client sent `didOpen` for this one
  // document — not "the didOpen count went up since a moment ago". The count
  // form was the flake: the client can send `didOpen` for the project before
  // the snapshot is taken (the window tells the server about an open document
  // on its own schedule, not between two of the suite's awaits), and then the
  // required increment never comes. The suite then waited its full bound,
  // failed with a message that read like a client bug, and took the run down
  // before the restart — so `exit`, `client exit` and the summary were all
  // absent and only one server process was ever recorded. The fact is monotone
  // and idempotent: an earlier `didOpen` for the same document satisfies it.
  const projectUri = project.uri.toString();
  const didOpen = "Sending notification 'textDocument/didOpen";
  await waitFor(
    () => captured.some((entry) => entry.text.includes(didOpen) && entry.text.includes(projectUri)),
    15000,
    `the client never sent textDocument/didOpen for ${projectPath}`,
    `a textDocument/didOpen notification for ${project.name}`
  );
  record("suite", "state", `opened the editing-help project ${projectPath}`);

  const positions = editingHelpPositions(project);
  await checkCompletion(project, positions.completion, capabilities);
  await checkRename(project, positions, capabilities);

  // The restart command stops the running client and starts another one, so
  // both halves of the lifecycle run once more, both observed. The new server is
  // waited for by pid, not by "a server is running": the server being replaced
  // can still be in `ps` while it exits, so "a server is running" was already
  // true when it was asked — which is how a run recorded the old pid twice and
  // reported one server process while two had run.
  const firstPids = started.map((entry) => entry.pid);
  await vscode.commands.executeCommand("jasminIde.restartServer");
  await waitFor(
    () => count(running) >= 2,
    60000,
    `the restarted language client never reported the running state; transcript:\n${transcript()}`,
    "the language client reporting a second running state, after the restart command"
  );

  const restarted = await waitForReplacementServer(configured, firstPids, 30000);
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

/**
 * Runs the suite and records the terminal line, whatever the outcome.
 *
 * The suite's end and the window's end are two events, and VS Code closes the
 * window once `run` settles. `run.mjs` therefore cannot treat the window's exit
 * as proof that the suite finished: an extension host torn down mid-wait exits
 * without the suite reaching its last line. This wrapper writes the terminal
 * record as the suite's own last act, from its own process, before the promise
 * settles — so a capture with no terminal record is a suite that never finished
 * (and its `waiting` record names what it was waiting for), while a capture
 * with one carries the outcome and the reason in the same file.
 */
async function runAndRecordTerminal() {
  try {
    await run();
    record("suite", terminalLevel, "PASS the suite reached its last check");
  } catch (error) {
    record("suite", terminalLevel, `FAIL the suite stopped early: ${messageOf(error)}`);
    throw error;
  }
}

module.exports = {run: runAndRecordTerminal};
