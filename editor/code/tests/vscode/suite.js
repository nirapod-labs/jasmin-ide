// The checks that need a VS Code window. `tests/vscode/run.mjs` starts the
// window and points the extension at the built server through the
// `jasminIde.server.path` setting.
//
// What is checked here is the editor's half of the start: the extension
// activates, the language and the two commands are registered, a `.jazz` file
// opens as the language, the client starts a `jasmin-lsp` child process of this
// extension host, and the restart command stops that process and starts
// another one. Feature-level checks (a diagnostic for a file that does not
// parse) need the analysis engine, which the server does not have yet.
const assert = require("node:assert/strict");
const { execFile } = require("node:child_process");
const { mkdtemp, rm, writeFile } = require("node:fs/promises");
const { tmpdir } = require("node:os");
const { join } = require("node:path");
const { promisify } = require("node:util");
const vscode = require("vscode");

const execute = promisify(execFile);

const extensionId = "nirapod-labs.jasmin-ide";
const languageId = "jasmin-crypto";
const commands = ["jasminIde.restartServer", "jasminIde.showServerLog"];
const brokenSource = "fn broken( -> { return }\n";

/**
 * The `jasmin-lsp` processes whose parent is this extension host, read from
 * `ps` because the extension keeps its client private.
 */
async function serverProcesses() {
  const { stdout } = await execute("ps", ["-Ao", "pid,ppid,command"]);
  const rows = stdout.split("\n").slice(1);

  return rows
    .map((row) => /^\s*(\d+)\s+(\d+)\s+(.*)$/u.exec(row))
    .filter((match) => match !== null && Number(match[2]) === process.pid)
    .map((match) => ({pid: Number(match[1]), command: match[3]}))
    .filter((entry) => entry.command.includes("jasmin-lsp"));
}

/** Waits until the extension host has a server child, and returns it. */
async function waitForServer(timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  for (;;) {
    const found = await serverProcesses();
    if (found.length > 0) {
      return found;
    }
    if (Date.now() > deadline) {
      throw new Error(`no jasmin-lsp child of the extension host (${process.pid}) appeared in ${timeoutMs} ms`);
    }
    await new Promise((resolve) => setTimeout(resolve, 250));
  }
}

async function run() {
  const extension = vscode.extensions.getExtension(extensionId);
  assert.ok(extension, `${extensionId} is not in the test window`);

  await extension.activate();
  assert.equal(extension.isActive, true, "the extension activated but is not active");

  assert.ok(vscode.languages.getLanguages().includes(languageId), `the ${languageId} language is not registered`);

  const registered = await vscode.commands.getCommands(true);
  for (const command of commands) {
    assert.ok(registered.includes(command), `the ${command} command is not registered`);
  }

  const configured = vscode.workspace.getConfiguration("jasminIde").get("server.path");
  assert.ok(typeof configured === "string" && configured !== "", "the run is not configured with a server path");

  const started = await waitForServer(30000);
  assert.ok(
    started.some((entry) => entry.command.includes(configured)),
    `the client started ${started.map((entry) => entry.command).join(", ")} instead of ${configured}`
  );

  const directory = await mkdtemp(join(tmpdir(), "jasmin-ide-vscode-suite-"));
  try {
    const path = join(directory, "broken.jazz");
    await writeFile(path, brokenSource, "utf8");
    const document = await vscode.workspace.openTextDocument(path);
    assert.equal(document.languageId, languageId, `a .jazz file does not open as ${languageId}`);
    await vscode.window.showTextDocument(document);
  } finally {
    await rm(directory, {recursive: true, force: true});
  }

  // The restart command stops the running client and starts another one, so
  // a server is present again once it resolves.
  await vscode.commands.executeCommand("jasminIde.restartServer");
  const restarted = await waitForServer(30000);
  assert.ok(restarted.length > 0, "the server is not running after the restart command");
}

module.exports = {run};
