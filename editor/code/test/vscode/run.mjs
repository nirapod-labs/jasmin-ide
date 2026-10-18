// Runs the extension inside a real VS Code window and checks that the window
// itself starts the server and completes the protocol lifecycle: initialize,
// initialized, shutdown and exit.
//
// This is the only check that exercises the editor's half of the start path:
// the `LanguageClient` object, the client capabilities VS Code supplies, and
// the window's own teardown of the extension. It starts a window, so it is not
// part of `pnpm run validate`: `pnpm run test:vscode` runs it by hand and
// `make editor-vscode` runs it for the gate. The first run downloads a VS Code
// build into `.vscode-test/` beside this extension (gitignored), which is the
// build-and-test download the gate allows; a host where that is impossible
// fails the gate step unless the caller allows that skip by name
// (`JASMIN_ALLOWED_SKIPS`), so a gate log never reads green over a window that
// did not open.
//
// `VSCODE_TEST_CACHE` names another directory for that download, and the lab
// sets it to one shared directory, so that every worktree reads the same VS
// Code build instead of downloading its own copy into `.vscode-test/`. Unset,
// the cache is `.vscode-test/` beside this extension, as it always was.
//
// The window records everything the extension writes to its output channel
// into the capture file named by `JASMIN_IDE_VSCODE_CAPTURE`, and the checks
// after the window has closed read it back: a window that dies before its
// deactivation still leaves the record of what it did.
//
// Beside the lifecycle, the window runs the editing-help checks over a small
// project written into the same throw-away workspace: completion at a `:`
// trigger and a rename (prepareRename on a function name, then a refused
// rename on a keyword). The suite decides each check from the capabilities
// the client recorded in the server's initialize reply. A server that does
// not advertise a capability is not asked for it, so the check is recorded as
// SKIP with the missing capability named.
//
// A skip is a measurement that did not happen, so it cannot by itself end in
// a green run: every skip is matched against the allowlist in `skips.mjs`,
// which names the skips this tree expects today and why, and any other skip
// fails the run. An allowed skip is printed with its reason, and the summary
// line counts it, so a PASS says exactly which checks it measured.
import { runTests } from "@vscode/test-electron";
import { existsSync } from "node:fs";
import { mkdir, mkdtemp, readFile, rm, stat, writeFile } from "node:fs/promises";
import { spawnSync } from "node:child_process";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { allowedSkips, expectedChecks, judgeEditingHelp } from "./skips.mjs";

const editorDirectory = fileURLToPath(new URL("../..", import.meta.url));
const repositoryRoot = fileURLToPath(new URL("../../../..", import.meta.url));
const builtServer = join(repositoryRoot, "_build", "default", "lsp-server", "native", "main.exe");
const serverPath = process.env.JASMIN_LSP ?? builtServer;

// A fixed VS Code release, so that a gate log names the editor it was measured
// with. `JASMIN_IDE_VSCODE_VERSION` overrides it for a one-off run.
const vscodeVersion = process.env.JASMIN_IDE_VSCODE_VERSION ?? "1.145.2";

try {
  await stat(serverPath);
} catch {
  console.error(`no server executable at ${serverPath}: build it (make build) or set JASMIN_LSP`);
  process.exit(1);
}

// The window loads `main` from the manifest, which is the bundle. A tree that
// was never bundled opens its window with an extension host that cannot load
// it, so say which command is missing before spending a launch on it.
const bundle = join(editorDirectory, "dist", "extension.js");
if (!existsSync(bundle)) {
  console.error(`no extension bundle at ${bundle}: run pnpm run bundle first (pnpm run test:vscode does)`);
  process.exit(1);
}

// A throw-away workspace whose settings point the extension at the built
// server: the same setting a user sets, and the first place the lookup tries.
const workspace = await mkdtemp(join(tmpdir(), "jasmin-ide-vscode-"));
const capturePath = join(workspace, "capture.jsonl");
await mkdir(join(workspace, ".vscode"), {recursive: true});
await writeFile(
  join(workspace, ".vscode", "settings.json"),
  `${JSON.stringify(
    {
      "jasminIde.server.path": serverPath,
      "jasminIde.trace.server": "verbose"
    },
    null,
    2
  )}\n`,
  "utf8"
);

// The small project the editing-help checks run over: one entry file, one
// library it requires, and a project file that names the entry. The entry
// holds the three positions the suite finds by their text: the `:` of a
// memory access's type annotation (the completion trigger), the declared
// name `load` (the rename target), and the `fn` keyword on that line (the
// rename that must be refused).
const entrySource = `require "lib.jinc"

export
fn load(reg ptr u32[4] source) -> reg u32 {
  reg u32 value;
  value = [#aligned :u32 source + 0];
  value = twice(value);
  return value;
}
`;

const librarySource = `fn twice(reg u32 value) -> reg u32 {
  return value + value;
}
`;

const projectSource = `${JSON.stringify({version: 1, arch: "x86-64", entries: ["main.jazz"]}, null, 2)}\n`;

await writeFile(join(workspace, "jasmin-project.json"), projectSource, "utf8");
await writeFile(join(workspace, "main.jazz"), entrySource, "utf8");
await writeFile(join(workspace, "lib.jinc"), librarySource, "utf8");

// The path is printed from this variable after the window, when the
// throw-away workspace itself is already gone.
const projectEntry = join(workspace, "main.jazz");

let code;
let failure;
try {
  code = await runTests({
    version: vscodeVersion,
    cachePath: process.env.VSCODE_TEST_CACHE || join(editorDirectory, ".vscode-test"),
    extensionDevelopmentPath: editorDirectory,
    extensionTestsPath: fileURLToPath(new URL("./suite.js", import.meta.url)),
    extensionTestsEnv: {JASMIN_IDE_VSCODE_CAPTURE: capturePath},
    launchArgs: [
      workspace,
      "--disable-extensions",
      "--user-data-dir",
      join(workspace, "user-data"),
      "--extensions-dir",
      join(workspace, "extensions")
    ]
  });
} catch (error) {
  failure = error;
}

// The window is gone by now, but its record is not necessarily complete: the
// window's exit and the suite's own end are two events, and the suite's end is
// the one that decides. A window whose extension host was torn down while the
// suite was still waiting leaves a capture with no terminal record, and reading
// it once at that moment reported every step after the teardown as a failure of
// the code under test. So the read waits for the terminal record the suite
// writes as its last act, whatever the outcome, and gives up only after a
// bound. The bound is not a fixed sleep: the read returns the moment the record
// is there, and on timeout the message names the record that never arrived.
const terminalWaitMs = 30000;

/** One complete read of the capture file the suite keeps replacing atomically. */
async function readCapture(path) {
  if (!existsSync(path)) {
    return {records: [], malformed: []};
  }
  const records = [];
  const malformed = [];
  for (const line of (await readFile(path, "utf8")).split("\n")) {
    if (line.trim() === "") {
      continue;
    }
    try {
      records.push(JSON.parse(line));
    } catch (error) {
      malformed.push(`${line.slice(0, 120)} (${error instanceof Error ? error.message : String(error)})`);
    }
  }
  return {records, malformed};
}

/** Waits until the capture holds a terminal record, or the bound passes. */
async function waitForTerminal(path, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  for (;;) {
    const read = await readCapture(path);
    const terminal = read.records.find((entry) => entry.level === "terminal");
    if (terminal !== undefined) {
      return {...read, terminal};
    }
    if (Date.now() > deadline) {
      return {...read, terminal: undefined};
    }
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
}

const capture = await waitForTerminal(capturePath, terminalWaitMs);
const captured = capture.records;

await rm(workspace, {recursive: true, force: true});

const texts = captured.map((entry) => entry.text);
const count = (needle) => texts.filter((text) => text.includes(needle)).length;
const firstAt = (needle) => texts.findIndex((text) => text.includes(needle));

// The editing-help checks the suite recorded in the window, whatever their
// outcome. A check that could not be measured is recorded as SKIP with its
// reason, so a skip is named in this log instead of passing as an unmentioned
// gap; the expected count and the skips a green run may carry are in
// `skips.mjs`, and fewer records mean the window never got to them.
const editingHelp = captured
  .filter((entry) => entry.channel === "suite" && entry.level === "m3-check")
  .map((entry) => JSON.parse(entry.text));

// The client's own message log, one line per message it sent. The window turns
// it on for the checks below, so it is the client's account of the same four
// steps the server reported. The strings are exact: `initialize` also prefixes
// `initialized`, so a loose match would count one for the other.
const messages = {
  initialize: "Sending request 'initialize - (",
  initialized: "Sending notification 'initialized'",
  shutdown: "Sending request 'shutdown",
  exit: "Sending notification 'exit"
};
const sent = (method) => count(messages[method]);

// The summary the suite records after the restart, so the log carries the
// counts the window observed in one line a reader can compare across runs.
const summary = captured.find((entry) => entry.level === "summary");

// The four lifecycle steps, each with the observation that proves it. The
// server writes its own markers to stderr and the client pipes them into the
// extension's output channel, so a marker in this list is a message the server
// actually handled with the client VS Code started on the other end.
//
// Each check also names the line it looked for, so a FAIL prints the terminal
// line that never arrived rather than only the property that was hoped for. A
// reader of the gate log does not have the capture file, so the name of the
// missing line is the whole diagnosis.
const checks = [
  {
    step: "initialize",
    detail: "the server answered an initialize request",
    missing: 'the server line "[jasmin-lsp] initialized", which it writes when it answers an initialize request',
    ok: count("[jasmin-lsp] initialized") >= 1
  },
  {
    step: "initialized",
    detail: "the client reached the running state, which it reports only after sending the initialized notification",
    missing: 'the client line "The Jasmin language server is running.", which it logs after the initialized notification',
    ok: count("The Jasmin language server is running.") >= 1
  },
  {
    step: "shutdown",
    detail: "the server answered a shutdown request",
    missing: 'the server line "[jasmin-lsp] shutdown requested", which it writes when it answers a shutdown',
    ok: count("[jasmin-lsp] shutdown requested") >= 1
  },
  {
    step: "exit",
    detail: "the server handled the exit notification after a shutdown",
    missing:
      'the server line "[jasmin-lsp] exiting" after "[jasmin-lsp] shutdown requested"; the server writes it only ' +
      "when it handles the exit notification",
    ok:
      count("[jasmin-lsp] exiting") >= 1 &&
      firstAt("[jasmin-lsp] shutdown requested") < firstAt("[jasmin-lsp] exiting")
  },
  {
    step: "client initialize",
    detail: "the client logged the initialize request it sent",
    missing: `the client trace line "${messages.initialize}", which it writes when it sends the initialize request`,
    ok: sent("initialize") >= 1
  },
  {
    step: "client initialized",
    detail: "the client logged the initialized notification it sent",
    missing: `the client trace line "${messages.initialized}", which it writes when it sends the initialized notification`,
    ok: sent("initialized") >= 1
  },
  {
    step: "client shutdown",
    detail: "the client logged the shutdown request it sent",
    missing: `the client trace line "${messages.shutdown}", which it writes when it sends the shutdown request`,
    ok: sent("shutdown") >= 1
  },
  {
    step: "client exit",
    detail: "the client logged the exit notification it sent",
    missing: `the client trace line "${messages.exit}", which it writes when it sends the exit notification`,
    ok: sent("exit") >= 1
  },
  {
    step: "suite summary",
    detail: "the suite recorded the lifecycle counts it observed",
    missing:
      'the suite line "[suite/summary]", which it records after the restart; without it the suite never reached the ' +
      "restart, so the whole second half of the lifecycle is unmeasured",
    ok: summary !== undefined
  }
];

const serverPids = captured
  .filter((entry) => entry.level === "server-pid")
  .map((entry) => Number(/pid (\d+)/u.exec(entry.text)?.[1]))
  .filter((pid) => Number.isInteger(pid));
const survivors = serverPids.filter((pid) => {
  const alive = spawnSync("ps", ["-o", "command=", "-p", String(pid)], {encoding: "utf8"}).stdout.trim();
  return alive.includes(serverPath);
});

const passed = checks.filter((check) => check.ok).length;

// Which skips this run carries, and whether each one was expected. A skip is
// a measurement that did not happen, so the run cannot pass on one the
// allowlist does not name (`skips.mjs`); an allowed one is printed with its
// reason and counted in the summary.
const {
  ok: editingHelpOk,
  skipped: skippedChecks,
  failed: failedChecks,
  unexpected: unexpectedSkips
} = judgeEditingHelp(editingHelp);

console.log(`vscode-test: VS Code ${vscodeVersion}, server ${serverPath}`);
console.log(`vscode-test: editing-help project ${projectEntry}`);
console.log(
  `vscode-test: window exit code ${code ?? "none"}, ${captured.length} captured line(s), ` +
    `${serverPids.length} server process(es) seen`
);
if (summary !== undefined) {
  console.log(`vscode-test: suite summary ${summary.text}`);
}

// The terminal record the suite writes as its own last act, and the condition
// it had begun to wait for when it never got there. A run that finished prints
// only the outcome; a run that did not prints which condition was outstanding,
// so a failing log says by itself what the window was waiting for instead of
// making a reader infer it from the last unrelated captured line.
const terminal = capture.terminal;
const lastWaiting = [...captured].reverse().find((entry) => entry.level === "waiting");
const suiteFinished = terminal !== undefined && terminal.text.startsWith("PASS");

if (terminal !== undefined) {
  console.log(`vscode-test: suite ended: ${terminal.text}`);
} else {
  console.log(
    `vscode-test: FAIL suite did not finish: the capture holds no terminal line after ` +
      `${terminalWaitMs} ms, so the suite never wrote its last record: the window was torn down ` +
      `while the suite was still running`
  );
}
if (!suiteFinished && lastWaiting !== undefined) {
  console.log(`vscode-test: FAIL suite never got: ${lastWaiting.text}`);
}
for (const line of capture.malformed) {
  console.log(`vscode-test: FAIL capture line is not JSON: ${line}`);
}

for (const check of checks) {
  console.log(`vscode-test: ${check.ok ? "PASS" : "FAIL"} ${check.step}: ${check.detail}`);
  if (!check.ok) {
    console.log(`vscode-test:   never arrived: ${check.missing}`);
  }
}
for (const check of editingHelp) {
  console.log(`vscode-test: ${check.status} ${check.check}: ${check.detail}`);
  if (check.status === "SKIP") {
    // The reason belongs in the log: a reader of the gate's output has only
    // this line to tell an expected skip from a check that stopped happening.
    const reason = allowedSkips.get(check.check);
    console.log(
      reason === undefined
        ? `vscode-test: FAIL skip ${check.check}: the skip allowlist does not name this check, so the run cannot ` +
            "pass on a measurement it did not make"
        : `vscode-test:   allowed skip: ${reason}`
    );
  }
}
if (editingHelp.length === 0) {
  console.log("vscode-test: FAIL editing-help: the window recorded neither of the two checks");
  console.log(
    "vscode-test:   never arrived: a [suite/m3-check] record for each of completion and rename, written when " +
      "the suite reaches them; none means the suite stopped before the editing-help project was open"
  );
} else if (editingHelp.length !== expectedChecks) {
  console.log(
    `vscode-test: FAIL editing-help: the window recorded ${editingHelp.length} check(s), expected ${expectedChecks}`
  );
}
console.log(
  `vscode-test: editing-help checks ${editingHelp.length - skippedChecks.length} ran, ` +
    `${skippedChecks.length} skipped (${skippedChecks.length - unexpectedSkips.length} allowed), ` +
    `${failedChecks.length} failed`
);
if (serverPids.length === 0) {
  console.log("vscode-test: FAIL exit: no server process was seen inside the window");
}
if (survivors.length > 0) {
  console.log(`vscode-test: FAIL exit: server process(es) ${survivors.join(", ")} survived the window`);
}
// The client's own message trace, when the client wrote one.
for (const method of ["initialize", "initialized", "shutdown", "exit"]) {
  const traced = sent(method);
  if (traced > 0) {
    console.log(`vscode-test: trace ${method} x${traced} (the client's own message log)`);
  }
}

const ok =
  passed === checks.length &&
  code === 0 &&
  failure === undefined &&
  terminal !== undefined &&
  terminal.text.startsWith("PASS") &&
  capture.malformed.length === 0 &&
  serverPids.length > 0 &&
  survivors.length === 0 &&
  editingHelpOk;

console.log(`vscode-test: ${passed}/${checks.length} lifecycle checks observed`);
if (skippedChecks.length > 0) {
  const names = skippedChecks.map((check) => check.check).join(", ");
  console.log(
    `vscode-test: ${skippedChecks.length} editing-help check(s) skipped, not measured: ${names}; ` +
      `the server does not advertise what they need (each SKIP line names it, and an allowed one carries its reason)`
  );
}
console.log(`vscode-test: ${ok ? "PASS" : "FAIL"}`);

if (failure !== undefined) {
  // The window's own exit code and the suite's terminal line are separate
  // events, so a reader needs both to tell "the suite failed" from "the window
  // closed under the suite". The second half names which of the two happened,
  // and when the suite never finished, the condition it was still waiting for.
  const suiteEnd =
    terminal === undefined
      ? "the suite never finished: it wrote no terminal line, and the last condition it waited for was " +
        `${lastWaiting === undefined ? "not recorded" : JSON.stringify(lastWaiting.text)}`
      : `the suite's own last line was ${JSON.stringify(terminal.text)}`;
  console.error(
    `vscode-test: the window did not complete: ${failure instanceof Error ? failure.message : String(failure)}; ` +
      `${suiteEnd}`
  );
}

// The raw record, whatever the verdict: the gate keeps the log, and a reader
// must be able to check the lines the checks above were made from.
if (captured.length === 0) {
  console.log("vscode-test: the window captured nothing, so no lifecycle step can be shown");
} else {
  console.log("vscode-test: the captured transcript follows");
  for (const entry of captured) {
    console.log(`  [${entry.channel}/${entry.level}] ${entry.text}`);
  }
}

process.exitCode = ok ? 0 : 1;
