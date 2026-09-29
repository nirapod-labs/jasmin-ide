const assert = require("node:assert/strict");
const path = require("node:path");
const vscode = require("vscode");

const extensionId = "nirapod-labs.jasmin-ide";
const entryName = "main.jazz";
const libraryName = "lib.jinc";

const expectedDiagnostic = /unknown function: `missing'/;
const expectedDiagnosticLine = 6;
const expectedDiagnosticColumn = 10;
const expectedDiagnosticEndColumn = 17;

const definitionLine = 5;
const definitionColumn = 11;

const waitMilliseconds = 120000;

const prefix = "[clean-install]";
const lines = [];

/** Prints a line and keeps it, so a failure can print the whole transcript. */
function line(text) {
  const rendered = `${prefix} ${text}`;
  lines.push(rendered);
  console.log(rendered);
}

function sleep(milliseconds) {
  return new Promise((resolve) => setTimeout(resolve, milliseconds));
}

/** Waits for a probe to satisfy `accept`, or throws naming what never came. */
async function waitFor(description, probe, accept) {
  const deadline = Date.now() + waitMilliseconds;
  let last;
  for (;;) {
    last = await probe();
    if (accept(last)) {
      return last;
    }
    if (Date.now() > deadline) {
      throw new Error(
        `${description} did not arrive within ${waitMilliseconds} ms; the last answer was ${JSON.stringify(last)}`
      );
    }
    await sleep(200);
  }
}

/** The one-based `line:column` of a position, for a reader of the log. */
function where(position) {
  return `${position.line + 1}:${position.character + 1}`;
}

/** The first target of a definition answer, whether a location or a link. */
function firstTarget(result) {
  if (!Array.isArray(result) || result.length === 0) {
    return undefined;
  }
  const entry = result[0];
  if (entry === null || typeof entry !== "object") {
    return undefined;
  }
  if (entry.targetUri !== undefined) {
    return {uri: entry.targetUri, range: entry.targetRange ?? entry.targetSelectionRange};
  }
  if (entry.uri !== undefined) {
    return {uri: entry.uri, range: entry.range};
  }
  return undefined;
}

exports.run = async function run() {
  const workspace = process.env.JASMIN_IDE_CLEAN_INSTALL_WORKSPACE;
  const installed = process.env.JASMIN_IDE_CLEAN_INSTALL_EXTENSION;
  assert.ok(workspace !== undefined, "the runner did not name the workspace");
  assert.ok(installed !== undefined, "the runner did not name the installed extension");

  const folders = vscode.workspace.workspaceFolders;
  assert.ok(folders !== undefined && folders.length > 0, "the window opened with no workspace folder");

  const extension = vscode.extensions.getExtension(extensionId);
  assert.ok(extension !== undefined, `${extensionId} is not installed in this window`);
  assert.equal(
    path.resolve(extension.extensionPath),
    path.resolve(installed),
    `the window loaded ${extension.extensionPath}, not the installed ${installed}`
  );
  line(`PASS installed-tree: ${extensionId} ${extension.packageJSON.version} at ${extension.extensionPath}`);

  await extension.activate();
  line("PASS activation: the extension activated");

  const entry = vscode.Uri.joinPath(folders[0].uri, entryName);
  const document = await vscode.workspace.openTextDocument(entry);
  await vscode.window.showTextDocument(document);

  const diagnostics = await waitFor(
    "a diagnostic",
    () => Promise.resolve(vscode.languages.getDiagnostics(entry)),
    (found) => found.some((diagnostic) => expectedDiagnostic.test(diagnostic.message))
  );

  const error = diagnostics.find((diagnostic) => expectedDiagnostic.test(diagnostic.message));
  assert.ok(error !== undefined, "a matching diagnostic disappeared between two reads");
  assert.equal(
    error.range.start.line,
    expectedDiagnosticLine,
    `the diagnostic is at line ${error.range.start.line + 1}, not ${expectedDiagnosticLine + 1}`
  );
  assert.equal(
    error.range.start.character,
    expectedDiagnosticColumn,
    `the diagnostic starts at column ${error.range.start.character + 1}, not ${expectedDiagnosticColumn + 1}`
  );
  assert.equal(
    error.range.end.character,
    expectedDiagnosticEndColumn,
    `the diagnostic ends at column ${error.range.end.character + 1}, not ${expectedDiagnosticEndColumn + 1}`
  );
  line(
    `PASS diagnostics: "${error.message}" at ${entryName}:${where(error.range.start)} ` +
      `(${diagnostics.length} diagnostic(s) on the file)`
  );

  const position = new vscode.Position(definitionLine, definitionColumn);
  const definition = await waitFor(
    "a definition",
    () => vscode.commands.executeCommand("vscode.executeDefinitionProvider", entry, position),
    (result) => firstTarget(result) !== undefined
  );

  const target = firstTarget(definition);
  assert.ok(target !== undefined, "a definition that was there disappeared between two reads");
  assert.equal(
    path.basename(target.uri.fsPath ?? target.uri.path),
    libraryName,
    `the definition points at ${target.uri.toString()}, not at ${libraryName}`
  );
  assert.equal(target.range.start.line, 0, `the definition points at line ${target.range.start.line + 1} of ${libraryName}, not at line 1`);
  line(
    `PASS definition: the call at ${entryName}:${where(position)} goes to ` +
      `${libraryName}:${where(target.range.start)}`
  );

  line("PASS both checks answered from the installed package");
};
