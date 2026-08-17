// Runs the extension inside a real VS Code window and checks that the window
// started the server and can restart it.
//
// This is the only check that exercises the editor's half of the start path:
// the `LanguageClient` object and the client capabilities VS Code supplies.
// It needs a display and a desktop session, so it is not part of
// `pnpm run validate`; `pnpm run test:vscode` runs it by hand. The first run
// downloads a VS Code build into `.vscode-test/`.
import { runTests } from "@vscode/test-electron";
import { mkdir, mkdtemp, rm, stat, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

const editorDirectory = fileURLToPath(new URL("../..", import.meta.url));
const repositoryRoot = fileURLToPath(new URL("../../../..", import.meta.url));
const builtServer = join(repositoryRoot, "_build", "default", "lsp-server", "native", "main.exe");
const serverPath = process.env.JASMIN_LSP ?? builtServer;

try {
  await stat(serverPath);
} catch {
  console.error(`no server executable at ${serverPath}: build it (make build) or set JASMIN_LSP`);
  process.exit(1);
}

// A throw-away workspace whose settings point the extension at the built
// server: the same setting a user sets, and the first place the lookup tries.
const workspace = await mkdtemp(join(tmpdir(), "jasmin-ide-vscode-"));
await mkdir(join(workspace, ".vscode"), {recursive: true});
await writeFile(
  join(workspace, ".vscode", "settings.json"),
  `${JSON.stringify({"jasminIde.server.path": serverPath}, null, 2)}\n`,
  "utf8"
);

let failure;
try {
  const code = await runTests({
    extensionDevelopmentPath: editorDirectory,
    extensionTestsPath: fileURLToPath(new URL("./suite.js", import.meta.url)),
    launchArgs: [
      workspace,
      "--disable-extensions",
      "--user-data-dir",
      join(workspace, "user-data"),
      "--extensions-dir",
      join(workspace, "extensions")
    ]
  });

  if (code !== 0) {
    failure = new Error(`the VS Code run exited with ${code}`);
  } else {
    console.log("the extension started the server inside VS Code and restarted it");
  }
} finally {
  await rm(workspace, {recursive: true, force: true});
}

if (failure !== undefined) {
  console.error(failure.message);
  process.exitCode = 1;
}
