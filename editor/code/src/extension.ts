import { statSync } from "node:fs";
import { delimiter, join } from "node:path";
import * as vscode from "vscode";
import { LanguageClient, State, TransportKind } from "vscode-languageclient/node";
import type { LanguageClientOptions, ServerOptions } from "vscode-languageclient/node";
import { resolveServerPath } from "./server-lookup.js";
import type { ServerSource } from "./server-lookup.js";
import { planServerLaunch } from "./server-start.js";
import { configurationSection, readSettings } from "./settings.js";

const languageId = "jasmin-crypto";
const clientId = configurationSection;
const clientName = "Jasmin IDE";
const outputChannelName = "Jasmin IDE";

const restartCommand = "jasminIde.restartServer";
const showLogCommand = "jasminIde.showServerLog";

// The transports the launch plan can name, by the name the plan gives them.
const transports = {stdio: TransportKind.stdio} as const;

let client: LanguageClient | undefined;
let channel: vscode.LogOutputChannel | undefined;

/**
 * Starts the language client. Activation never throws: every failure is
 * reported in the output channel and in a notification, and the grammar keeps
 * working on its own.
 */
export async function activate(context: vscode.ExtensionContext): Promise<void> {
  let created: vscode.LogOutputChannel;
  try {
    created = vscode.window.createOutputChannel(outputChannelName, {log: true});
  } catch {
    return;
  }

  channel = created;

  try {
    context.subscriptions.push(
      created,
      vscode.commands.registerCommand(restartCommand, () => restartServer(context, created)),
      vscode.commands.registerCommand(showLogCommand, () => {
        created.show(true);
      })
    );

    await startServer(context, created);
  } catch (error) {
    reportFailure(created, "Jasmin IDE could not start", error);
  }
}

/** Stops the language client and disposes the output channel. */
export async function deactivate(): Promise<void> {
  const activeChannel = channel;
  channel = undefined;
  await stopServer(activeChannel);
  activeChannel?.dispose();
}

async function restartServer(
  context: vscode.ExtensionContext,
  output: vscode.LogOutputChannel
): Promise<void> {
  output.info("Restarting the Jasmin language server.");
  await stopServer(output);
  await startServer(context, output);
}

async function startServer(
  context: vscode.ExtensionContext,
  output: vscode.LogOutputChannel
): Promise<void> {
  if (client !== undefined) {
    return;
  }

  const settings = readSettings();
  const resolution = resolveServerPath({
    settingPath: settings.serverPath,
    extensionPath: context.extensionPath,
    pathLookup: findOnPath,
    isFile
  });

  if (resolution.kind === "missing") {
    output.error(resolution.message);
    void vscode.window.showErrorMessage(resolution.message);
    return;
  }

  const launch = planServerLaunch(resolution.path, settings);

  output.info(
    `Starting the Jasmin language server at ${launch.command} (${describeSource(resolution.source)}).`
  );

  const serverOptions: ServerOptions = {
    command: launch.command,
    args: [...launch.args],
    transport: transports[launch.transport]
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{language: languageId}],
    outputChannel: output,
    synchronize: {configurationSection},
    initializationOptions: launch.initializationOptions
  };

  const started = new LanguageClient(clientId, clientName, serverOptions, clientOptions);

  try {
    await started.start();
  } catch (error) {
    reportFailure(
      output,
      `The Jasmin language server at ${resolution.path} could not be started`,
      error
    );
    await stopServer(output, started);
    return;
  }

  client = started;
  output.info("The Jasmin language server is running.");
  warnOnVersionMismatch(context, started, output);
}

async function stopServer(
  output: vscode.LogOutputChannel | undefined,
  running: LanguageClient | undefined = client
): Promise<void> {
  if (running === undefined) {
    return;
  }
  if (client === running) {
    client = undefined;
  }

  try {
    if (running.state !== State.Stopped) {
      await running.stop();
    }
  } catch (error) {
    output?.warn(`The Jasmin language server did not stop cleanly: ${describeError(error)}`);
  }
}

function warnOnVersionMismatch(
  context: vscode.ExtensionContext,
  running: LanguageClient,
  output: vscode.LogOutputChannel
): void {
  const serverVersion = running.initializeResult?.serverInfo?.version;
  if (serverVersion === undefined) {
    // A server that reports no serverInfo is not an error.
    return;
  }

  const clientVersion = extensionVersion(context);
  if (clientVersion === undefined || clientVersion === serverVersion) {
    return;
  }

  const message =
    `The Jasmin language server reports version ${serverVersion}, ` +
    `but this extension is version ${clientVersion}. ` +
    "Some features may be missing or may not work.";
  output.warn(message);
  void vscode.window.showWarningMessage(message);
}

/** The version in the extension's own manifest. */
function extensionVersion(context: vscode.ExtensionContext): string | undefined {
  // The VS Code API types `Extension.packageJSON` as `any`; it is narrowed once
  // here so no untyped value reaches the rest of the extension.
  const manifest = context.extension.packageJSON as {readonly version?: unknown};
  return typeof manifest.version === "string" ? manifest.version : undefined;
}

/** Resolves a command name on `PATH`, the third place the lookup tries. */
function findOnPath(command: string): string | undefined {
  const searchPath = process.env.PATH;
  if (searchPath === undefined || searchPath === "") {
    return undefined;
  }

  for (const directory of searchPath.split(delimiter)) {
    if (directory === "") {
      continue;
    }
    const candidate = join(directory, command);
    if (isFile(candidate)) {
      return candidate;
    }
  }

  return undefined;
}

function isFile(candidate: string): boolean {
  try {
    return statSync(candidate, {throwIfNoEntry: false})?.isFile() === true;
  } catch {
    // A path that cannot be inspected is not a usable server.
    return false;
  }
}

function describeSource(source: ServerSource): string {
  switch (source) {
    case "setting":
      return "from the jasminIde.server.path setting";
    case "extension":
      return "bundled with the extension";
    case "path":
      return "from PATH";
  }
}

function describeError(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

function reportFailure(output: vscode.LogOutputChannel, summary: string, error: unknown): void {
  const message = `${summary}: ${describeError(error)}`;
  output.error(message);
  void vscode.window.showErrorMessage(message);
}
