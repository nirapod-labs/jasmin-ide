import * as vscode from "vscode";

/** The configuration section every setting lives under. */
export const configurationSection = "jasminIde";

export const defaultArch = "x86-64";
export const defaultDiagnosticsDelay = 250;
export const minimumDiagnosticsDelay = 50;
export const maximumDiagnosticsDelay = 1000;

/** The settings the extension reads at start-up. */
export interface JasminSettings {
  readonly serverPath: string;
  readonly serverArgs: readonly string[];
  readonly diagnosticsDelay: number;
  readonly arch: string;
  readonly include: Readonly<Record<string, string>>;
}

/**
 * Reads the `jasminIde` settings and repairs values that are out of range or
 * of the wrong type, so a malformed settings file never stops the server from
 * starting.
 */
export function readSettings(): JasminSettings {
  const configuration = vscode.workspace.getConfiguration(configurationSection);
  const arch = readString(configuration, "arch", defaultArch).trim();

  return {
    serverPath: readString(configuration, "server.path", "").trim(),
    serverArgs: readStringArray(configuration, "server.args"),
    diagnosticsDelay: readDiagnosticsDelay(configuration),
    arch: arch === "" ? defaultArch : arch,
    include: readInclude(configuration)
  };
}

function readString(
  configuration: vscode.WorkspaceConfiguration,
  section: string,
  fallback: string
): string {
  const value = configuration.get<unknown>(section);
  return typeof value === "string" ? value : fallback;
}

function readStringArray(
  configuration: vscode.WorkspaceConfiguration,
  section: string
): readonly string[] {
  const value = configuration.get<unknown>(section);
  if (!Array.isArray(value)) {
    return [];
  }
  return value.filter((item): item is string => typeof item === "string");
}

function readDiagnosticsDelay(configuration: vscode.WorkspaceConfiguration): number {
  const value = configuration.get<unknown>("diagnostics.delay");
  if (typeof value !== "number" || !Number.isFinite(value)) {
    return defaultDiagnosticsDelay;
  }
  return Math.min(maximumDiagnosticsDelay, Math.max(minimumDiagnosticsDelay, Math.round(value)));
}

function readInclude(configuration: vscode.WorkspaceConfiguration): Record<string, string> {
  const value = configuration.get<unknown>("include");
  if (value === null || typeof value !== "object" || Array.isArray(value)) {
    return {};
  }

  const include: Record<string, string> = {};
  for (const [name, directory] of Object.entries(value as Record<string, unknown>)) {
    if (typeof directory === "string" && directory !== "") {
      include[name] = directory;
    }
  }
  return include;
}
