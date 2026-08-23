/**
 * The part of the workspace configuration the extension reads.
 *
 * It is a plain interface rather than the VS Code type so that the mapping
 * from settings to the server's options can be tested with Node alone.
 */
export interface ConfigurationReader {
  /** Returns the value of one dotted setting, or undefined when it is unset. */
  get(section: string): unknown;
}

/** The configuration section every setting lives under. */
export const configurationSection = "jasminIde";

/** The architecture used when neither a project file nor a setting names one. */
export const defaultArch = "x86-64";

/** The delay in milliseconds before the server checks a file again. */
export const defaultDiagnosticsDelay = 250;

/** The shortest diagnostics delay the server accepts. */
export const minimumDiagnosticsDelay = 50;

/** The longest diagnostics delay the server accepts. */
export const maximumDiagnosticsDelay = 1000;

/** The settings the extension reads at start-up. */
export interface JasminSettings {
  readonly serverPath: string;
  readonly serverArgs: readonly string[];
  readonly diagnosticsDelay: number;
  readonly arch: string;
  readonly include: Readonly<Record<string, string>>;
}

/** What the server receives in the `initialize` request's initializationOptions. */
export interface InitializationOptions {
  readonly diagnosticsDelay: number;
  readonly arch: string;
  readonly include: Readonly<Record<string, string>>;
}

/**
 * Reads the `jasminIde` settings and repairs values that are out of range or
 * of the wrong type, so a malformed settings file never stops the server from
 * starting.
 */
export function readSettings(configuration: ConfigurationReader): JasminSettings {
  const arch = readString(configuration, "arch", defaultArch).trim();

  return {
    serverPath: readString(configuration, "server.path", "").trim(),
    serverArgs: readStringArray(configuration, "server.args"),
    diagnosticsDelay: readDiagnosticsDelay(configuration),
    arch: arch === "" ? defaultArch : arch,
    include: readInclude(configuration)
  };
}

/**
 * Maps the settings onto the options of the `initialize` request, for the
 * server to use when the workspace has no project file.
 */
export function toInitializationOptions(settings: JasminSettings): InitializationOptions {
  return {
    diagnosticsDelay: settings.diagnosticsDelay,
    arch: settings.arch,
    include: settings.include
  };
}

function readString(
  configuration: ConfigurationReader,
  section: string,
  fallback: string
): string {
  const value = configuration.get(section);
  return typeof value === "string" ? value : fallback;
}

function readStringArray(
  configuration: ConfigurationReader,
  section: string
): readonly string[] {
  const value = configuration.get(section);
  if (!Array.isArray(value)) {
    return [];
  }
  return value.filter((item): item is string => typeof item === "string");
}

function readDiagnosticsDelay(configuration: ConfigurationReader): number {
  const value = configuration.get("diagnostics.delay");
  if (typeof value !== "number" || !Number.isFinite(value)) {
    return defaultDiagnosticsDelay;
  }
  return Math.min(maximumDiagnosticsDelay, Math.max(minimumDiagnosticsDelay, Math.round(value)));
}

function readInclude(configuration: ConfigurationReader): Record<string, string> {
  const value = configuration.get("include");
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
