import { join } from "node:path";

/** Where a resolved server executable was found. */
export type ServerSource = "setting" | "extension" | "path";

/** A server executable the extension can start. */
export interface ServerFound {
  readonly kind: "found";
  readonly path: string;
  readonly source: ServerSource;
}

/** No usable server executable, with a message that says what to do. */
export interface ServerMissing {
  readonly kind: "missing";
  readonly message: string;
}

export type ServerResolution = ServerFound | ServerMissing;

/** The inputs of the lookup, so the order can be tested without VS Code. */
export interface ServerLookupOptions {
  /** The value of the `jasminIde.server.path` setting. */
  readonly settingPath: string;
  /** The directory the extension is installed in. */
  readonly extensionPath: string;
  /** Resolves a command name on `PATH`, returning an absolute path. */
  readonly pathLookup: (command: string) => string | undefined;
  /** Reports whether a path is an existing regular file. */
  readonly isFile: (candidate: string) => boolean;
}

/** The server executable's name. */
export const serverExecutable = "jasmin-lsp";

/** The name of the setting that overrides the search. */
export const serverSetting = "jasminIde.server.path";

const executableNames = [serverExecutable, `${serverExecutable}.exe`] as const;

/** The error shown when no server executable can be found anywhere. */
export function serverNotFoundMessage(): string {
  return (
    `No Jasmin language server was found: ${serverExecutable} is not installed. ` +
    `Set "${serverSetting}" to the absolute path of the executable, ` +
    `or install the server and put ${serverExecutable} on your PATH.`
  );
}

/** The error shown when the setting names a path that is not a file. */
export function serverSettingInvalidMessage(settingPath: string): string {
  return (
    `The "${serverSetting}" setting points at ${settingPath}, which is not a file. ` +
    `Correct the path, or clear the setting to search for the server automatically.`
  );
}

/**
 * Finds the server executable in the documented order: the setting, then the
 * copy inside the extension installation, then `PATH`.
 *
 * The function never touches the file system itself: it asks `isFile` and
 * `pathLookup`, which keeps the order testable without VS Code.
 */
export function resolveServerPath(options: ServerLookupOptions): ServerResolution {
  const settingPath = options.settingPath.trim();

  if (settingPath !== "") {
    if (options.isFile(settingPath)) {
      return {kind: "found", path: settingPath, source: "setting"};
    }
    return {kind: "missing", message: serverSettingInvalidMessage(settingPath)};
  }

  for (const name of executableNames) {
    const bundled = join(options.extensionPath, "server", name);
    if (options.isFile(bundled)) {
      return {kind: "found", path: bundled, source: "extension"};
    }
  }

  for (const name of executableNames) {
    const onPath = options.pathLookup(name);
    if (onPath !== undefined && onPath !== "") {
      return {kind: "found", path: onPath, source: "path"};
    }
  }

  return {kind: "missing", message: serverNotFoundMessage()};
}
