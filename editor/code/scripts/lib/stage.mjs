// Staging the bundled server inside the extension directory.
//
// `vsce package` copies the extension directory into the archive and consults
// `.vscodeignore`; it has no notion of "fetch this file from elsewhere". A
// package that bundles the server therefore has to have the server in the
// extension directory when `vsce` runs, with the mode and the digest file the
// package is supposed to hold.
//
// That is what this module does, and it does it against a directory the
// caller names, so the packaging test can stage a fake binary in a temporary
// directory and exercise the same code the packaging script runs.
import { statSync } from "node:fs";
import { chmod, cp, mkdir, readFile, rm, stat, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { digestFileContents, digestOfBytes, parseDigestFile } from "./digest.mjs";
import { digestFileName, serverDirectory, serverExecutable } from "./platforms.mjs";
import { resolveServerPath } from "../../src/server-lookup.ts";

/** The mode a bundled server has to carry: readable, writable, executable. */
export const stagedServerMode = 0o755;

/** Reports whether a path is an existing regular file, as the extension does. */
export function isFile(candidate) {
  try {
    return statSync(candidate, {throwIfNoEntry: false})?.isFile() === true;
  } catch {
    // A path that cannot be inspected is not a usable server.
    return false;
  }
}

/** The `server/` directory inside an extension directory. */
export function serverDirectoryPath(extensionDirectory) {
  return join(extensionDirectory, serverDirectory);
}

/** The bundled executable's path inside an extension directory. */
export function bundledServerPath(extensionDirectory) {
  return join(extensionDirectory, serverDirectory, serverExecutable);
}

/** The digest file's path inside an extension directory. */
export function bundledDigestPath(extensionDirectory) {
  return join(extensionDirectory, serverDirectory, digestFileName);
}

/**
 * Copies a server executable into an extension directory as `server/jasmin-lsp`,
 * gives it mode 0755 and writes `server/SHA256SUMS` with its digest.
 *
 * The caller has already checked that the binary exists and is executable;
 * this function checks again, because the digest and the mode are what the
 * package is judged by and a silently wrong one is worse than a refusal.
 *
 * Returns the staged path, the digest file's path and the digest itself.
 */
export async function stageServer(extensionDirectory, binaryPath) {
  const binary = await stat(binaryPath);
  if (!binary.isFile()) {
    throw new Error(`${binaryPath} is not a regular file`);
  }
  if ((binary.mode & 0o111) === 0) {
    throw new Error(`${binaryPath} is not executable; a server with no execute bit cannot start`);
  }

  const directory = serverDirectoryPath(extensionDirectory);
  await mkdir(directory, {recursive: true});

  const serverPath = bundledServerPath(extensionDirectory);
  await cp(binaryPath, serverPath);
  // The copy keeps the source's permission bits, and the umask can clear one
  // of them, so the mode is set rather than assumed.
  await chmod(serverPath, stagedServerMode);

  const digest = digestOfBytes(await readFile(serverPath));
  const digestPath = bundledDigestPath(extensionDirectory);
  await writeFile(digestPath, digestFileContents([[serverExecutable, digest]]), "utf8");

  return {serverPath, digestPath, digest};
}

/** Removes a staged `server/` directory, if there is one. */
export async function clearStagedServer(extensionDirectory) {
  await rm(serverDirectoryPath(extensionDirectory), {recursive: true, force: true});
}

/** Reports whether an extension directory has a staged `server/`. */
export async function hasStagedServer(extensionDirectory) {
  const found = await stat(serverDirectoryPath(extensionDirectory)).catch(() => undefined);
  return found !== undefined;
}

/**
 * What the extension's own lookup finds in a staged extension directory.
 *
 * The setting is empty and `PATH` is not consulted, because the question is
 * only whether the package carries a server the extension will find: a host
 * that has `jasmin-lsp` installed must not be able to make a package without
 * one look correct.
 */
export function stagedResolution(extensionDirectory) {
  return resolveServerPath({
    settingPath: "",
    extensionPath: extensionDirectory,
    pathLookup: () => undefined,
    isFile
  });
}

/**
 * Checks a staged extension directory the way a package of it is judged: the
 * lookup finds `server/jasmin-lsp`, the digest file is there, it names that
 * file, and the digest it records is the digest of the bytes on disk.
 *
 * Returns the list of problems, empty when the directory passes. Every
 * problem is reported rather than the first, because a message that names one
 * thing at a time makes a round trip for each.
 */
export async function inspectStagedServer(extensionDirectory) {
  const problems = [];
  const resolution = stagedResolution(extensionDirectory);

  if (resolution.kind !== "found") {
    problems.push(`the server lookup does not find ${serverDirectory}/${serverExecutable}`);
    return problems;
  }
  if (resolution.source !== "extension") {
    problems.push(`the server lookup found the server from ${resolution.source}`);
  }
  if (resolution.path !== bundledServerPath(extensionDirectory)) {
    problems.push(`the server lookup found ${resolution.path}, not the bundled server`);
  }

  const serverPath = bundledServerPath(extensionDirectory);
  const stagedMode = (await stat(serverPath)).mode & 0o777;
  if ((stagedMode & 0o111) === 0) {
    problems.push(`the bundled server has mode ${stagedMode.toString(8)}, which is not executable`);
  }

  let digestText;
  try {
    digestText = await readFile(bundledDigestPath(extensionDirectory), "utf8");
  } catch {
    problems.push(`${serverDirectory}/${digestFileName} is missing`);
    return problems;
  }

  let entries;
  try {
    entries = parseDigestFile(digestText);
  } catch (error) {
    problems.push(`${serverDirectory}/${digestFileName} does not parse: ${error.message}`);
    return problems;
  }

  const recorded = entries.get(serverExecutable);
  if (recorded === undefined) {
    problems.push(`${serverDirectory}/${digestFileName} has no line for ${serverExecutable}`);
    return problems;
  }

  const actual = digestOfBytes(await readFile(serverPath));
  if (recorded !== actual) {
    problems.push(
      `${serverDirectory}/${digestFileName} records ${recorded} for ${serverExecutable}, but the file is ${actual}`
    );
  }

  return problems;
}
