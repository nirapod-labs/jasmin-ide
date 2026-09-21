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

