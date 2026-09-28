// The digest file the packages carry, and what it has to say.
//
// A package bundles the server, and a user who wants to know that the
// executable inside it is the one that was built for that platform reads
// `server/SHA256SUMS`. The file holds one line per file in `server/`, in the
// form the `sha256sum` tool writes and reads:
//
//   <digest><two spaces><name>
//
// Two spaces is the marker `sha256sum` writes for a text file and what its
// `--check` mode expects; a single space is the binary marker and would make
// the file a candidate for a binary read that the parser also accepts, so the
// writer here uses two and the reader accepts both, the way the tool does.
//
// The name is the file's name inside `server/`, never a path: a digest file
// whose entry names a directory outside the package would be a way to make a
// checker read an unrelated file.
import { createHash } from "node:crypto";

/** The digest of a buffer, in the lower-case hex `sha256sum` writes. */
export function sha256(buffer) {
  return createHash("sha256").update(buffer).digest("hex");
}

/** The digest of a file's bytes, given the bytes. */
export function digestOfBytes(bytes) {
  return sha256(bytes);
}

/** One line of a digest file, with the two-space separator `sha256sum` writes. */
export function digestLine(name, digest) {
  if (name === "" || name.includes("/") || name.includes("\\")) {
    throw new Error(`a digest line names a file in the digest file's directory, not ${name}`);
  }
  if (!/^[0-9a-f]{64}$/u.test(digest)) {
    throw new Error(`a digest is 64 lower-case hex characters, not ${digest}`);
  }
  return `${digest}  ${name}\n`;
}

/** A digest file with the given entries, one line each, sorted by name. */
export function digestFileContents(entries) {
  return [...entries]
    .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
    .map(([name, digest]) => digestLine(name, digest))
    .join("");
}

/**
 * Parses a digest file.
 *
 * Every non-empty, non-comment line must be a digest and a name, so a file
 * that was written for another tool, or that held a line whose digest was cut
 * short, is an error rather than a shorter list of entries: a checker that
 * skipped the lines it did not understand would accept a file whose server
 * line had been edited away.
 */
export function parseDigestFile(text) {
  const entries = new Map();

  for (const [index, raw] of text.split(/\r?\n/u).entries()) {
    const line = raw.trimEnd();
    if (line === "" || line.startsWith("#")) {
      continue;
    }

    const match = /^([0-9a-fA-F]{64}) [ *](.+)$/u.exec(line);
    if (match === null) {
      throw new Error(`line ${index + 1} of the digest file is not "<digest>  <name>": ${line}`);
    }

    const [, digest, name] = match;
    if (name.includes("/") || name.includes("\\")) {
      throw new Error(`line ${index + 1} of the digest file names a path, not a file: ${name}`);
    }
    if (entries.has(name)) {
      throw new Error(`the digest file names ${name} twice`);
    }

    entries.set(name, digest.toLowerCase());
  }

  return entries;
}
