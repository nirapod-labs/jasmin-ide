// Reading a `.vsix`, which is a zip archive.
//
// The packaging check has to look inside the file `vsce` produced: which
// entries it holds, what mode each one carries and what bytes the server
// entry has. A check that only reads the file name cannot tell a package with
// a server from one without, and a check that trusts the staging directory
// would pass on a package that never picked the server up.
//
// Node has no zip reader in its standard library, so the two records of the
// format that matter here are read directly: the central directory, which
// names every entry and holds its mode, and the local header that starts its
// data. Stored and deflated entries are both read; those are the two methods
// a `.vsix` uses.
import { inflateRawSync } from "node:zlib";

const endOfCentralDirectory = 0x06054b50;
const centralDirectoryHeader = 0x02014b50;
const localFileHeader = 0x04034b50;
const stored = 0;
const deflated = 8;

/** Finds the end record, which sits after the directory, before the zip comment. */
function findEndRecord(buffer) {
  // The comment is at most 65535 bytes, so the record is within that of the end.
  const earliest = Math.max(0, buffer.length - 22 - 0xffff);
  for (let offset = buffer.length - 22; offset >= earliest; offset -= 1) {
    if (buffer.readUInt32LE(offset) === endOfCentralDirectory) {
      return offset;
    }
  }
  throw new Error("no end-of-central-directory record: this is not a zip archive");
}

/**
 * Reads every entry of a zip archive, with the mode the archive records for
 * it.
 *
 * The mode comes from the high half of the external attributes and only means
 * something when the archive was written on Unix, which is what `vsce` does.
 * An entry written elsewhere reports mode 0, and every check that reads the
 * mode treats 0 as "no mode recorded" rather than as a file with no bits set.
 */
export function readZip(buffer) {
  const end = findEndRecord(buffer);
  const count = buffer.readUInt16LE(end + 10);
  let offset = buffer.readUInt32LE(end + 16);

  const entries = new Map();

  for (let index = 0; index < count; index += 1) {
    if (buffer.readUInt32LE(offset) !== centralDirectoryHeader) {
      throw new Error(`central directory entry ${index} does not start where the directory says`);
    }

    const versionMadeBy = buffer.readUInt16LE(offset + 4);
    const method = buffer.readUInt16LE(offset + 10);
    const compressedSize = buffer.readUInt32LE(offset + 20);
    const nameLength = buffer.readUInt16LE(offset + 28);
    const extraLength = buffer.readUInt16LE(offset + 30);
    const commentLength = buffer.readUInt16LE(offset + 32);
    const externalAttributes = buffer.readUInt32LE(offset + 38);
    const localOffset = buffer.readUInt32LE(offset + 42);
    const path = buffer.toString("utf8", offset + 46, offset + 46 + nameLength);

    const unixHost = versionMadeBy >> 8 === 3;
    const mode = unixHost ? externalAttributes >>> 16 : 0;

    entries.set(path, {path, mode, method, compressedSize, localOffset});

    offset += 46 + nameLength + extraLength + commentLength;
  }

  function bytesFor(entry) {
    if (buffer.readUInt32LE(entry.localOffset) !== localFileHeader) {
      throw new Error(`${entry.path} has no local file header where the directory says`);
    }
    const nameLength = buffer.readUInt16LE(entry.localOffset + 26);
    const extraLength = buffer.readUInt16LE(entry.localOffset + 28);
    const start = entry.localOffset + 30 + nameLength + extraLength;
    const data = buffer.subarray(start, start + entry.compressedSize);

    switch (entry.method) {
      case stored:
        return Buffer.from(data);
      case deflated:
        return inflateRawSync(data);
      default:
        throw new Error(`${entry.path} is compressed with method ${entry.method}, which is not read`);
    }
  }

  return {
    /** Every entry's path, the way the archive spells it. */
    paths() {
      return [...entries.keys()];
    },
    /** One entry's mode, or `undefined` when the archive holds no such entry. */
    mode(path) {
      return entries.get(path)?.mode;
    },
    /** One entry's bytes, or `undefined` when the archive holds no such entry. */
    read(path) {
      const entry = entries.get(path);
      return entry === undefined ? undefined : bytesFor(entry);
    },
    /** One entry as UTF-8 text, or `undefined` when it is not in the archive. */
    text(path) {
      return this.read(path)?.toString("utf8");
    }
  };
}
