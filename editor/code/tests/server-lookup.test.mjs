import assert from "node:assert/strict";
import { join } from "node:path";
import test from "node:test";
import {
  resolveServerPath,
  serverExecutable,
  serverNotFoundMessage,
  serverSetting,
  serverSettingInvalidMessage
} from "../src/server-lookup.ts";

const extensionPath = join("/opt", "jasmin-ide");
const bundledServer = join(extensionPath, "server", serverExecutable);
const bundledWindowsServer = join(extensionPath, "server", `${serverExecutable}.exe`);
const settingServer = join("/usr", "local", "bin", serverExecutable);
const pathServer = join("/home", "jasmin", ".opam", "default", "bin", serverExecutable);

/** Runs the lookup against a fixed list of files and commands. */
function resolve({ settingPath = "", files = [], commands = {} } = {}) {
  return resolveServerPath({
    settingPath,
    extensionPath,
    isFile: (candidate) => files.includes(candidate),
    pathLookup: (command) => commands[command]
  });
}

test("the setting wins over the bundled server and PATH", () => {
  const resolution = resolve({
    settingPath: settingServer,
    files: [settingServer, bundledServer],
    commands: { [serverExecutable]: pathServer }
  });

  assert.deepEqual(resolution, { kind: "found", path: settingServer, source: "setting" });
});

test("the bundled server is used when the setting is empty", () => {
  const resolution = resolve({
    files: [bundledServer],
    commands: { [serverExecutable]: pathServer }
  });

  assert.deepEqual(resolution, { kind: "found", path: bundledServer, source: "extension" });
});

test("the bundled Windows executable is found too", () => {
  const resolution = resolve({ files: [bundledWindowsServer] });

  assert.deepEqual(resolution, {
    kind: "found",
    path: bundledWindowsServer,
    source: "extension"
  });
});

test("PATH is searched when the setting is empty and nothing is bundled", () => {
  const resolution = resolve({ commands: { [serverExecutable]: pathServer } });

  assert.deepEqual(resolution, { kind: "found", path: pathServer, source: "path" });
});

test("a missing server gives the message that names the setting and PATH", () => {
  const resolution = resolve();

  assert.equal(resolution.kind, "missing");
  assert.equal(resolution.message, serverNotFoundMessage());
  assert.ok(resolution.message.includes(serverSetting));
  assert.ok(resolution.message.includes("PATH"));
});

test("a setting that is not a file is reported instead of being skipped", () => {
  const missingSettingServer = join("/usr", "local", "bin", "not-a-server");
  const resolution = resolve({
    settingPath: missingSettingServer,
    files: [bundledServer],
    commands: { [serverExecutable]: pathServer }
  });

  assert.equal(resolution.kind, "missing");
  assert.equal(resolution.message, serverSettingInvalidMessage(missingSettingServer));
  assert.ok(resolution.message.includes(serverSetting));
});
