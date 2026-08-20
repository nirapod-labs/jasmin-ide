import assert from "node:assert/strict";
import {readFile} from "node:fs/promises";
import test from "node:test";
import {restartCommand, showLogCommand} from "../src/commands.ts";
import {
  defaultArch,
  defaultDiagnosticsDelay,
  maximumDiagnosticsDelay,
  minimumDiagnosticsDelay
} from "../src/settings.ts";

const manifest = JSON.parse(
  await readFile(new URL("../package.json", import.meta.url), "utf8")
);
const readme = await readFile(new URL("../README.md", import.meta.url), "utf8");
const settings = manifest.contributes.configuration.properties;

test("the manifest contributes the six settings with their defaults", () => {
  assert.deepEqual(Object.keys(settings).sort(), [
    "jasminIde.arch",
    "jasminIde.diagnostics.delay",
    "jasminIde.include",
    "jasminIde.server.args",
    "jasminIde.server.path",
    "jasminIde.trace.server"
  ]);

  assert.equal(settings["jasminIde.server.path"].default, "");
  assert.deepEqual(settings["jasminIde.server.args"].default, []);
  assert.equal(settings["jasminIde.trace.server"].default, "off");
  assert.equal(settings["jasminIde.diagnostics.delay"].default, defaultDiagnosticsDelay);
  assert.equal(settings["jasminIde.diagnostics.delay"].minimum, minimumDiagnosticsDelay);
  assert.equal(settings["jasminIde.diagnostics.delay"].maximum, maximumDiagnosticsDelay);
  assert.equal(settings["jasminIde.arch"].default, defaultArch);
  assert.deepEqual(settings["jasminIde.include"].default, {});
});

test("each setting carries a description for the settings editor", () => {
  for (const [name, setting] of Object.entries(settings)) {
    const text = setting.description ?? setting.markdownDescription;
    assert.equal(typeof text, "string", `${name} has no description`);
    assert.ok(text.length > 20, `${name} has a description too short to be useful`);
  }
});

test("the commands the extension registers are the ones the manifest contributes", () => {
  const contributed = manifest.contributes.commands.map((command) => command.command);

  assert.deepEqual([...contributed].sort(), [restartCommand, showLogCommand].sort());
});

test("each command has a title and the menu category", () => {
  for (const command of manifest.contributes.commands) {
    assert.ok(command.title.length > 0, `${command.command} has no title`);
    assert.equal(command.category, "Jasmin");
  }
});

test("the README documents every setting and every command", () => {
  for (const name of Object.keys(settings)) {
    assert.ok(readme.includes(`\`${name}\``), `the README does not name ${name}`);
  }

  for (const command of manifest.contributes.commands) {
    assert.ok(
      readme.includes(`Jasmin: ${command.title}`),
      `the README does not name Jasmin: ${command.title}`
    );
  }
});
