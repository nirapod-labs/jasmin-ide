/**
 * The one wire shape of the initialize options, checked against the fixture
 * the server's own test reads.
 *
 * `test/fixtures/initialization-options.json` is the object a client sends as
 * `initializationOptions`. This test builds that object from the fixture's
 * own values through both places the extension builds it — `settings.ts` for
 * the `LanguageClient` and `server-start.ts` for the launch plan — and
 * compares each with the file, key for key. The server's cram test
 * (`test/server/lifecycle/initialization-options.t`) sends the same file in a
 * real `initialize` request and records the delay the server applies, so a
 * name that changes on one side and not the other fails a test instead of a
 * user's session.
 *
 * The delay is nested: `diagnostics.delay`. The flat `diagnosticsDelay` the
 * extension used to send is not a name the server reads, and neither is the
 * delay under the configuration section's own name (`jasminIde`), which is
 * the name a `workspace/didChangeConfiguration` notification carries. The
 * last two tests refuse both by name, so neither can come back as a
 * compatibility alias.
 */
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { join } from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";
import { planServerLaunch } from "../src/server-start.ts";
import {
  configurationSection,
  readSettings,
  toInitializationOptions
} from "../src/settings.ts";

const repositoryRoot = fileURLToPath(new URL("../../..", import.meta.url));
const fixture = JSON.parse(
  readFileSync(join(repositoryRoot, "test", "fixtures", "initialization-options.json"), "utf8")
);

/** The settings a client reads when the user has the fixture's values set. */
function settingsFromFixture() {
  return readSettings({
    get(section) {
      switch (section) {
        case "diagnostics.delay":
          return fixture.diagnostics.delay;
        case "arch":
          return fixture.arch;
        case "include":
          return fixture.include;
        default:
          return undefined;
      }
    }
  });
}

test("the settings module sends the fixture's options, and nothing else", () => {
  assert.deepEqual(toInitializationOptions(settingsFromFixture()), fixture);
});

test("the launch plan sends the fixture's options unchanged", () => {
  const launch = planServerLaunch("/usr/local/bin/jasmin-lsp", settingsFromFixture());

  assert.deepEqual(launch.initializationOptions, fixture);
});

test("the fixture's options are the three documented keys, the delay nested", () => {
  const options = toInitializationOptions(settingsFromFixture());

  assert.deepEqual(Object.keys(options).sort(), ["arch", "diagnostics", "include"]);
  assert.deepEqual(Object.keys(options.diagnostics), ["delay"]);
  assert.equal(JSON.stringify(options).includes("diagnosticsDelay"), false);
});

test("the options never wrap the delay in the configuration section's name", () => {
  const options = toInitializationOptions(settingsFromFixture());

  assert.equal(Object.hasOwn(options, configurationSection), false);
  assert.deepEqual(Object.keys(options.diagnostics), ["delay"]);
});
