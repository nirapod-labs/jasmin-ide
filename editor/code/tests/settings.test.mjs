import assert from "node:assert/strict";
import test from "node:test";
import {
  defaultArch,
  defaultDiagnosticsDelay,
  maximumDiagnosticsDelay,
  minimumDiagnosticsDelay,
  readSettings,
  toInitializationOptions
} from "../src/settings.ts";

/** The `get` of a VS Code configuration, over a plain object. */
function configuration(values = {}) {
  return {
    get(section) {
      return values[section];
    }
  };
}

test("an empty configuration gives the documented defaults", () => {
  assert.deepEqual(readSettings(configuration()), {
    serverPath: "",
    serverArgs: [],
    diagnosticsDelay: defaultDiagnosticsDelay,
    arch: defaultArch,
    include: {}
  });
});

test("every setting is read and repaired", () => {
  const settings = readSettings(
    configuration({
      "server.path": "  /usr/local/bin/jasmin-lsp  ",
      "server.args": ["--stdio", 7, "--log", null],
      "diagnostics.delay": 500,
      arch: "  armv8  ",
      include: {libjade: "/src/libjade", broken: 5, empty: ""}
    })
  );

  assert.deepEqual(settings, {
    serverPath: "/usr/local/bin/jasmin-lsp",
    serverArgs: ["--stdio", "--log"],
    diagnosticsDelay: 500,
    arch: "armv8",
    include: {libjade: "/src/libjade"}
  });
});

test("the diagnostics delay is rounded and kept inside the supported range", () => {
  const cases = [
    [49.6, 50],
    [minimumDiagnosticsDelay, minimumDiagnosticsDelay],
    [249.4, 249],
    [499.6, 500],
    [maximumDiagnosticsDelay, maximumDiagnosticsDelay],
    [1000.4, maximumDiagnosticsDelay],
    [5000, maximumDiagnosticsDelay],
    [-5, minimumDiagnosticsDelay]
  ];

  for (const [value, expected] of cases) {
    const settings = readSettings(configuration({"diagnostics.delay": value}));
    assert.equal(settings.diagnosticsDelay, expected, `delay ${value}`);
  }
});

test("a diagnostics delay that is not a finite number falls back to the default", () => {
  for (const value of [Number.NaN, Number.POSITIVE_INFINITY, "300", null, {}]) {
    const settings = readSettings(configuration({"diagnostics.delay": value}));
    assert.equal(settings.diagnosticsDelay, defaultDiagnosticsDelay);
  }
});

test("a setting of the wrong type falls back to its default", () => {
  const settings = readSettings(
    configuration({"server.path": 42, "server.args": "--stdio", arch: null, include: []})
  );

  assert.equal(settings.serverPath, "");
  assert.deepEqual(settings.serverArgs, []);
  assert.equal(settings.arch, defaultArch);
  assert.deepEqual(settings.include, {});
});

test("a blank architecture falls back to the default", () => {
  assert.equal(readSettings(configuration({arch: "   "})).arch, defaultArch);
});

test("an include map keeps only named non-empty directories", () => {
  assert.deepEqual(readSettings(configuration({include: {good: "lib", blank: ""}})).include, {
    good: "lib"
  });
  assert.deepEqual(readSettings(configuration({include: null})).include, {});
  assert.deepEqual(readSettings(configuration({include: "lib"})).include, {});
});

test("the initialization options carry the delay, the architecture and the include map", () => {
  const settings = readSettings(
    configuration({
      "server.path": "/usr/local/bin/jasmin-lsp",
      "server.args": ["--stdio"],
      "diagnostics.delay": 120,
      arch: "armv8",
      include: {libjade: "/src/libjade"}
    })
  );

  assert.deepEqual(toInitializationOptions(settings), {
    arch: "armv8",
    include: {libjade: "/src/libjade"},
    diagnostics: {delay: 120}
  });
});

test("the initialization options hold nothing but the three server settings", () => {
  const options = toInitializationOptions(readSettings(configuration()));

  assert.deepEqual(Object.keys(options).sort(), ["arch", "diagnostics", "include"]);
});

test("the delay is nested, with no flat twin beside it", () => {
  const options = toInitializationOptions(readSettings(configuration({"diagnostics.delay": 700})));

  assert.deepEqual(options.diagnostics, {delay: 700});
  assert.equal(
    Object.hasOwn(options, "diagnosticsDelay"),
    false,
    "the server reads diagnostics.delay; a flat name would be a second wire shape"
  );
});

test("the server receives repaired values, not the raw settings", () => {
  const settings = readSettings(
    configuration({"diagnostics.delay": 2000, arch: "  ", include: {blank: ""}})
  );

  assert.deepEqual(toInitializationOptions(settings), {
    arch: defaultArch,
    include: {},
    diagnostics: {delay: maximumDiagnosticsDelay}
  });
});
