import assert from "node:assert/strict";
import test from "node:test";
import {versionMismatchMessage} from "../src/version-check.ts";

/** A server that reports the given version. */
function server(version) {
  return {serverInfo: {name: "jasmin-lsp", version}};
}

test("matching versions give no warning", () => {
  assert.equal(versionMismatchMessage("0.1.0", server("0.1.0")), undefined);
});

test("a mismatch names both versions once", () => {
  const message = versionMismatchMessage("0.1.0", server("0.2.0"));

  assert.equal(typeof message, "string");
  assert.equal(message.match(/0\.1\.0/gu)?.length, 1);
  assert.equal(message.match(/0\.2\.0/gu)?.length, 1);
  assert.ok(message.includes("extension"));
});

test("a server without serverInfo gives no warning", () => {
  assert.equal(versionMismatchMessage("0.1.0", undefined), undefined);
  assert.equal(versionMismatchMessage("0.1.0", {}), undefined);
  assert.equal(versionMismatchMessage("0.1.0", {serverInfo: {}}), undefined);
});

test("a server that reports an empty version gives no warning", () => {
  assert.equal(versionMismatchMessage("0.1.0", server("")), undefined);
});

test("an extension without a version gives no warning", () => {
  assert.equal(versionMismatchMessage(undefined, server("0.2.0")), undefined);
});

test("the comparison is exact, so a suffix is a mismatch", () => {
  assert.notEqual(versionMismatchMessage("0.1.0", server("0.1.0-rc1")), undefined);
  assert.notEqual(versionMismatchMessage("0.1.0", server("0.1")), undefined);
});
