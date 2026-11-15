import assert from "node:assert/strict";
import {spawnSync} from "node:child_process";
import {describe, test} from "node:test";
import {fileURLToPath} from "node:url";
import {allowedSkips, expectedChecks, judgeEditingHelp} from "../test/vscode/skips.mjs";

// Two skips stand between a CI run and a measured check: a check the window
// could not run, recorded as SKIP, and a whole step the host could not run,
// printed as `skip editor-vscode`. Both have to be allowed by name, or the
// run reports green over a measurement nobody made. These cases pin both
// decisions without starting a window: the check-level allowlist in
// `test/vscode/skips.mjs`, and the step-level decision the Makefile's
// `editor-vscode` target makes.

/** One recorded check, as `suite.js` writes it. */
function record(check, status) {
  return {check, status, detail: `${check} recorded as ${status}`};
}

describe("the editing-help skip allowlist", () => {
  test("an unlisted skip fails the editing-help half", () => {
    const judgement = judgeEditingHelp([record("completion", "PASS"), record("hover", "SKIP")]);
    assert.equal(judgement.ok, false, "a skip nobody allowed is a measurement that did not happen");
    assert.deepEqual(
      judgement.unexpected.map((entry) => entry.check),
      ["hover"]
    );
  });

  test("a rename skip fails, because the server advertises rename", () => {
    assert.equal(allowedSkips.has("rename"), false);
    const judgement = judgeEditingHelp([record("completion", "PASS"), record("rename", "SKIP")]);
    assert.equal(judgement.ok, false, "a rename the server offers has to be measured, not skipped");
    assert.deepEqual(
      judgement.unexpected.map((entry) => entry.check),
      ["rename"]
    );
  });

  test("every allowed skip carries a reason", () => {
    for (const [check, reason] of allowedSkips) {
      assert.equal(typeof reason, "string", `${check} has no reason`);
      assert.ok(reason.length > 40, `${check}'s reason is too short to explain anything`);
    }
  });

  test("a FAIL is never allowed, and the allowlist does not rescue it", () => {
    const judgement = judgeEditingHelp([record("completion", "PASS"), record("rename", "FAIL")]);
    assert.equal(judgement.ok, false);
    assert.equal(judgement.failed.length, 1);
    assert.equal(judgement.unexpected.length, 0, "a failure is counted as a failure, never as a skip");
  });

  test("a run that recorded fewer checks fails even when the rest passed", () => {
    assert.equal(judgeEditingHelp([record("completion", "PASS")]).ok, false, `the half needs ${expectedChecks} records`);
    assert.equal(judgeEditingHelp([]).ok, false, "a window that recorded nothing is not green");
  });

  test("two passing checks are green", () => {
    const judgement = judgeEditingHelp([record("completion", "PASS"), record("rename", "PASS")]);
    assert.equal(judgement.ok, true);
    assert.equal(judgement.skipped.length, 0);
  });
});

describe("the step skip a run may allow", () => {
  const repositoryRoot = fileURLToPath(new URL("../../..", import.meta.url));

  /** What `make editor-vscode` would run, with only its own variable set. */
  function decide(allowed) {
    const variables = allowed === undefined ? [] : [`JASMIN_ALLOWED_SKIPS=${allowed}`];
    const result = spawnSync("make", ["-s", "-n", "-C", repositoryRoot, "editor-vscode", ...variables], {
      encoding: "utf8",
      env: {PATH: process.env.PATH}
    });
    return {status: result.status, stdout: result.stdout};
  }

  test("the step runs the window unless the skip is allowed", () => {
    const result = decide();
    assert.equal(result.status, 0);
    assert.match(result.stdout, /pnpm run test:vscode/u, "a step that says skip must not be the default");
    assert.doesNotMatch(result.stdout, /skip editor-vscode/u);
  });

  test("an allowed skip says so and runs nothing", () => {
    const result = decide("editor-vscode");
    assert.equal(result.status, 0);
    assert.match(result.stdout, /skip editor-vscode \(allowed by JASMIN_ALLOWED_SKIPS\)/u);
    assert.doesNotMatch(result.stdout, /test:vscode/u);
  });

  test("the list may be comma-separated, and another step's name allows nothing", () => {
    assert.match(decide("editor-zed,editor-vscode").stdout, /skip editor-vscode/u);
    assert.match(decide("editor-zed").stdout, /test:vscode/u);
    assert.match(decide("editor-vscode-legacy").stdout, /test:vscode/u, "a name matches whole, not as a prefix");
  });
});
