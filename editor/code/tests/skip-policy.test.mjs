import assert from "node:assert/strict";
import {spawnSync} from "node:child_process";
import {readFileSync} from "node:fs";
import {join} from "node:path";
import {describe, test} from "node:test";
import {fileURLToPath} from "node:url";
import {allowedSkips, expectedChecks, judgeEditingHelp} from "../test/vscode/skips.mjs";

// Two skips stand between a gate run and a measured check: a check the window
// could not run, recorded as SKIP, and a whole step the host could not run,
// printed as `skip editor-vscode`. Both have to be allowed by name, or the
// gate reports green over a measurement nobody made. These cases pin both
// decisions without starting a window: the check-level allowlist in
// `test/vscode/skips.mjs`, and the step-level decision in
// `scripts/skip-allowed.sh` that the Makefile calls.

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

  test("the rename skip the allowlist names passes, and carries its reason", () => {
    const judgement = judgeEditingHelp([record("completion", "PASS"), record("rename", "SKIP")]);
    assert.equal(judgement.ok, true, "the one skip this tree expects must not turn every run red");
    assert.equal(judgement.skipped.length, 1);
    assert.equal(judgement.unexpected.length, 0);

    const reason = allowedSkips.get("rename");
    assert.equal(typeof reason, "string");
    assert.match(reason, /renameProvider/u, "the reason names the capability the engine does not advertise");
  });

  test("every allowed skip carries a reason", () => {
    assert.ok(allowedSkips.size > 0, "an empty allowlist is not the same as no allowlist");
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

describe("the step skip the gate allows", () => {
  const repositoryRoot = fileURLToPath(new URL("../../..", import.meta.url));
  const script = join(repositoryRoot, "scripts", "skip-allowed.sh");

  /** Runs the decision the way the Makefile does, with only its own variable set. */
  function decide(environment, step = "editor-vscode") {
    const result = spawnSync("sh", [script, step, "VS Code cannot be downloaded"], {
      encoding: "utf8",
      env: {PATH: process.env.PATH, ...environment}
    });
    return {status: result.status, stdout: result.stdout, stderr: result.stderr};
  }

  test("a step that cannot run fails unless the skip is allowed", () => {
    const result = decide({});
    assert.equal(result.status, 1, "a gate line that says skip must not be the default");
    assert.match(result.stdout, /^fail editor-vscode \(VS Code cannot be downloaded\)/mu);
    assert.match(result.stdout, /JASMIN_ALLOWED_SKIPS=editor-vscode/u, "the failure says how to allow it");
  });

  test("an allowed skip prints the reason and passes", () => {
    const result = decide({JASMIN_ALLOWED_SKIPS: "editor-vscode"});
    assert.equal(result.status, 0);
    assert.match(
      result.stdout,
      /^skip editor-vscode \(VS Code cannot be downloaded; allowed by JASMIN_ALLOWED_SKIPS\)$/mu
    );
  });

  test("the list may be comma-separated, and another step's name allows nothing", () => {
    assert.equal(decide({JASMIN_ALLOWED_SKIPS: "editor-zed,editor-vscode"}).status, 0);
    assert.equal(decide({JASMIN_ALLOWED_SKIPS: "editor-zed"}).status, 1);
    assert.equal(
      decide({JASMIN_ALLOWED_SKIPS: "editor-vscode-legacy"}).status,
      1,
      "a name matches whole, not as a prefix"
    );
  });

  test("the gate step routes its skip through the decision", () => {
    const makefile = readFileSync(join(repositoryRoot, "Makefile"), "utf8");
    const lines = makefile.split("\n");
    const start = lines.findIndex((line) => line.startsWith("editor-vscode:"));
    assert.notEqual(start, -1, "the Makefile defines no editor-vscode target");

    const body = [];
    for (const line of lines.slice(start + 1)) {
      if (/^[A-Za-z][A-Za-z0-9_.-]*:/u.test(line)) {
        break;
      }
      body.push(line);
    }
    const target = body.join("\n");

    assert.match(target, /sh scripts\/skip-allowed\.sh editor-vscode/u, "the step must ask before it skips");
    assert.doesNotMatch(
      target,
      /printf 'skip editor-vscode \(VS Code cannot be downloaded\)/u,
      "the unconditional skip this fix removes must not come back"
    );
  });
});
