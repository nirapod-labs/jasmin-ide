import assert from "node:assert/strict";
import {spawnSync} from "node:child_process";
import {readFileSync} from "node:fs";
import {mkdtemp, rm} from "node:fs/promises";
import {tmpdir} from "node:os";
import {join} from "node:path";
import {describe, test} from "node:test";
import {fileURLToPath} from "node:url";

// The corpus step is the half of `pnpm run validate` that can only be green
// after a real tokenize run. These cases pin the three statuses it must keep
// apart: 0 after tokenizing, a printed skip only when no corpus is named, and
// a failure for anything else — including a gate run with no corpus.

const step = new URL("./corpus-step.mjs", import.meta.url).pathname;
const skip = 3;

/** Runs the step with exactly the given environment plus the current PATH. */
function runStep(environment) {
  const result = spawnSync(process.execPath, [step], {
    encoding: "utf8",
    env: {PATH: process.env.PATH, ...environment}
  });
  return {status: result.status, stdout: result.stdout, stderr: result.stderr};
}

describe("the corpus step", () => {
  test("no corpus is a printed skip, not a pass", () => {
    const result = runStep({});
    assert.equal(result.status, 0, "a developer without a corpus still gets a green validate");
    assert.match(
      result.stdout,
      /^skip corpus \(JASMIN_TM_CORPUS is not set;/mu,
      "the skip has to be visible in the log"
    );
    assert.match(
      result.stdout,
      /not corpus evidence/iu,
      "the log has to say the run is not corpus evidence"
    );
  });

  test("a gate run with no corpus fails, so make ci cannot be green having read nothing", () => {
    const result = runStep({JASMIN_TM_CORPUS_REQUIRED: "1"});
    assert.equal(result.status, 1);
    assert.match(result.stderr, /JASMIN_TM_CORPUS is not set and this run requires a corpus/u);
    assert.match(result.stderr, /compiler\/tests/u, "the message names the expected shape");
    assert.match(result.stderr, /compiler\/examples/u, "the message names both roots");
  });

  test("a corpus path that does not exist fails, naming the shape", () => {
    const result = runStep({JASMIN_TM_CORPUS: "no-such-corpus-directory"});
    assert.equal(result.status, 1);
    assert.match(result.stderr, /does not exist/u);
    assert.match(result.stderr, /compiler\/tests/u);
    assert.notEqual(result.status, skip, "a wrong path is never the skip");
  });

  test("a path that is not a directory fails", async () => {
    const directory = await mkdtemp(join(tmpdir(), "corpus-step-"));
    try {
      const file = join(directory, "not-a-directory.jazz");
      await (await import("node:fs/promises")).writeFile(file, "fn f() {}\n");
      const result = runStep({JASMIN_TM_CORPUS: file});
      assert.equal(result.status, 1);
      assert.match(result.stderr, /is not a directory/u);
    } finally {
      await rm(directory, {recursive: true, force: true});
    }
  });

  test("a corpus with no Jasmin file fails rather than tokenizing nothing", async () => {
    const directory = await mkdtemp(join(tmpdir(), "corpus-step-"));
    try {
      const result = runStep({JASMIN_TM_CORPUS: directory});
      assert.equal(result.status, 1);
      assert.match(result.stderr, /no \.jazz or \.jinc file under/u);
    } finally {
      await rm(directory, {recursive: true, force: true});
    }
  });
});

// The statuses above are worth nothing if `pnpm run validate` never calls the
// step, or if the gate calls it without requiring a corpus: that is the "a
// step that always skips is not a step" defect in its other two shapes. These
// cases read the two files that do the wiring, so a `validate` that stops
// running the step, or an `editor-code` step that stops demanding the corpus,
// fails a test instead of quietly weakening the gate.
describe("the wiring that makes the corpus step a gate", () => {
  const repositoryRoot = fileURLToPath(new URL("../../..", import.meta.url));

  /** What `make editor-code` would run, with the corpus named or not. */
  function dryRun(corpus) {
    const variables = corpus === undefined ? [] : [`JASMIN_TM_CORPUS=${corpus}`];
    const result = spawnSync("make", ["-s", "-n", "-C", repositoryRoot, "editor-code", ...variables], {
      encoding: "utf8",
      env: {PATH: process.env.PATH}
    });
    assert.equal(result.status, 0, result.stderr);
    return result.stdout;
  }

  test("pnpm run validate runs the corpus step, not the bare tokenizer", () => {
    const manifest = JSON.parse(
      readFileSync(join(repositoryRoot, "editor", "code", "package.json"), "utf8")
    );
    assert.match(manifest.scripts.validate, /pnpm run corpus\b/u);
    assert.match(
      manifest.scripts.corpus,
      /tests\/corpus-step\.mjs/u,
      "the step, not the instrument: a developer's validate must not see its exit 3"
    );
  });

  test("make ci's editor-code step requires the corpus", () => {
    const named = dryRun("/corpus/tree");
    assert.match(named, /JASMIN_TM_CORPUS_REQUIRED=1/u, "a gate run has to make an absent corpus a failure, not a skip");
    assert.match(named, /JASMIN_TM_CORPUS="\/corpus\/tree"/u, "the gate run has to pass the named tree through, or it tokenizes nothing");
    assert.match(
      dryRun(),
      /skip editor-code \(needs JASMIN_TM_CORPUS\)/u,
      "a run with no corpus has to print a skip, not run the step and pass"
    );
  });
});
