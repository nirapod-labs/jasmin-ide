// The corpus step of `pnpm run validate`.
//
// The instrument is `tests/corpus-tokenize.mjs`: exit 0 means every file it
// found tokenized, exit 1 means the corpus is there and something is wrong
// with it (a tokenizer error, or a path that names nothing), and exit 3 means
// there was no corpus to tokenize at all.
//
// This step keeps those three apart. A pass is exit 0 and nothing else. A skip
// is exit 3, is printed as `skip corpus (…)`, and is the only status this
// script turns into 0 — so `pnpm run validate` on a developer's machine
// without a Jasmin release tree is green and its log says why. A wrong corpus
// is a failure here, never a skip: naming a directory that holds no Jasmin
// source is the same "green having done nothing" hazard as naming none.
//
// `make ci` sets JASMIN_TM_CORPUS_REQUIRED, which the instrument reads, so a
// `make ci` run with no corpus is exit 1 from the instrument and red here. That is
// the point: a green `make ci` log must never be able to mean the tokenizer
// read nothing.

import {spawnSync} from "node:child_process";

/** The instrument's status for "there was no corpus". */
const SKIP = 3;

const instrument = new URL("./corpus-tokenize.mjs", import.meta.url);

const run = spawnSync(process.execPath, [instrument.pathname], {stdio: "inherit"});

if (run.error) {
  console.error(`corpus: the tokenizer could not start: ${run.error.message}`);
  process.exitCode = 1;
} else if (run.status === 0) {
  // The instrument printed the count; the step has nothing to add.
  process.exitCode = 0;
} else if (run.status === SKIP) {
  console.log(
    "corpus: no file was tokenized by this run, so it is not corpus evidence; set JASMIN_TM_CORPUS to a Jasmin release tree to make it one"
  );
  process.exitCode = 0;
} else {
  // A tokenizer error, a corpus path that names nothing, or a `make ci` run with
  // no corpus: all of them are failures of the step, with the instrument's own
  // message and status.
  process.exitCode = typeof run.status === "number" ? run.status : 1;
}
