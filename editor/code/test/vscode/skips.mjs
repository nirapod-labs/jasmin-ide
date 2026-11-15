// Which editing-help skips a green run may carry, and why.
//
// The window records a check it could not measure as SKIP with the capability
// it was missing. A skip is not a measurement, so a run that ends PASS with
// one has to say so and has to have expected it: otherwise a check can stop
// being measured and the run stays green, which is the failure this module
// exists to prevent.
//
// The allowlist is kept in code, not in an environment variable, because each
// entry is a promise about the product: the reason names what has to land
// before the entry can go. `run.mjs` prints the entry beside the skip and
// fails on a skip whose name is not here; `tests/skip-policy.test.mjs` holds
// both halves to their contract without starting a window.

/** The checks the window is expected to record. Fewer means it stopped early. */
export const expectedChecks = 2;

/**
 * The checks that may be recorded as SKIP, each with the reason the skip is
 * allowed today. The key is the check's own name — `recordCheck`'s first
 * argument in `suite.js` — not the capability that was missing.
 *
 * The list is empty: the server advertises both completion and rename (with
 * `prepareProvider`), so the window measures both checks and a skip of either
 * fails the run.
 *
 * @type {Map<string, string>}
 */
export const allowedSkips = new Map([]);

/**
 * Decides the editing-help half of the run from the records the window wrote.
 *
 * `records` is the suite's `{check, status, detail}` list, in the order the
 * checks ran. The half is green only when every expected check was recorded,
 * none failed and every skip is in the allowlist: a skip of a check nobody
 * allowed is a measurement that did not happen, and the run must not pass on
 * it.
 */
export function judgeEditingHelp(records) {
  const skipped = records.filter((record) => record.status === "SKIP");
  const failed = records.filter((record) => record.status === "FAIL");
  const unexpected = skipped.filter((record) => !allowedSkips.has(record.check));

  return {
    ok: records.length === expectedChecks && failed.length === 0 && unexpected.length === 0,
    skipped,
    failed,
    unexpected
  };
}
