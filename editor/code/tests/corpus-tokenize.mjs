import {readdir, readFile, stat} from "node:fs/promises";
import {extname, join, relative} from "node:path";
import {loadGrammar, openScopesAtEnd, splitLines} from "./grammar-harness.mjs";

/**
 * The exit status that says there was no corpus to tokenize. It is distinct
 * from 0 (every file tokenized) and from 1 (the corpus is there and something
 * is wrong with it), so a caller can accept it as a skip of its own or refuse
 * it; `tests/corpus-step.mjs` is the one caller that accepts it.
 */
export const SKIP = 3;

/** The variable that names the corpus. */
const corpusVariable = "JASMIN_TM_CORPUS";

/**
 * Set to anything to make a missing corpus a failure instead of a skip. The
 * `editor-code` step of `make ci` sets it: the check is "every corpus file
 * tokenizes", so a run that tokenized nothing is red, not green.
 */
const requiredVariable = "JASMIN_TM_CORPUS_REQUIRED";

/** The shape every message about the corpus names. */
const shape =
  "a Jasmin release tree with compiler/tests and compiler/examples (that directory, or either of the two)";

const sourceExtensions = new Set([".jazz", ".jinc"]);

/** Fails the run with one message and a non-zero status. */
function fail(message) {
  console.error(`corpus: ${message}`);
  process.exitCode = 1;
}

/** Collects every .jazz and .jinc file under a directory, in a stable order. */
async function collectFiles(directory) {
  const entries = await readdir(directory, {withFileTypes: true});
  const files = [];

  for (const entry of entries) {
    const path = join(directory, entry.name);
    if (entry.isDirectory()) {
      files.push(...(await collectFiles(path)));
    } else if (sourceExtensions.has(extname(entry.name))) {
      const entryStat = entry.isSymbolicLink() ? await stat(path) : entry;
      if (entryStat.isFile()) {
        files.push(path);
      }
    }
  }

  return files;
}

/**
 * Tokenizes one file and appends the problems it found to the error list.
 * Returns the number of lines the file has: a file abandoned at its first
 * error still counts every line it holds, so the printed total is the size of
 * the corpus and not the part of it that got as far as a token.
 */
async function tokenizeFile(grammar, directory, file, errors) {
  const source = await readFile(file, "utf8");
  const lines = splitLines(source);
  const name = relative(directory, file);
  let ruleStack = null;

  for (const [index, line] of lines.entries()) {
    let tokenized;
    try {
      tokenized = grammar.tokenizeLine(line, ruleStack);
    } catch (error) {
      errors.push(`${name}:${index + 1}: tokenizer error: ${error.message}`);
      return lines.length;
    }

    ruleStack = tokenized.ruleStack;

    const errorToken = tokenized.tokens.find((token) =>
      token.scopes.some((scope) => scope === "ERROR" || scope.startsWith("ERROR."))
    );
    if (errorToken) {
      errors.push(
        `${name}:${index + 1}: tokenizer produced the ERROR scope at column ${errorToken.startIndex + 1}`
      );
      return lines.length;
    }
  }

  const openScopes = openScopesAtEnd(grammar, ruleStack);
  if (openScopes.length > 0) {
    errors.push(
      `${name}: unterminated ${openScopes.join(", ")} scope at end of file (${lines.length} lines)`
    );
  }

  return lines.length;
}

/** Runs the corpus tokenize test against a directory that has to be there. */
async function run(directory) {
  let directoryStat;
  try {
    directoryStat = await stat(directory);
  } catch (error) {
    if (error.code !== "ENOENT") {
      throw error;
    }
    fail(
      `${directory} does not exist; ${corpusVariable} names ${shape}`
    );
    return;
  }

  if (!directoryStat.isDirectory()) {
    fail(`${directory} is not a directory; ${corpusVariable} names ${shape}`);
    return;
  }

  const grammar = await loadGrammar();
  const files = (await collectFiles(directory)).sort();

  if (files.length === 0) {
    fail(
      `no .jazz or .jinc file under ${directory}; ${corpusVariable} names ${shape}`
    );
    return;
  }

  const errors = [];
  let lineCount = 0;
  for (const file of files) {
    lineCount += await tokenizeFile(grammar, directory, file, errors);
  }

  for (const error of errors) {
    console.error(error);
  }
  console.log(`${files.length} files, ${lineCount} lines, ${errors.length} errors`);

  if (errors.length > 0) {
    process.exitCode = 1;
  }
}

// The corpus root comes from the environment, so the step `pnpm run validate`
// runs is the one `make ci` runs and a caller can point both at a release tree
// without editing anything. An argument overrides it, for a hand run against
// one directory.
const argument = process.argv[2];
const root = argument === undefined ? (process.env[corpusVariable] ?? "") : argument;

if (root === "") {
  if ((process.env[requiredVariable] ?? "") === "") {
    console.log(`skip corpus (${corpusVariable} is not set; it names ${shape})`);
    process.exitCode = SKIP;
  } else {
    fail(
      `${corpusVariable} is not set and this run requires a corpus; set it to ${shape}`
    );
  }
} else {
  await run(root);
}
