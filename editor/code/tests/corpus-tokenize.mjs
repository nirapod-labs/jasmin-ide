import {readdir, readFile, stat} from "node:fs/promises";
import {extname, join, relative} from "node:path";
import {loadGrammar, openScopesAtEnd, splitLines} from "./grammar-harness.mjs";

const sourceExtensions = new Set([".jazz", ".jinc"]);

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

/** Tokenizes one file and appends the problems it found to the error list. */
async function tokenizeFile(grammar, directory, file, errors) {
  const source = await readFile(file, "utf8");
  const lines = splitLines(source);
  const name = relative(directory, file);
  let ruleStack = null;
  let lineCount = 0;

  for (const [index, line] of lines.entries()) {
    let tokenized;
    try {
      tokenized = grammar.tokenizeLine(line, ruleStack);
    } catch (error) {
      errors.push(`${name}:${index + 1}: tokenizer error: ${error.message}`);
      return lineCount;
    }

    ruleStack = tokenized.ruleStack;
    lineCount += 1;

    const errorToken = tokenized.tokens.find((token) =>
      token.scopes.some((scope) => scope === "ERROR" || scope.startsWith("ERROR."))
    );
    if (errorToken) {
      errors.push(
        `${name}:${index + 1}: tokenizer produced the ERROR scope at column ${errorToken.startIndex + 1}`
      );
      return lineCount;
    }
  }

  const openScopes = openScopesAtEnd(grammar, ruleStack);
  if (openScopes.length > 0) {
    errors.push(
      `${name}: unterminated ${openScopes.join(", ")} scope at end of file (${lines.length} lines)`
    );
  }

  return lineCount;
}

/** Runs the corpus tokenize test against a directory. */
async function run(directory) {
  let directoryStat;
  try {
    directoryStat = await stat(directory);
  } catch (error) {
    if (error.code === "ENOENT") {
      console.log(`Skipping the corpus tokenize test: ${directory} does not exist.`);
      return;
    }
    throw error;
  }

  if (!directoryStat.isDirectory()) {
    console.error(`The corpus tokenize test needs a directory, not ${directory}.`);
    process.exitCode = 1;
    return;
  }

  const grammar = await loadGrammar();
  const files = (await collectFiles(directory)).sort();

  if (files.length === 0) {
    console.error(`No .jazz or .jinc files were found under ${directory}.`);
    process.exitCode = 1;
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

const directory = process.argv[2];

if (directory === undefined) {
  console.log(
    "Skipping the corpus tokenize test: pass the corpus directory as the first argument."
  );
} else {
  await run(directory);
}
