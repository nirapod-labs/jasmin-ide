import {readFile} from "node:fs/promises";
import textmate from "vscode-textmate";
import oniguruma from "vscode-oniguruma";

const {Registry, parseRawGrammar} = textmate;
const {loadWASM, OnigScanner, OnigString} = oniguruma;

/** The scope name of the grammar under test. */
export const grammarScope = "source.jasmin.crypto";

const grammarPath = new URL("../syntaxes/jasmin.tmLanguage.json", import.meta.url);
const wasmPath = new URL("../node_modules/vscode-oniguruma/release/onig.wasm", import.meta.url);

let grammarPromise;

/**
 * Loads the Jasmin grammar once per process, the same way grammar.test.mjs
 * does, and returns the loaded grammar.
 */
export function loadGrammar() {
  grammarPromise ??= (async () => {
    const [grammarText, wasm] = await Promise.all([readFile(grammarPath, "utf8"), readFile(wasmPath)]);
    await loadWASM(wasm.buffer);

    const registry = new Registry({
      onigLib: Promise.resolve({
        createOnigScanner: (patterns) => new OnigScanner(patterns),
        createOnigString: (value) => new OnigString(value)
      }),
      loadGrammar: async (scopeName) => {
        if (scopeName !== grammarScope) {
          return null;
        }
        return parseRawGrammar(grammarText, grammarPath.pathname);
      }
    });

    const grammar = await registry.loadGrammar(grammarScope);
    if (!grammar) {
      throw new Error(`The grammar ${grammarScope} could not be loaded.`);
    }
    return grammar;
  })();

  return grammarPromise;
}

/**
 * Splits source text into lines. An empty file has no lines and a final
 * newline does not add an empty line, so the count is the number of physical
 * lines.
 */
export function splitLines(source) {
  if (source === "") {
    return [];
  }
  const lines = source.split(/\r?\n/u);
  if (lines.at(-1) === "") {
    lines.pop();
  }
  return lines;
}

/**
 * Returns every scope that is still open after the last line of a file. An
 * empty result means the file ends outside any comment, string or annotation.
 */
export function openScopesAtEnd(grammar, ruleStack) {
  const probe = grammar.tokenizeLine("", ruleStack);
  const open = new Set();
  for (const token of probe.tokens) {
    for (const scope of token.scopes) {
      if (scope !== grammarScope) {
        open.add(scope);
      }
    }
  }
  return [...open];
}

/**
 * Tokenizes a source string and returns every token with its line index, its
 * text and its scopes, together with the final rule stack.
 */
export function tokenizeSource(grammar, source) {
  const lines = splitLines(source);
  const tokens = [];
  let ruleStack = null;

  for (const [lineIndex, line] of lines.entries()) {
    const tokenized = grammar.tokenizeLine(line, ruleStack);
    ruleStack = tokenized.ruleStack;
    for (const token of tokenized.tokens) {
      tokens.push({
        lineIndex,
        startIndex: token.startIndex,
        endIndex: token.endIndex,
        text: line.slice(token.startIndex, token.endIndex),
        scopes: token.scopes
      });
    }
  }

  return {lines, tokens, ruleStack, openScopes: openScopesAtEnd(grammar, ruleStack)};
}
