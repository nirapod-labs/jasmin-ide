import assert from "node:assert/strict";
import {describe, test} from "node:test";
import {loadGrammar, tokenizeSource} from "./grammar-harness.mjs";

const grammar = await loadGrammar();

const attributeScope = "meta.attribute.jasmin.crypto";
const blockCommentScope = "comment.block.jasmin.crypto";
const controlScope = "keyword.control.jasmin.crypto";
const declarationScope = "keyword.declaration.jasmin.crypto";
const intrinsicScope = "support.function.intrinsic.jasmin.crypto";
const numericScope = "constant.numeric.jasmin.crypto";
const operatorScope = "keyword.operator.jasmin.crypto";
const modifierScope = "storage.modifier.jasmin.crypto";
const typeScope = "storage.type.jasmin.crypto";

/** Returns every token of a source string. */
function tokensOf(source) {
  return tokenizeSource(grammar, source).tokens;
}

/** Returns the tokens on one zero-based line of a source string. */
function tokensOnLine(source, lineIndex) {
  return tokensOf(source).filter((token) => token.lineIndex === lineIndex);
}

/** Returns every token whose text is exactly the given text. */
function tokensWithText(source, text) {
  return tokensOf(source).filter((token) => token.text === text);
}

/** Returns the scopes of the n-th token whose text is exactly the given text. */
function scopesOf(source, text, occurrence = 0) {
  const matches = tokensWithText(source, text);
  assert.ok(
    matches.length > occurrence,
    `Expected ${JSON.stringify(text)} in the source; found ${matches.length} matching token(s).`
  );
  return matches[occurrence].scopes;
}

/** Asserts that a token carries a scope. */
function assertScope(source, text, scope, occurrence = 0) {
  const scopes = scopesOf(source, text, occurrence);
  assert.ok(
    scopes.includes(scope),
    `Expected ${JSON.stringify(text)} to carry ${scope}; got ${scopes.join(" ")}.`
  );
}

/** Asserts that a token does not carry a scope. */
function refuteScope(source, text, scope, occurrence = 0) {
  const scopes = scopesOf(source, text, occurrence);
  assert.ok(
    !scopes.includes(scope),
    `Expected ${JSON.stringify(text)} not to carry ${scope}; got ${scopes.join(" ")}.`
  );
}

/** Asserts that a source string does not end inside a comment, string or annotation. */
function assertNoOpenScopes(source) {
  assert.deepEqual(tokenizeSource(grammar, source).openScopes, []);
}

/** Counts how often a scope appears in a scope list. */
function scopeCount(scopes, scope) {
  return scopes.filter((candidate) => candidate === scope).length;
}

describe("declarations", () => {
  const source = [
    "namespace AES {",
    "  type lane = u32;",
    "}",
    "",
    "export fn rotate(reg u64 state) -> reg u64 {",
    "  return state;",
    "}"
  ].join("\n");

  test("fn introduces a function name", () => {
    assertScope(source, "fn", controlScope);
    assertScope(source, "rotate", "entity.name.function.jasmin.crypto");
  });

  test("type introduces a type declaration", () => {
    assertScope(source, "type", declarationScope);
  });

  test("namespace introduces a namespace name", () => {
    assertScope(source, "namespace", "keyword.declaration.namespace.jasmin.crypto");
    assertScope(source, "AES", "entity.name.namespace.jasmin.crypto");
  });
});

describe("modifiers", () => {
  test("export and inline mark a function", () => {
    const source = ["export", "inline", "fn mix(reg u64 value) -> reg u64 {", "  return value;", "}"].join(
      "\n"
    );
    assertScope(source, "export", modifierScope);
    assertScope(source, "inline", modifierScope);
  });

  test("param and global declare storage", () => {
    const source = ["param int ROUNDS = 10;", "global u64 state = 0;"].join("\n");
    assertScope(source, "param", modifierScope);
    assertScope(source, "global", modifierScope);
    assertScope(source, "int", typeScope);
    assertScope(source, "u64", typeScope);
  });

  test("reg, stack, ptr, const and mut are modifiers", () => {
    const source = [
      "export fn copy(reg const ptr u64 source) -> stack u64 {",
      "  const mut u64 value;",
      "  return source;",
      "}"
    ].join("\n");
    assertScope(source, "reg", modifierScope);
    assertScope(source, "const", modifierScope, 0);
    assertScope(source, "const", modifierScope, 1);
    assertScope(source, "ptr", modifierScope);
    assertScope(source, "stack", modifierScope);
    assertScope(source, "mut", modifierScope);
    assertScope(source, "u64", typeScope);
  });
});

describe("control keywords", () => {
  test("exec declares the execution environment", () => {
    assertScope("exec preserve(0:32);", "exec", controlScope);
  });

  test("require and from import files", () => {
    const source = 'from AES require "aes-core.jinc" "aes-round.jinc";';
    assertScope(source, "from", controlScope);
    assertScope(source, "require", controlScope);
    assertScope(source, "aes-core.jinc", "string.quoted.double.jasmin.crypto");
    assertScope(source, '"', "punctuation.definition.string.begin.jasmin.crypto", 0);
    assertScope(source, '"', "punctuation.definition.string.end.jasmin.crypto", 1);
  });

  test("loops and branches are control keywords", () => {
    const source = [
      "for i = 0 to 4 { }",
      "for j = 4 downto 0 { }",
      "while (x) { }",
      "if (x) { } else { }",
      "return x;"
    ].join("\n");
    assertScope(source, "for", controlScope, 0);
    assertScope(source, "for", controlScope, 1);
    assertScope(source, "to", controlScope);
    assertScope(source, "downto", controlScope);
    assertScope(source, "while", controlScope);
    assertScope(source, "if", controlScope);
    assertScope(source, "else", controlScope);
    assertScope(source, "return", controlScope);
  });

  test("assert and ArrayInit are control keywords", () => {
    const source = ['assert("round count", ROUNDS > 0);', "ArrayInit(destination);"].join("\n");
    assertScope(source, "assert", controlScope);
    assertScope(source, "ArrayInit", controlScope);
    assertScope(source, "round count", "string.quoted.double.jasmin.crypto");
  });
});

describe("annotations", () => {
  test("a bracketed annotation marks its interior", () => {
    const source = ["#[mmx]", "type lane = u32;"].join("\n");
    assertScope(source, "#[", attributeScope);
    assertScope(source, "#[", "punctuation.definition.annotation.begin.jasmin.crypto");
    assertScope(source, "mmx", attributeScope);
    assertScope(source, "]", attributeScope);
    assertScope(source, "]", "punctuation.definition.annotation.end.jasmin.crypto");
    assertScope(source, "type", declarationScope);
    refuteScope(source, "type", attributeScope);
    assertNoOpenScopes(source);
  });

  test("a multi-line sct string stays inside the annotation", () => {
    const source = [
      '#[sct="',
      "  {",
      "    state -> state",
      "  }",
      '  "]',
      "fn identity(reg u64 state) -> reg u64 {",
      "  return state;",
      "}"
    ].join("\n");

    assertScope(source, "#[", attributeScope);
    assertScope(source, "#[", "punctuation.definition.annotation.begin.jasmin.crypto");

    for (const lineIndex of [1, 2, 3, 4]) {
      const tokens = tokensOnLine(source, lineIndex);
      assert.ok(tokens.length > 0, `Expected tokens on line ${lineIndex + 1}.`);
      for (const token of tokens) {
        assert.ok(
          token.scopes.includes(attributeScope),
          `Expected ${JSON.stringify(token.text)} on line ${lineIndex + 1} to be inside the annotation.`
        );
      }
    }

    assertScope(source, "    state -> state", attributeScope);
    assertScope(source, "]", attributeScope);
    assertScope(source, "]", "punctuation.definition.annotation.end.jasmin.crypto");
    assertScope(source, "fn", controlScope);
    refuteScope(source, "fn", attributeScope);
    assertScope(source, "identity", "entity.name.function.jasmin.crypto");
    assertNoOpenScopes(source);
  });
});

describe("intrinsics", () => {
  test("an intrinsic carries the intrinsic scope", () => {
    assertScope("word = #set0();", "#set0", intrinsicScope);
  });

  test("alignment intrinsics carry the alignment scope", () => {
    const source = ["word = [#aligned :u32 source];", "#unaligned"].join("\n");
    assertScope(source, "#aligned", "storage.modifier.alignment.jasmin.crypto");
    assertScope(source, "#unaligned", "storage.modifier.alignment.jasmin.crypto");
  });

  test("a suffix stays part of the intrinsic name", () => {
    const source = "#ADCX_32 #BLENDV_16u8 #ADD_64";
    assertScope(source, "#ADCX_32", intrinsicScope);
    assertScope(source, "#BLENDV_16u8", intrinsicScope);
    assertScope(source, "#ADD_64", intrinsicScope);
    assert.equal(tokensWithText(source, "#ADCX_32").length, 1);
  });
});

describe("numbers", () => {
  test("numbers with underscores are single numeric tokens", () => {
    const source = "values = [0x0000_0000, 0b0101, 0o07, 1_024, 42, 0xFF_FF];";
    for (const literal of ["0x0000_0000", "0b0101", "0o07", "1_024", "42", "0xFF_FF"]) {
      assert.equal(tokensWithText(source, literal).length, 1, `Expected one token for ${literal}.`);
      assertScope(source, literal, numericScope);
    }
  });
});

