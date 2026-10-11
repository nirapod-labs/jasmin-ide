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

describe("constants", () => {
  const booleanScope = "constant.language.boolean.jasmin.crypto";

  test("true and false are boolean constants", () => {
    const source = "if (a && true || false) { }";
    assertScope(source, "true", booleanScope);
    assertScope(source, "false", booleanScope);
  });

  test("a boolean constant is not a numeric constant and not an operator", () => {
    const source = "x = true;";
    refuteScope(source, "true", numericScope);
    refuteScope(source, "true", operatorScope);
  });

  test("a word that ends in a boolean name is not a boolean constant", () => {
    // The grammar anchors on word boundaries, so a name that merely ends in
    // `true` stays plain. `untrue` shares a token with the whitespace around
    // it, so the check is on the scope carried by any token that holds it.
    const source = ["param int untrue = 1;", "x = untrue;"].join("\n");
    const holding = tokensOf(source).filter((token) => token.text.includes("untrue"));
    assert.equal(holding.length, 2, "Expected both mentions of `untrue` in tokens.");
    for (const token of holding) {
      assert.ok(
        !token.scopes.includes(booleanScope),
        `Expected ${JSON.stringify(token.text)} not to carry ${booleanScope}.`
      );
    }
    assertScope(source, "param", modifierScope);
  });
});

describe("strings", () => {
  const stringScope = "string.quoted.double.jasmin.crypto";
  const escapeScope = "constant.character.escape.jasmin.crypto";

  test("an escaped quote is an escape constant and does not end the string", () => {
    const source = 'x = "a\\"b";';
    assertScope(source, '\\"', escapeScope);
    assertScope(source, "b", stringScope);
    assert.equal(
      tokensWithText(source, '"').filter((token) => token.scopes.includes("punctuation.definition.string.end.jasmin.crypto"))
        .length,
      1,
      "Expected exactly one end quote: the escaped one is not one."
    );
    assertNoOpenScopes(source);
  });

  test("an escape carries the string scope and the escape scope", () => {
    const source = "x = \"tab\\there\";";
    assertScope(source, "\\t", escapeScope);
    assertScope(source, "\\t", stringScope);
    assertScope(source, "tab", stringScope);
    assertScope(source, "here", stringScope);
    assertNoOpenScopes(source);
  });

  test("a bare backslash-free string has no escape", () => {
    const source = 'x = "plain";';
    assert.equal(tokensWithText(source, "plain").length, 1);
    assertScope(source, "plain", stringScope);
    refuteScope(source, "plain", escapeScope);
  });
});

describe("operators", () => {
  test("the function arrow is an operator", () => {
    assertScope("export fn copy(reg u64 value) -> reg u64 {", "->", operatorScope);
  });

  test("arithmetic operators are operators", () => {
    const source = "x = a + b - c * d / e % f & g | h ^ i ~ j;";
    for (const operator of ["+", "-", "*", "/", "%", "&", "|", "^", "~"]) {
      assertScope(source, operator, operatorScope);
    }
  });

  test("shift operators keep their direction and width", () => {
    const source = "x = a << 2; y = a >> 2; z = a <<r 2; w = a >>r 2; u = a >>8s 2; v = a +32u 3;";
    for (const operator of ["<<", ">>", "<<r", ">>r", ">>8s", "+32u"]) {
      assertScope(source, operator, operatorScope);
    }
  });

  test("compound assignments are single operators", () => {
    const source = "x <<r= 2; y >>r= 3; z <<= 4; w >>= 5; v +32u= 6;";
    for (const operator of ["<<r=", ">>r=", "<<=", ">>=", "+32u="]) {
      assertScope(source, operator, operatorScope);
    }
  });

  test("comparison and logical operators are operators", () => {
    const source = "if (a <= b && c >= d || e == f && g != h) { }";
    assertScope(source, "<=", operatorScope);
    assertScope(source, ">=", operatorScope);
    assertScope(source, "==", operatorScope);
    assertScope(source, "!=", operatorScope);
    assertScope(source, "&&", operatorScope, 0);
    assertScope(source, "&&", operatorScope, 1);
    assertScope(source, "||", operatorScope);
  });

  test("the namespace accessor is punctuation", () => {
    const source = "state = AES::round(word);";
    assertScope(source, "::", "punctuation.accessor.namespace.jasmin.crypto");
    refuteScope(source, "::", operatorScope);
  });
});

describe("comments", () => {
  test("a line comment runs to the end of the line", () => {
    const source = ["// a line comment", "fn f() {}"].join("\n");
    assertScope(source, "// a line comment", "comment.line.double-slash.jasmin.crypto");
    assertScope(source, "fn", controlScope);
    assertNoOpenScopes(source);
  });

  test("a block comment has begin and end punctuation", () => {
    const source = ["/* a block comment */", "fn f() {}"].join("\n");
    assertScope(source, "/*", blockCommentScope);
    assertScope(source, "/*", "punctuation.definition.comment.begin.jasmin.crypto");
    assertScope(source, " a block comment ", blockCommentScope);
    assertScope(source, "*/", blockCommentScope);
    assertScope(source, "*/", "punctuation.definition.comment.end.jasmin.crypto");
    assertScope(source, "fn", controlScope);
    assertNoOpenScopes(source);
  });

  test("a nested block comment keeps one scope per level", () => {
    const source = ["/* outer", "   /* inner */", "   still outer */", "fn f() {}"].join("\n");

    assert.equal(scopeCount(scopesOf(source, "/*", 0), blockCommentScope), 1);
    assert.equal(scopeCount(scopesOf(source, "/*", 1), blockCommentScope), 2);
    assert.equal(scopeCount(scopesOf(source, " inner ", 0), blockCommentScope), 2);
    assert.equal(scopeCount(scopesOf(source, "*/", 0), blockCommentScope), 2);
    assert.equal(scopeCount(scopesOf(source, "*/", 1), blockCommentScope), 1);
    assertScope(source, "   still outer ", blockCommentScope);
    assertScope(source, "fn", controlScope);
    refuteScope(source, "fn", blockCommentScope);
    assertNoOpenScopes(source);
  });

  test("a line comment does not open a block comment", () => {
    const source = ["// /* not a block", "fn f() {}"].join("\n");
    assertScope(source, "// /* not a block", "comment.line.double-slash.jasmin.crypto");
    assertScope(source, "fn", controlScope);
    assertNoOpenScopes(source);
  });

  test("a block comment does not start a string", () => {
    const source = ['/* "not a string */', "fn f() {}"].join("\n");
    const commentTokens = tokensOnLine(source, 0);
    assert.ok(commentTokens.length >= 3, "Expected the comment to be tokenized in three parts.");
    for (const token of commentTokens) {
      assert.ok(
        token.scopes.includes(blockCommentScope),
        `Expected ${JSON.stringify(token.text)} on the comment line to carry ${blockCommentScope}.`
      );
      assert.ok(
        !token.scopes.some((scope) => scope.startsWith("string.")),
        `Expected ${JSON.stringify(token.text)} on the comment line not to start a string.`
      );
    }
    assertScope(source, "fn", controlScope);
    assertNoOpenScopes(source);
  });

  test("an unterminated comment is reported at end of file", () => {
    assert.deepEqual(tokenizeSource(grammar, "/* open\nstill open").openScopes, [blockCommentScope]);
  });

  test("an unterminated string is reported at end of file", () => {
    assert.deepEqual(tokenizeSource(grammar, 'x = "open').openScopes, [
      "string.quoted.double.jasmin.crypto"
    ]);
  });
});
