(** The token stream a recovering parse works on.

    The compiler's parser is driven one token at a time by the driver in
    [Recover], so the buffer is lexed once here: every token with its byte
    span, its source positions, and the brace depth it sits at. The same
    pass records where an item and a statement can begin, because those are
    the two places a broken parse can resume from.

    The lexer is the compiler's own, so a file it cannot tokenise (an
    unterminated string, a stray byte) stops the stream where the lexer
    stopped. The scan then blanks that line and lexes again, a bounded
    number of times, so the items after the bad line are still seen. Every
    blanking preserves the buffer length, so every offset stays the offset
    the editor and the diagnostics use.

    The driver's two restart sets are written here as predicates over
    tokens. A test checks them against the grammar's own FIRST sets, taken
    from Menhir's inspection API, so a keyword added to a later Jasmin
    release fails that test instead of silently narrowing recovery. *)

open Jasmin

(** The largest buffer the driver will look at. Above this the parse is
    refused with one diagnostic rather than risking the editor's thread. *)
let max_bytes = 8 * 1024 * 1024

(** How many lines of a buffer one scan will blank to get past a lexing
    error before it gives up on the rest of the file. *)
let max_lexical_repairs = 8

(** Tokens that can begin a top-level item, other than the ones that begin
    a global declaration. A global starts with its type, which is also a
    keyword like [bool] or a type name, so no token marks it; the driver
    only restarts at items that begin with a keyword of their own. The
    guard test pins this set to the grammar's FIRST set for [top] minus its
    FIRST set for [pglobal]. *)
let item_tokens =
  [
    Parser.FN;
    Parser.EXPORT;
    Parser.INLINE;
    Parser.PARAM;
    Parser.TYPE;
    Parser.EXEC;
    Parser.NAMESPACE;
    Parser.REQUIRE;
    Parser.FROM;
    Parser.SHARPLBRACKET;
  ]

let item_start token = List.mem token item_tokens

(** Tokens that can begin a statement inside a function body, as the
    grammar's FIRST set for [pinstr] has them. The guard test pins this set
    too. *)
let statement_tokens =
  [
    Parser.ARRAYINIT;
    Parser.ASSERT;
    Parser.FOR;
    Parser.GLOBAL;
    Parser.IF;
    Parser.INLINE;
    Parser.LBRACKET;
    Parser.LPAREN;
    Parser.NID "";
    Parser.QUESTIONMARK;
    Parser.REG;
    Parser.SHARP;
    Parser.SHARPLBRACKET;
    Parser.STACK;
    Parser.UNDERSCORE;
    Parser.WHILE;
  ]

let statement_start token = List.mem token statement_tokens

type token = {
  token : Parser.token;
  start_byte : int;
  end_byte : int;
  start_pos : Lexing.position;
  end_pos : Lexing.position;
  depth : int;
      (** Braces opened and not closed before this token. A [}] carries the
          depth of the block it closes, so it is at least one. *)
}

type t = {
  name : string;
  text : string;  (** The buffer after the blankings, same length as given. *)
  tokens : token array;
  eof : Lexing.position;
}

let count t = Array.length t.tokens
let token t index = t.tokens.(index)
let text t = t.text
let name t = t.name
let eof_position t = t.eof

let span t ~start_byte ~end_byte =
  Jide_lang.Span.make ~start_byte ~end_byte

(** The three bytes of a byte order mark. Jasmin has no use for one and the
    lexer would reject it, so it is blanked before anything is read. The
    blanking keeps the length, so an offset in the result is the offset in
    the file the user has open. *)
let bom_length text =
  if
    String.length text >= 3
    && text.[0] = '\xef'
    && text.[1] = '\xbb'
    && text.[2] = '\xbf'
  then 3
  else 0

