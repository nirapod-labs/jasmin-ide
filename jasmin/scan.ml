(** The token stream a recovering parse works on.

    The compiler's parser is driven one token at a time by the driver in
    [Recover], so the buffer is lexed once here: every token with its byte
    span, its source positions, and the brace depth it sits at. The same
    pass records where an item and a statement can begin, because those are
    the two places a broken parse can resume from.

    The lexer is the compiler's own, so a file it cannot tokenise stops the
    stream where the lexer stopped. The tokens read before that point are
    still a valid prefix of the file, so they are kept: a file whose only
    fault is an unterminated comment at the end returns every item before
    the comment. The rest of the buffer is then blanked, a bounded number of
    times, so the items after a bad line are seen too. Every blanking
    preserves the buffer length, so every offset stays the offset the editor
    and the diagnostics use, and every blanked region is reported, because a
    diagnostic inside one of them is about bytes the compiler never read.

    One blanking rule is not a line: an unterminated string. The lexer
    rejects it at the opening quote, and a string runs to the next quote or
    to the end of the file, so blanking the quote's line would hand the
    string's body to the scanner as code and a function written inside the
    string would be fabricated into the tree. The quote's region is blanked
    instead, which is what a string the user never closed means.

    The driver's two restart sets are written here as predicates over
    tokens. A test checks them against the grammar's own FIRST sets, taken
    from Menhir's inspection API, so a keyword added to a later Jasmin
    release fails that test instead of silently narrowing recovery. *)

open Jasmin

(** The largest buffer the driver will look at. Above this the parse is
    refused with one diagnostic rather than risking the editor's thread: the
    work a scan does is linear in the tokens it builds. Real Jasmin runs
    between four and eleven bytes per token, so a buffer at this ceiling
    holds roughly 800,000 tokens; the token ceiling below is what actually
    bounds the work, at 262,144 tokens, and it is reached well before this
    byte count. Every Jasmin file in the corpus and in the reference
    libraries is under 7 KiB, four orders of magnitude below it.

    This must not be smaller than the ceiling the engine applies before it
    asks for a parse. The engine admits a document of up to its own
    [max_bytes] and promises it an analysis, so a buffer the engine let
    through and this module refused would turn a promise into a diagnostic
    about a size the caller never imposed. The engine depends on this
    library, not the other way round, so the number is written here; the
    engine's test suite pins the two ceilings against each other. *)
let max_bytes = 8 * 1024 * 1024

(** The most tokens one scan will build. A dense buffer reaches this well
    before the byte ceiling, and this, not the byte count, is what bounds
    the work done on the editor's thread. *)
let max_tokens = 262_144

(** How many regions of a buffer one scan will blank to get past a lexing
    error before it gives up on the rest of the file. *)
let max_lexical_repairs = 8

(** Tokens that can begin a top-level item, other than the ones that begin
    a global declaration. A global starts with its type, which is also a
    keyword like [bool] or a type name, so no token marks it; the driver
    only restarts at items that begin with a keyword of their own. The
    guard test pins this set to the grammar's FIRST set for [top] minus its
    FIRST set for [ptype]. *)
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

(** Tokens that can begin a top-level item and can never begin a statement:
    {!item_tokens} with the members of {!statement_tokens} taken out. Seeing
    one of them where a statement belongs means the block that contains the
    statement was never closed, which is the one repair the design makes
    without dropping anything: a virtual [}] closes the block and the text
    read so far stays in the tree.

    Two kinds of token are left out. [inline] and [#[] are storage classes and
    annotations, so a body legitimately contains them. [export] is an
    annotation label as well, and it is here because it cannot begin a
    statement: the places it can appear inside a body are all inside a [#[]
    group, and a virtual [}] offered there breaks that group, so the parser
    refuses the attempt and the driver tries its other repairs.
    A name is left out for the same reason: it begins a statement as well as
    a global. *)
let item_only_tokens =
  [
    Parser.FN;
    Parser.EXPORT;
    Parser.TYPE;
    Parser.PARAM;
    Parser.EXEC;
    Parser.NAMESPACE;
    Parser.REQUIRE;
    Parser.FROM;
  ]

(** Tokens that can begin a type, which is how a global declaration begins.
    The grammar's FIRST set for [ptype], which is the one FIRST set the item
    set above leaves out; a test pins it. A token of this set that is where
    an item belongs may begin a global, may begin a statement and may
    continue an expression, so it is never a restart target on its own: the
    driver asks the parser whether a whole item really parses from there. *)
let type_tokens =
  [
    Parser.T_BOOL;
    Parser.T_INT;
    Parser.T_W (U8, `Word (Some `Unsigned));
    Parser.NID "";
  ]

(** Two tokens are the same token when their constructors match. A token that
    carries a value matches any value of its constructor: the sets below name
    token kinds, and a restart point must be found whatever the identifier at
    it happens to be spelled, and whichever word size a type keyword carries.
    Structural equality alone would make the name in {!statement_tokens} the
    only name that starts a statement, and [u64] the only word type. *)
let same_token (a : Parser.token) (b : Parser.token) =
  match (a, b) with
  | Parser.NID _, Parser.NID _ -> true
  | Parser.T_W _, Parser.T_W _ -> true
  | _ -> a = b

let item_start token = List.exists (same_token token) item_tokens
let item_only token = List.exists (same_token token) item_only_tokens
let type_start token = List.exists (same_token token) type_tokens

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

let statement_start token = List.exists (same_token token) statement_tokens

type token = {
  token : Parser.token;
  start_byte : int;
  end_byte : int;
  start_pos : Lexing.position;
  end_pos : Lexing.position;
  mutable depth : int;
      (** Braces opened and not closed before this token. A [}] carries the
          depth of the block it closes, so it is at least one. *)
}

type t = {
  name : string;
  text : string;  (** The buffer after the blankings, same length as given. *)
  tokens : token array;
  eof : Lexing.position;
  blanked : Jide_lang.Span.t list;
      (** The regions this scan rewrote, in byte order. Blanking keeps the
          length, so these are spans of the file the editor has open. A
          diagnostic inside one of them is about bytes the compiler never
          read. *)
}

let count t = Array.length t.tokens
let token t index = t.tokens.(index)
let text t = t.text
let name t = t.name
let eof_position t = t.eof
let blanked t = t.blanked
let span t ~start_byte ~end_byte = Jide_lang.Span.make ~start_byte ~end_byte

(** The three bytes of a byte order mark. Jasmin has no use for one and the
    lexer would reject it, so it is blanked before the recovery reads the
    buffer. The blanking keeps the length, so an offset in the result is the
    offset in the file the user has open. It is not done before the
    compiler's own parse: [jasminc] rejects a mark at byte zero, and the
    recovering parse reports that error rather than hiding it. *)
let bom_length text =
  if
    String.length text >= 3
    && text.[0] = '\xef'
    && text.[1] = '\xbb'
    && text.[2] = '\xbf'
  then 3
  else 0

let blank_bom text =
  let length = bom_length text in
  if length = 0 then text
  else
    String.make length ' ' ^ String.sub text length (String.length text - length)

(** [blank text ~from ~upto] replaces the bytes of that range with spaces,
    keeping newlines and the length. *)
let blank text ~from ~upto =
  let length = String.length text in
  let from = max 0 from and upto = min length upto in
  if upto <= from then text
  else
    let bytes = Bytes.of_string text in
    for index = from to upto - 1 do
      if Bytes.get bytes index <> '\n' then Bytes.set bytes index ' '
    done;
    Bytes.to_string bytes

(* The buffer holds more tokens than one scan will build. *)
exception Too_large

(* One lexing pass over the whole buffer. The lexer is the compiler's, so
   the tokens are exactly the ones the parser would be given. A lexical
   error does not throw the pass away: the tokens read before it are
   returned with the span the lexer stopped at, because they are a valid
   prefix of the file and the items they hold are worth keeping. *)
let lex_once ~name text =
  let lexbuf = Lexing.from_string text in
  Lexing.set_filename lexbuf name;
  let rec loop count acc =
    match Lexer.main lexbuf with
    | Parser.EOF -> (List.rev acc, None)
    | token ->
        (* [max_tokens] tokens are admitted; the next one is refused. The
           check is here, after the lexer answered, so a buffer of exactly
           the bound is not refused by the token it does not have. *)
        if count >= max_tokens then raise Too_large;
        let start_pos = Lexing.lexeme_start_p lexbuf in
        let end_pos = Lexing.lexeme_end_p lexbuf in
        loop (count + 1)
          ({
             token;
             start_byte = start_pos.Lexing.pos_cnum;
             end_byte = end_pos.Lexing.pos_cnum;
             start_pos;
             end_pos;
             depth = 0;
           }
          :: acc)
    | exception Syntax.ParseError (location, _) ->
        (List.rev acc, Some (Error_span.of_location location))
    | exception exn -> (List.rev acc, Some (Error.of_exn exn).span)
  in
  loop 0 []

(** [rest_of_line text byte] is the offset of the newline that ends the line
    [byte] is on, or the length of the buffer. *)
let rest_of_line text byte =
  let length = String.length text in
  let rec stop index =
    if index >= length || text.[index] = '\n' then index else stop (index + 1)
  in
  stop (max 0 (min byte (length - 1)))

(** Lex the buffer, blanking the region of a lexical error and trying again,
    so the items after the offending region are still seen. Returns the
    tokens, the buffer they were read from and the regions this scan
    blanked. Offsets are the same in either buffer because every blanking
    keeps the length.

    An error at the end of the buffer, which is where the lexer reports an
    unterminated comment, has no region left to blank: the tokens read
    before it are returned as they are. An error at a quote is an
    unterminated string, and a string runs to the next quote or to the end
    of the file, so its region is blanked rather than its line. *)
let lex_all ~name text =
  let rec attempt text blanked repairs =
    match lex_once ~name text with
    | tokens, None -> (tokens, text, List.rev blanked)
    | tokens, Some span ->
        let start = span.Jide_lang.Span.start_byte in
        let length = String.length text in
        if repairs <= 0 || start >= length then (tokens, text, List.rev blanked)
        else
          let upto =
            if text.[start] = '"' then length else rest_of_line text start
          in
          let text' = blank text ~from:start ~upto in
          if String.equal text' text then (tokens, text, List.rev blanked)
          else
            attempt text'
              (Jide_lang.Span.make ~start_byte:start ~end_byte:upto :: blanked)
              (repairs - 1)
  in
  attempt text [] max_lexical_repairs

(* The brace depth of every token. A [}] carries the depth of the block it
   closes, so the tokens that end a block and the tokens that begin the next
   item can be told apart. An unbalanced [}] stops at zero. *)
let with_depth tokens =
  let depth = ref 0 in
  Array.iter
    (fun entry ->
      let here = !depth in
      (match entry.token with
      | Parser.LBRACE -> incr depth
      | Parser.RBRACE -> if !depth > 0 then decr depth
      | _ -> ());
      entry.depth <- here)
    tokens;
  tokens

let eof_of ~name text =
  {
    Lexing.pos_fname = name;
    pos_lnum =
      1 + String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 text;
    pos_bol = String.length text;
    pos_cnum = String.length text;
  }

(** Why a buffer was refused: it has more bytes than one scan will read, or
    it holds more tokens than one scan will build. The two are reported
    apart, because the buffer that fills the byte ceiling exactly is refused
    for its token count and must not be told its size is the fault. *)
type refusal = Too_many_bytes | Too_many_tokens

(** The one diagnostic a buffer over {!max_bytes} or over {!max_tokens} gets.
    Its code is its own, because a refusal by policy is not a syntax error
    and no consumer should offer a syntax fix for it, and its span is empty:
    there is no byte of a file that was never read that a range could point
    at. *)
let too_large (refusal : refusal) =
  let reason =
    match refusal with
    | Too_many_bytes ->
        Printf.sprintf "the file is larger than %d bytes" max_bytes
    | Too_many_tokens ->
        Printf.sprintf "the file holds more than %d tokens" max_tokens
  in
  Jide_lang.Diagnostic.make ~code:"size" ~source:"jasmin"
    ~severity:Jide_lang.Diagnostic.Error
    (Jide_lang.Span.make ~start_byte:0 ~end_byte:0)
    (reason ^ ", which is too large to analyse")

(** [make ~name text] is the scan of [text], or the diagnostic that says the
    buffer is too large to look at. The byte order mark is blanked first, and
    the blanking is reported like every other one. *)
let make ~name text : (t, Jide_lang.Diagnostic.t) result =
  if String.length text > max_bytes then Error (too_large Too_many_bytes)
  else
    let bom = bom_length text in
    let text = blank_bom text in
    match lex_all ~name text with
    | exception Too_large -> Error (too_large Too_many_tokens)
    | tokens, text, blanked ->
        let blanked =
          if bom = 0 then blanked
          else Jide_lang.Span.make ~start_byte:0 ~end_byte:bom :: blanked
        in
        Ok
          {
            name;
            text;
            tokens = with_depth (Array.of_list tokens);
            eof = eof_of ~name text;
            blanked;
          }

(** The triple the incremental parser wants for [index]. *)
let triple t index =
  let entry = t.tokens.(index) in
  (entry.token, entry.start_pos, entry.end_pos)

let eof_triple t = (Parser.EOF, t.eof, t.eof)

(** [statement_boundary t index] is true when the token at [index] begins a
    statement: it is one of the statement's own first tokens and the token
    before it ends a statement or opens a block. The rule is deliberately
    lexical. Asking the parser whether a statement can start here costs a
    speculative reduction run at every identifier in every expression, and
    an expression's identifiers pass the grammar's test as well. *)
let statement_boundary t index =
  let entry = t.tokens.(index) in
  if not (statement_start entry.token) then false
  else if index = 0 then true
  else
    match t.tokens.(index - 1).token with
    | Parser.SEMICOLON | Parser.LBRACE | Parser.RBRACE -> true
    | _ -> false

(** [next_statement t ~from ~depth] is where a statement-level restart goes
    when the parse broke inside a body at brace depth [depth]:

    - the [}] that closes the block, so the body ends and its function
      still reaches the tree;
    - the [;] that ends the broken statement, resuming after it;
    - the next statement that begins at that depth, by
      {!statement_boundary}.

    A bare identifier or a parenthesis is not a target on its own: those
    start expressions as well as statements, and resuming at one of them
    inside a broken statement produces a tree the user never wrote. *)
let next_statement t ~from ~depth =
  let length = count t in
  let rec loop index =
    if index >= length then None
    else
      let entry = t.tokens.(index) in
      if entry.token = Parser.RBRACE && entry.depth <= depth then Some index
      else if entry.token = Parser.SEMICOLON && entry.depth = depth then
        Some (index + 1)
      else if entry.depth = depth && statement_boundary t index then Some index
      else loop (index + 1)
  in
  loop (max 0 from)

(** The byte an item starts at, for a prefix that ends the last closed item
    exactly where the next one begins. *)
let start_byte t index =
  if index >= count t then String.length t.text else t.tokens.(index).start_byte
