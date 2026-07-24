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

(** Two tokens are the same token when their constructors match. A token that
    carries a value, which here is always a name, matches any value of its
    constructor: the sets below name token kinds, and a restart point must be
    found whatever the identifier at it happens to be spelled. Structural
    equality alone would make the name in {!statement_tokens} the only name
    that starts a statement. *)
let same_token (a : Parser.token) (b : Parser.token) =
  match (a, b) with Parser.NID _, Parser.NID _ -> true | _ -> a = b

let item_start token = List.exists (same_token token) item_tokens

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
let span t ~start_byte ~end_byte = Jide_lang.Span.make ~start_byte ~end_byte

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

(* A lexing error, carrying the bytes the lexer stopped at. *)
exception Stopped of Jide_lang.Span.t

(* One lexing pass over the whole buffer. The lexer is the compiler's, so
   the tokens are exactly the ones the parser would be given. *)
let lex_once ~name text =
  let lexbuf = Lexing.from_string text in
  Lexing.set_filename lexbuf name;
  let rec loop acc =
    match Lexer.main lexbuf with
    | Parser.EOF -> List.rev acc
    | token ->
        let start_pos = Lexing.lexeme_start_p lexbuf in
        let end_pos = Lexing.lexeme_end_p lexbuf in
        loop
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
        raise (Stopped (Error_span.of_location location))
    | exception exn -> raise (Stopped (Error.of_exn exn).span)
  in
  loop []

(** [blank_line text byte] blanks the rest of the line [byte] is on. A
    lexing error is reported at the first byte the lexer could not accept,
    and the line is the smallest region that both removes the offending
    bytes and leaves the line numbering of the rest of the file alone. *)
let blank_line text byte =
  let length = String.length text in
  let byte = max 0 (min byte (length - 1)) in
  let rec stop index =
    if index >= length || text.[index] = '\n' then index else stop (index + 1)
  in
  blank text ~from:byte ~upto:(stop byte)

(** Lex the buffer, blanking the line of a lexical error and trying again,
    so the items after a line the lexer rejects are still seen. Returns the
    tokens and the buffer they were read from; offsets are the same in
    either buffer because every blanking keeps the length. *)
let lex_all ~name text =
  let rec attempt text repairs =
    match lex_once ~name text with
    | tokens -> (tokens, text)
    | exception Stopped span ->
        if repairs <= 0 then ([], text)
        else
          let text' = blank_line text span.Jide_lang.Span.start_byte in
          if String.equal text' text then ([], text)
          else attempt text' (repairs - 1)
  in
  attempt text max_lexical_repairs

(* The brace depth of every token. A [}] carries the depth of the block it
   closes, so the tokens that end a block and the tokens that begin the next
   item can be told apart. An unbalanced [}] stops at zero. *)
let with_depth tokens =
  let depth = ref 0 in
  Array.map
    (fun entry ->
      let here = !depth in
      (match entry.token with
      | Parser.LBRACE -> incr depth
      | Parser.RBRACE -> if !depth > 0 then decr depth
      | _ -> ());
      { entry with depth = here })
    tokens

let eof_of ~name text =
  {
    Lexing.pos_fname = name;
    pos_lnum =
      1 + String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 text;
    pos_bol = String.length text;
    pos_cnum = String.length text;
  }

(** The one diagnostic a buffer over [max_bytes] gets. *)
let too_large () =
  Jide_lang.Diagnostic.make ~code:"parse" ~source:"jasmin"
    ~severity:Jide_lang.Diagnostic.Error
    (Jide_lang.Span.make ~start_byte:0 ~end_byte:0)
    (Printf.sprintf
       "the file is larger than %d bytes, which is too large to analyse"
       max_bytes)

(** [make ~name text] is the scan of [text], or the diagnostic that says the
    buffer is too large to look at. The byte order mark is blanked first. *)
let make ~name text : (t, Jide_lang.Diagnostic.t) result =
  if String.length text > max_bytes then Error (too_large ())
  else
    let text = blank_bom text in
    let tokens, text = lex_all ~name text in
    Ok
      {
        name;
        text;
        tokens = with_depth (Array.of_list tokens);
        eof = eof_of ~name text;
      }

(** The triple the incremental parser wants for [index]. *)
let triple t index =
  let entry = t.tokens.(index) in
  (entry.token, entry.start_pos, entry.end_pos)

let eof_triple t = (Parser.EOF, t.eof, t.eof)

(** [next_item t ~from] is the token index the driver restarts an item at:
    the first token at or after [from] that can begin an item, among those
    with the smallest brace depth the rest of the buffer offers. The depth
    matters because a missing [}] leaves every later token one level deeper
    than it reads; the smallest depth is the item level even then. *)
let next_item t ~from =
  let length = count t in
  let from = max 0 from in
  if from >= length then None
  else
    let best_depth = ref max_int and best = ref None in
    for index = from to length - 1 do
      let entry = t.tokens.(index) in
      if item_start entry.token && entry.depth < !best_depth then (
        best_depth := entry.depth;
        best := Some index)
    done;
    !best

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
