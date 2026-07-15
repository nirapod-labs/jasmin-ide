(** The mutation harness of the batch checker.

    One Jasmin file is altered in one place at a time: one token deleted, one
    token inserted, one token duplicated, [;] and [}] swapped, or the text cut
    at a token boundary. Every altered text goes through the checker, and the
    run reports the three properties a recovering front end has to keep:

    - no exception and no hang, in every case;
    - the outline of the items away from the edit unchanged, in every case
      whose altered text still lexes;
    - at most two errors per single-token mutation, in at least 95% of the
      single-token cases, with the error-count distribution reported.

    The tokens are the compiler's own: the harness drives the vendored lexer,
    so a token boundary is one the compiler would see. The outline is the
    sequence of item headers read from those tokens ([fn], [proc], [param],
    [global], [type] and [require]), each a keyword followed by the name the
    grammar puts there. Reading it from tokens rather than from a parse tree
    keeps it available for a text the parser refuses, which is the normal
    state of a mutated file; a text the lexer itself refuses has no outline
    and is counted as such rather than as a difference. *)

type token = { start : int; stop : int; kind : kind }

and kind =
  | Fn
  | Param
  | Global
  | Type
  | Require
  | Ident of string
  | Text of string
  | Lparen
  | Equals
  | Other

let kind_of (token : Jasmin.Parser.token) : kind option =
  match token with
  | Jasmin.Parser.EOF -> None
  | Jasmin.Parser.FN -> Some Fn
  | Jasmin.Parser.PARAM -> Some Param
  | Jasmin.Parser.GLOBAL -> Some Global
  | Jasmin.Parser.TYPE -> Some Type
  | Jasmin.Parser.REQUIRE -> Some Require
  | Jasmin.Parser.NID name -> Some (Ident name)
  | Jasmin.Parser.STRING text -> Some (Text text)
  | Jasmin.Parser.LPAREN -> Some Lparen
  | Jasmin.Parser.EQ -> Some Equals
  | _ -> Some Other

let lex (text : string) : token list option =
  let lexbuf = Lexing.from_string text in
  let rec loop acc =
    match Jasmin.Lexer.main lexbuf with
    | exception _ -> None
    | token -> (
        let start = lexbuf.Lexing.lex_start_p.Lexing.pos_cnum in
        let stop = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum in
        match kind_of token with
        | None -> Some (List.rev acc)
        | Some kind -> loop ({ start; stop; kind } :: acc))
  in
  loop []

type item = { kind : string; name : string; start : int }
(** One item of the outline: what it is, what it is called and where its
    header starts in the text. *)

let outline_of_tokens (tokens : token list) : item list =
  let tokens = Array.of_list tokens in
  let length = Array.length tokens in
  let at index = if index < length then Some tokens.(index) else None in
  let header kind name index acc =
    ({ kind; name; start = tokens.(index).start } : item) :: acc
  in
  (* A declaration is [keyword type* name = ...], so the name is the last
     identifier before the equals sign. The search is bounded because the
     type is a short sequence and an unbounded one would read the rest of the
     file to find the next assignment. *)
  let rec name_before_equals index fuel =
    if fuel = 0 || index + 1 >= length then None
    else
      match (tokens.(index).kind, tokens.(index + 1).kind) with
      | Ident name, Equals -> Some name
      | _ -> name_before_equals (index + 1) (fuel - 1)
  in
  let rec declaration kind index acc =
    match name_before_equals (index + 1) 6 with
    | None -> loop acc (index + 1)
    | Some name -> loop (header kind name index acc) (index + 2)
  and loop acc index =
    if index >= length then List.rev acc
    else
      match (tokens.(index).kind, at (index + 1), at (index + 2)) with
      | Fn, Some { kind = Ident name; _ }, Some { kind = Lparen; _ } ->
          loop (header "fn" name index acc) (index + 3)
      | Param, _, _ -> declaration "param" index acc
      | Global, _, _ -> declaration "global" index acc
      | Type, _, _ -> declaration "type" index acc
      | Require, Some { kind = Text name; _ }, _ ->
          loop (header "require" name index acc) (index + 2)
      | _ -> loop acc (index + 1)
  in
  loop [] 0

let outline (text : string) : item list option =
  match lex text with
  | None -> None
  | Some tokens -> Some (outline_of_tokens tokens)

type mutation = { label : string; text : string; start : int; stop : int }
(** One alteration of one text. [start] and [stop] are the bytes of the
    original text the alteration replaces; an insertion has [stop = start]. *)

let splice text start stop inserted =
  String.sub text 0 start ^ inserted
  ^ String.sub text stop (String.length text - stop)

let token_text text (token : token) =
  String.sub text token.start (token.stop - token.start)

(** The tokens an insertion may put in. None of them is an item keyword, so
    an insertion can never add or remove an outline entry of its own: the
    outline is what the mutation must not disturb, not what it edits. *)
let insertions =
  [|
    ";";
    "=";
    "(";
    ")";
    "{";
    "}";
    "reg";
    "u64";
    "return";
    "x";
    ",";
    "[";
    "]";
    "+";
    "-";
    "/* edited */";
  |]

let deletion text (tokens : token array) state =
  let count = Array.length tokens in
  if count = 0 then []
  else
    let token = tokens.(Random.State.int state count) in
    [
      {
        label = "delete";
        text = splice text token.start token.stop "";
        start = token.start;
        stop = token.stop;
      };
    ]

let insertion text (tokens : token array) state =
  let count = Array.length tokens in
  if count = 0 then []
  else
    let token = tokens.(Random.State.int state count) in
    let inserted =
      insertions.(Random.State.int state (Array.length insertions))
    in
    let at = token.stop in
    [
      {
        label = "insert";
        text = splice text at at (" " ^ inserted);
        start = at;
        stop = at;
      };
    ]

(** A duplicated token is the one insertion that carries a name from the
    file: it is the compiler's own text for the token, put back beside it. *)
let duplication text (tokens : token array) state =
  let count = Array.length tokens in
  if count = 0 then []
  else
    let token = tokens.(Random.State.int state count) in
    let at = token.stop in
    [
      {
        label = "duplicate";
        text = splice text at at (" " ^ token_text text token);
        start = at;
        stop = at;
      };
    ]

