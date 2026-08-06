(** The tokens of a Jasmin source, as the compiler's lexer splits it.

    The harness mutates one lexical token at a time, so a token has to mean
    here what it means to the compiler: the same multi-character operators,
    the same identifiers and numbers, and the same two comment forms. A
    string or a number is one token even when it holds spaces or a semicolon.

    Tokenising is total. A byte the compiler would reject still starts a
    token of its own, so any text can be measured, including the text a
    mutation made invalid. Comment bytes are skipped, as the lexer skips
    them, so no mutation lands inside a comment. *)

type kind = Ident | Number | String_lit | Punct

type t = {
  start_byte : int;  (** First byte of the token, zero-based. *)
  end_byte : int;  (** One past its last byte. *)
  kind : kind;
  text : string;  (** [String.sub source start_byte (end_byte - start_byte)]. *)
}

let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let is_digit c = c >= '0' && c <= '9'
let is_ident_char c = is_ident_start c || is_digit c

(* The operators the lexer spells with more than one character, longest
   first so that a prefix cannot win: [>>r] is one token and [>>] another,
   and [<=u] is one token while [<=] alone is one too. *)
let multi_character =
  [
    "#unaligned";
    "#aligned";
    ">>r";
    "<<r";
    ">>u";
    ">>s";
    ">=u";
    ">=s";
    "<=u";
    "<=s";
    ">>";
    "<<";
    "<=";
    ">=";
    "==";
    "!=";
    "&&";
    "||";
    "->";
    "::";
    "#[";
    "<u";
    "<s";
    ">u";
    ">s";
    "/u";
    "/s";
    "%u";
    "%s";
  ]

let tokenize text =
  let length = String.length text in
  let tokens = ref [] in
  let emit kind start_byte end_byte =
    tokens :=
      {
        start_byte;
        end_byte;
        kind;
        text = String.sub text start_byte (end_byte - start_byte);
      }
      :: !tokens
  in
  (* A line comment runs to the newline, which stays part of the skipped
     run so that the next token starts after it. *)
  let rec line_comment index =
    if index >= length then length
    else if text.[index] = '\n' then index + 1
    else line_comment (index + 1)
  in
  (* The compiler's comments nest, so this one counts depth. *)
  let rec block_comment index depth =
    if index >= length then length
    else if index + 1 < length && text.[index] = '*' && text.[index + 1] = '/'
    then if depth <= 1 then index + 2 else block_comment (index + 2) (depth - 1)
    else if index + 1 < length && text.[index] = '/' && text.[index + 1] = '*'
    then block_comment (index + 2) (depth + 1)
    else block_comment (index + 1) depth
  in
  (* A string runs to its closing quote; a backslash takes the byte after it
     with it, so a quote inside a string does not close it. An unterminated
     string ends at the end of the text rather than failing. *)
  let rec string_lit index =
    if index >= length then length
    else
      match text.[index] with
      | '\\' -> if index + 1 < length then string_lit (index + 2) else length
      | '"' -> index + 1
      | _ -> string_lit (index + 1)
  in
  let rec run_of is_char index =
    if index < length && is_char text.[index] then run_of is_char (index + 1)
    else index
  in
  let punct index =
    let rec first = function
      | [] -> None
      | rule :: rest ->
          let rule_length = String.length rule in
          if
            index + rule_length <= length
            && String.sub text index rule_length = rule
          then Some (index + rule_length)
          else first rest
    in
    first multi_character
  in
  let rec loop index =
    if index >= length then ()
    else
      match text.[index] with
      | ' ' | '\t' | '\r' | '\n' -> loop (index + 1)
      | '/' when index + 1 < length && text.[index + 1] = '/' ->
          loop (line_comment (index + 2))
      | '/' when index + 1 < length && text.[index + 1] = '*' ->
          loop (block_comment (index + 2) 1)
      | '"' ->
          let stop = string_lit (index + 1) in
          emit String_lit index stop;
          loop stop
      | c when is_ident_start c ->
          let stop = run_of is_ident_char (index + 1) in
          emit Ident index stop;
          loop stop
      | c when is_digit c ->
          (* The lexer's numbers carry [_], an [x], [b] or [o] base marker
             and hex digits, so an alphanumeric run is one number here. *)
          let stop = run_of is_ident_char (index + 1) in
          emit Number index stop;
          loop stop
      | _ -> (
          match punct index with
          | Some stop ->
              emit Punct index stop;
              loop stop
          | None ->
              emit Punct index (index + 1);
              loop (index + 1))
  in
  loop 0;
  List.rev !tokens

