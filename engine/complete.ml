(* Completion and signature help, from a snapshot.

   Nothing here parses, resolves or checks: the input is the index of one
   file, the resolver over it and the typed facts of the last check. The
   parser drops an unfinished call, so a call is read from the text before
   the offset and never from the tree. *)

open Jide_jasmin

type kind =
  | Keyword
  | Function
  | Parameter
  | Variable
  | Global
  | Type_alias
  | Annotation_key

type item = { label : string; kind : kind; detail : string option }

type completion = {
  items : item list;
  prefix : string;
  span : Jide_lang.Span.t;
}

type trigger = Hash | Colon

(* The lexer's own table, so a keyword added to the language reaches
   completion without a second list to keep in step. [_] is a keyword of the
   grammar and never a candidate: it is written where a name is dropped. *)
let keywords =
  List.filter_map
    (fun (word, _token) -> if String.equal word "_" then None else Some word)
    Jasmin.Lexer._keywords

(* The annotation labels the compiler reads, sorted. Each one is a name the
   compiler's own code looks for: the function annotations of
   [FunctionAnnotations], [ct] and [sct] of [SecurityAnnotations], and the
   instruction and variable labels [align], [abbrev], [inline], [inplace],
   [keep], [mmx], [msf], [nospill], [sign] and [spill_to_mmx]. *)
let annotation_keys =
  [
    "abbrev";
    "align";
    "calldepth";
    "ct";
    "inline";
    "inplace";
    "keep";
    "mmx";
    "msf";
    "nospill";
    "returnaddress";
    "sct";
    "sign";
    "spill_to_mmx";
    "stackalign";
    "stackallocsize";
    "stacksize";
    "stackzero";
    "stackzerosize";
  ]

(* ------------------------------------------------------------------ *)
(* Words *)

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_blank = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

(* [word_start text byte] is where the word that ends at [byte] begins. A
   qualified name is one word when no space surrounds its [::], which is the
   rule the index uses for the same names. *)
let word_start (text : string) (byte : int) =
  let byte = max 0 (min byte (String.length text)) in
  let rec back index =
    if index <= 0 then 0
    else if is_ident_char text.[index - 1] then back (index - 1)
    else if index >= 2 && text.[index - 1] = ':' && text.[index - 2] = ':' then
      if index > 2 && is_ident_char text.[index - 3] then back (index - 2)
      else index
    else index
  in
  back byte

let starts_with ~(prefix : string) (label : string) =
  String.starts_with ~prefix label

(* The annotation keys are what a [#] asks for, and what the design's [:]
   trigger asks for. The [#] is read from the text when the client names no
   trigger, which is what a request arriving after the character looks like
   when the client sends no trigger kind. *)
let annotation_context (text : string) ~(start : int) (trigger : trigger option)
    =
  match trigger with
  | Some Hash | Some Colon -> true
  | None ->
      (start >= 1 && text.[start - 1] = '#')
      || (start >= 2 && text.[start - 1] = '[' && text.[start - 2] = '#')

(* ------------------------------------------------------------------ *)
(* Completion *)

let kind_of_declaration (kind : Index.kind) =
  match kind with
  | Index.Function -> Some Function
  | Index.Parameter -> Some Parameter
  | Index.Variable -> Some Variable
  | Index.Global -> Some Global
  | Index.Type_alias -> Some Type_alias
  | Index.Namespace | Index.Exec | Index.Require | Index.Intrinsic
  | Index.Annotation | Index.Include | Index.Unknown ->
      None

let rank = function
  | Function -> 0
  | Parameter -> 1
  | Variable -> 2
  | Global -> 3
  | Type_alias -> 4
  | Annotation_key -> 5
  | Keyword -> 6

let compare_item left right =
  match String.compare left.label right.label with
  | 0 -> Int.compare (rank left.kind) (rank right.kind)
  | order -> order

let items_of_scope (scope : Resolve.scope) =
  List.filter_map
    (fun (binding : Resolve.binding) ->
      Option.map
        (fun kind ->
          {
            label = binding.Resolve.name;
            kind;
            detail = binding.Resolve.declaration.Index.detail;
          })
        (kind_of_declaration binding.Resolve.declaration.Index.kind))
    (scope.types @ scope.funs @ scope.vars)

let empty_scope = { Resolve.types = []; funs = []; vars = [] }

let completion ?trigger ~(index : Index.t) ~(resolve : Resolve.t) ~(byte : int)
    () =
  let text = index.Index.text in
  let byte = max 0 (min byte (String.length text)) in
  let start = word_start text byte in
  let prefix = String.sub text start (byte - start) in
  let candidates =
    if annotation_context text ~start trigger then
      List.map
        (fun key -> { label = key; kind = Annotation_key; detail = None })
        annotation_keys
    else
      let scope =
        match Resolve.scope_at resolve ~file:index.Index.file ~byte with
        | Some scope -> scope
        | None -> empty_scope
      in
      List.map
        (fun word -> { label = word; kind = Keyword; detail = None })
        keywords
      @ items_of_scope scope
  in
  let items =
    candidates
    |> List.filter (fun item -> starts_with ~prefix item.label)
    |> List.sort_uniq compare_item
  in
  { items; prefix; span = Jide_lang.Span.make ~start_byte:start ~end_byte:byte }

(* ------------------------------------------------------------------ *)
(* Signature help *)

type signature = {
  label : string;
  parameters : string list;
  active : int;
  callee : string;
}

type delimiter = { character : char; position : int; commas : int }

let skip_string (text : string) limit index =
  let rec loop index =
    if index >= limit then limit
    else
      match text.[index] with
      | '\\' -> loop (index + 2)
      | '"' -> index + 1
      | _ -> loop (index + 1)
  in
  loop index

let skip_line (text : string) limit index =
  let rec loop index =
    if index >= limit then limit
    else if text.[index] = '\n' then index + 1
    else loop (index + 1)
  in
  loop index

let skip_comment (text : string) limit index =
  let rec loop index =
    if index + 1 >= limit then limit
    else if text.[index] = '*' && text.[index + 1] = '/' then index + 2
    else loop (index + 1)
  in
  loop index

(* The delimiters left open at the offset. Strings and comments are skipped,
   so a bracket or a comma in a message or in a [`ct`] string counts for
   nothing, and each open delimiter counts the commas written directly
   inside it, which is what the active parameter is read from. *)
let rec scan (text : string) limit index stack =
  if index >= limit then stack
  else
    match text.[index] with
    | '"' -> scan text limit (skip_string text limit (index + 1)) stack
    | '/' when index + 1 < limit && text.[index + 1] = '/' ->
        scan text limit (skip_line text limit (index + 2)) stack
    | '/' when index + 1 < limit && text.[index + 1] = '*' ->
        scan text limit (skip_comment text limit (index + 2)) stack
    | ('(' | '[' | '{') as character ->
        scan text limit (index + 1)
          ({ character; position = index; commas = 0 } :: stack)
    | ')' | ']' | '}' -> (
        match stack with
        | [] -> scan text limit (index + 1) []
        | _ :: rest -> scan text limit (index + 1) rest)
    | ',' -> (
        match stack with
        | [] -> scan text limit (index + 1) []
        | top :: rest ->
            scan text limit (index + 1)
              ({ top with commas = top.commas + 1 } :: rest))
    | _ -> scan text limit (index + 1) stack

let rec open_paren = function
  | [] -> None
  | entry :: rest ->
      if entry.character = '(' then Some entry else open_paren rest

(* [call_at text byte] is the call the offset is in: the callee as written,
   the byte its name starts at, the number of arguments already finished, and
   whether the call is a primitive, which is written [\#name(...)]. *)
let call_at (text : string) (byte : int) =
  let limit = max 0 (min byte (String.length text)) in
  match open_paren (scan text limit 0 []) with
  | None -> None
  | Some paren ->
      let rec before index =
        if index > 0 && is_blank text.[index - 1] then before (index - 1)
        else index
      in
      let stop = before paren.position in
      if stop = 0 || not (is_ident_char text.[stop - 1]) then None
      else
        let start = word_start text stop in
        let callee = String.sub text start (stop - start) in
        if String.equal callee "" then None
        else
          Some
            (callee, start, paren.commas, start >= 1 && text.[start - 1] = '#')

(* The parameters of a signature written in the source: the groups between
   its parentheses, split at the commas that are not inside a nested pair. *)
let split_at_commas (body : string) =
  let parts = ref [] in
  let current = Buffer.create 32 in
  let depth = ref 0 in
  String.iter
    (fun character ->
      match character with
      | '(' | '[' | '{' ->
          incr depth;
          Buffer.add_char current character
      | ')' | ']' | '}' ->
          decr depth;
          Buffer.add_char current character
      | ',' when !depth = 0 ->
          parts := Buffer.contents current :: !parts;
          Buffer.clear current
      | character -> Buffer.add_char current character)
    body;
  parts := Buffer.contents current :: !parts;
  List.rev !parts

let parameters_of_detail (detail : string) =
  match String.index_opt detail '(' with
  | None -> []
  | Some open_ ->
      let rec matching depth index =
        if index >= String.length detail then String.length detail
        else
          match detail.[index] with
          | '(' -> matching (depth + 1) (index + 1)
          | ')' -> if depth = 1 then index else matching (depth - 1) (index + 1)
          | _ -> matching depth (index + 1)
      in
      let close = matching 1 (open_ + 1) in
      let body = String.sub detail (open_ + 1) (max 0 (close - open_ - 1)) in
      List.filter
        (fun part -> not (String.equal part ""))
        (List.map String.trim (split_at_commas body))

let label (callee : string) (parameters : string list) (results : string list) =
  Printf.sprintf "%s(%s)%s" callee
    (String.concat ", " parameters)
    (match results with
    | [] -> ""
    | results -> " -> " ^ String.concat ", " results)

(* The signature the compiler left for one declaration. Facts hold the items
   of one checked file, so they answer only for a declaration of that file,
   and the identity is the design's: the file and the byte where the name
   starts. *)
let facts_signature (facts : Facts.t) (index : Index.t)
    (declaration : Index.declaration) =
  if
    not
      (String.equal
         (Index.normalise declaration.Index.file)
         (Index.normalise index.Index.file))
  then None
  else
    List.find_map
      (fun (item : Facts.item) ->
        if
          item.Facts.i_kind = Facts.Function
          && item.Facts.i_name_span.Jide_lang.Span.start_byte
             = declaration.Index.name_span.Jide_lang.Span.start_byte
        then
          Option.map
            (fun (signature : Facts.fun_sig) ->
              (signature.Facts.fs_in, signature.Facts.fs_out))
            item.Facts.i_sig
        else None)
      facts.Facts.items

(* The declaration a call names: the answer the resolver recorded for the
   callee when the parser kept it, and otherwise the function the scope holds
   under that name, which is what an unfinished call leaves to go on. *)
let declaration_of_call (resolve : Resolve.t) ~(file : string) ~(start : int)
    ~(callee : string) =
  match Resolve.answer_at resolve ~file ~byte:start with
  | Some (_, Resolve.Resolved declaration)
    when declaration.Index.kind = Index.Function ->
      Some declaration
  | _ -> (
      match Resolve.scope_at resolve ~file ~byte:start with
      | None -> None
      | Some scope ->
          List.find_opt
            (fun (binding : Resolve.binding) ->
              String.equal binding.Resolve.name callee)
            scope.Resolve.funs
          |> Option.map (fun (binding : Resolve.binding) ->
                 binding.Resolve.declaration))

let signature_help ?(facts = Facts.empty) ~(index : Index.t)
    ~(resolve : Resolve.t) ~(byte : int) () =
  match call_at index.Index.text byte with
  | None -> None
  | Some (callee, start, commas, intrinsic) -> (
      if intrinsic then None
      else
        match
          declaration_of_call resolve ~file:index.Index.file ~start ~callee
        with
        | None -> None
        | Some declaration -> (
            match facts_signature facts index declaration with
            | Some (parameters, results) ->
                Some
                  {
                    label = label callee parameters results;
                    parameters;
                    active = commas;
                    callee;
                  }
            | None -> (
                match declaration.Index.detail with
                | None -> None
                | Some detail ->
                    let parameters = parameters_of_detail detail in
                    let label =
                      if String.starts_with ~prefix:"fn " detail then
                        String.sub detail 3 (String.length detail - 3)
                      else detail
                    in
                    Some { label; parameters; active = commas; callee })))
