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

