(* The index of one untyped tree: identifiers, declarations and requires.

   Section 9 of the design asks for the syntax tree the compiler discards.
   The walk here is deliberately syntax-shaped: it visits every [pident] the
   grammar has, in source order, and never asks what a name means. The
   resolver does that, over the same tree, with pretyping's rules. *)

open Jasmin

type kind =
  | Function
  | Parameter
  | Variable
  | Global
  | Namespace
  | Type_alias
  | Exec
  | Require
  | Intrinsic
  | Annotation
  | Include
  | Unknown

type role =
  | Call
  | Var
  | Type
  | Prim
  | Annot
  | Include_path
  | Declared of kind

type occurrence = {
  span : Jide_lang.Span.t;
  last : Jide_lang.Span.t;
  name : string;
  role : role;
  value : string option;
}

type declaration = {
  kind : kind;
  name : string;
  file : string;
  name_span : Jide_lang.Span.t;
  item_span : Jide_lang.Span.t;
  detail : string option;
}

type item = {
  item_kind : kind;
  item_name : string option;
  item_name_span : Jide_lang.Span.t option;
  item_span : Jide_lang.Span.t;
  item_children : item list;
}

type require = {
  from : (string * Jide_lang.Span.t) option;
  written : string;
  written_span : Jide_lang.Span.t;
  item_span : Jide_lang.Span.t;
}

type t = {
  file : string;
  text : string;
  items : Syntax.pprogram;
  outline : item list;
  declarations : declaration list;
  occurrences : occurrence list;
  requires : require list;
}

(* ------------------------------------------------------------------ *)
(* Ranges *)

let span_of_location (location : Location.t) : Jide_lang.Span.t =
  let start = max 0 location.Location.loc_bchar in
  let stop = max start location.Location.loc_echar in
  Jide_lang.Span.make ~start_byte:start ~end_byte:stop

let span_of_ident (ident : string Location.located) =
  span_of_location (Location.loc ident)

let text_of_span (text : string) (span : Jide_lang.Span.t) =
  let length = String.length text in
  let start = max 0 (min span.Jide_lang.Span.start_byte length) in
  let stop = max start (min span.Jide_lang.Span.end_byte length) in
  String.sub text start (stop - start)

let collapse (text : string) =
  let buffer = Buffer.create (min (String.length text) 256) in
  let pending = ref false in
  String.iter
    (fun character ->
      match character with
      | ' ' | '\t' | '\r' | '\n' ->
          if Buffer.length buffer > 0 then pending := true
      | _ ->
          if !pending then Buffer.add_char buffer ' ';
          pending := false;
          Buffer.add_char buffer character)
    text;
  let text = Buffer.contents buffer in
  if String.length text > 240 then String.sub text 0 237 ^ "..." else text

(* One spelling per path, as Context writes it. Index does not depend on
   Context (Context is about files, this module about a tree), so the small
   fold is repeated here and the two are tested to agree. *)
let normalise (path : string) =
  let absolute = String.length path > 0 && path.[0] = '/' in
  let parts = String.split_on_char '/' path in
  let rec fold stack = function
    | [] -> List.rev stack
    | ("" | ".") :: rest -> fold stack rest
    | ".." :: rest -> (
        match stack with [] -> fold stack rest | _ :: stack -> fold stack rest)
    | part :: rest -> fold (part :: stack) rest
  in
  let body = String.concat "/" (fold [] parts) in
  if absolute then "/" ^ body else if body = "" then "." else body

(* [A::B::x] has one range in the compiler's location. The last segment is
   read from the source bytes: it ends where the identifier ends and starts
   one segment length before it, and only when the two bytes before it are
   [::]. A space around [::] gives the whole range, as the design says. *)
let last_segment (text : string) (span : Jide_lang.Span.t) (name : string) =
  let name_length = String.length name in
  let rec find index =
    if index < 0 then None
    else if
      index + 1 < name_length && name.[index] = ':' && name.[index + 1] = ':'
    then Some index
    else find (index - 1)
  in
  match find (name_length - 2) with
  | None -> span
  | Some index ->
      let segment = name_length - index - 2 in
      let start = span.Jide_lang.Span.end_byte - segment in
      let preceding = start - 2 in
      if
        start >= span.Jide_lang.Span.start_byte
        && preceding >= span.Jide_lang.Span.start_byte
        && preceding + 1 < String.length text
        && text.[preceding] = ':' && text.[preceding + 1] = ':'
      then Jide_lang.Span.make ~start_byte:start ~end_byte:span.Jide_lang.Span.end_byte
      else span

(* ------------------------------------------------------------------ *)
(* Storage and types as text, for hover *)

let writable = function
  | None -> ""
  | Some `Constant -> " const"
  | Some `Writable -> " mut"

let pointer_text = function
  | `Direct -> ""
  | `Pointer mode -> writable mode ^ " ptr"

let storage_text (storage : Syntax.pstorage) =
  match storage with
  | `Reg pointer -> "reg" ^ pointer_text pointer
  | `Stack pointer -> "stack" ^ pointer_text pointer
  | `Inline -> "inline"
  | `Global -> "global"

(* ------------------------------------------------------------------ *)
(* The walk *)

