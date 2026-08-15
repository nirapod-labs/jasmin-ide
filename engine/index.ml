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

