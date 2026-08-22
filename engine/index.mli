(** One file's untyped tree, indexed.

    The compiler's typed program keeps each variable's declaration and each
    use, but it drops call-target names, intrinsic names, type-alias names and
    everything of an item that failed to type. So the engine keeps its own
    index over the syntax tree (design section 9): every identifier
    occurrence with its byte range and its kind, every declaration with its
    name range and its item range, and the [require] edges.

    This module knows syntax only. {!Resolve} walks the same trees and
    decides what each occurrence names. *)

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
  | Call  (** the function position of a call *)
  | Var  (** a variable read or write *)
  | Type  (** an alias used as a type *)
  | Prim  (** the name after [\#] in a primitive call *)
  | Annot  (** an annotation label *)
  | Include_path  (** the [from NAME] of a require *)
  | Declared of kind  (** a name this occurrence introduces *)

type occurrence = {
  span : Jide_lang.Span.t;
      (** The whole identifier. A qualified name [A::B::x] has its whole
          range here, because that is all the compiler's location carries. *)
  last : Jide_lang.Span.t;
      (** The last segment of a qualified name, [span] when there is none. *)
  name : string;
  role : role;
  value : string option;
      (** The string an annotation carries, when it carries one. *)
}

type declaration = {
  kind : kind;
  name : string;  (** the name as written, not qualified *)
  file : string;
  name_span : Jide_lang.Span.t;
  item_span : Jide_lang.Span.t;
  detail : string option;
      (** A signature or a type, rendered from the source bytes, for hover.
          It is a substring of the file, never invented text. *)
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
  written : string;  (** the file name as written, without quotes *)
  written_span : Jide_lang.Span.t;  (** the string literal, quotes included *)
  item_span : Jide_lang.Span.t;
}

type t = {
  file : string;  (** as given to {!of_text} *)
  text : string;  (** the text as given, byte-order mark included *)
  items : Jasmin.Syntax.pprogram;
  outline : item list;  (** the tree as an outline, namespaces nested *)
  declarations : declaration list;
  occurrences : occurrence list;  (** in source order *)
  requires : require list;  (** in source order *)
}

val of_text : file:string -> string -> t * Jide_lang.Diagnostic.t list
(** [of_text ~file text] parses [text] and indexes it. A leading UTF-8
    byte-order mark is removed before the parse and every range is shifted
    back by it, so a range indexes [text] as the caller holds it. The
    diagnostics are the parser's, shifted the same way; a file that does not
    parse has no occurrences, no declarations and no items. Nothing here
    raises. *)

val normalise : string -> string
(** One spelling for one path: absolute where it was absolute, with no [.]
    or [..] left, exactly as {!Context.normalise} writes it. Two paths that
    name the same file through different symbolic links stay different. *)

val equal_declaration : declaration -> declaration -> bool
(** [equal_declaration a b] is the identity of design section 9: the same
    absolute file and the same declaration byte offset. A compiler unique id
    is never part of it. *)

val find_occurrence : t -> byte:int -> occurrence option
(** [find_occurrence t ~byte] is the occurrence whose span holds [byte],
    the smallest one when two hold it. *)

val find_declaration : t -> name_start:int -> declaration option
(** [find_declaration t ~name_start] is the declaration whose name begins at
    that byte. *)

val text_of_span : string -> Jide_lang.Span.t -> string
(** [text_of_span text span] is the part of [text] the span covers, with
    both ends clamped and an empty string for an empty range. *)
