(** Workspace symbols: the declarations of the project's indexed files.

    A declaration is a name the index records as introduced: a function, a
    parameter, a variable, a global, a type alias or a namespace. Nothing here
    resolves, parses or checks: the caller hands the indexes it already holds
    and the answer is a filter over them, so a query costs nothing beyond a
    walk of the declarations. *)

type symbol = {
  file : string;  (** the path the index holds, as the index holds it *)
  name : string;  (** the name as written, not qualified *)
  span : Jide_lang.Span.t;  (** the declaration's whole item *)
  name_span : Jide_lang.Span.t;  (** the declaration's name, for the client *)
  kind : Index.kind;
  detail : string option;
      (** a signature or a type rendered from the source bytes, for the
          client's detail column; the same string the index keeps *)
}

val matches : query:string -> symbol -> bool
(** [matches ~query symbol] is true when the query is a case-insensitive
    substring of the symbol's name. The empty query matches every symbol,
    which is how a client that has typed nothing lists the project. *)

val search : files:Index.t list -> query:string -> symbol list
(** [search ~files ~query] is every declaration in [files] whose name
    contains [query], case-insensitively. The empty query lists them all, so
    a client that opens the palette before typing sees the project.

    The order is the files' order as given, and within one file the
    declarations' source order, so two calls with the same input answer in the
    same order. Declarations the index marks [Unknown] are left out: they are
    the fallback the resolver keeps for a range it could not find, not names
    the source introduces. *)
