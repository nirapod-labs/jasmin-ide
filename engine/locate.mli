(** The answers the navigation requests ask for.

    A definition, a reference, a hover and a document's symbols are the
    engine's questions, but their answers are the protocol's: a position is a
    zero-based line and a UTF-16 character, and a range is two of them. The
    conversion from the byte spans the parser and the resolver use happens
    here, and only here, because only the engine holds the text of every file
    an answer can name: a location inside a required file cannot be turned
    into a range by a layer that never read that file.

    Nothing here raises, and an answer is never an error. What the engine
    cannot answer is an empty answer: [[]] from {!definition}, {!references}
    and {!symbols}, [None] from {!hover}. A position in whitespace, a
    position past the end of the text, a document that is not open and a file
    that does not parse are all empty answers, and the request that asked is
    answered [null] or [].

    **This module is a stub.** {!Resolve} and {!Index} already answer in byte
    spans of the files they read; the adapter that turns their declarations,
    occurrences and outline into the values below is another lane's, and the
    four functions here answer "unavailable" — the empty answer — until it
    lands. Its shape is fixed: a consumer reads these values and nothing
    else, so filling the stub in changes no controller and no test of the
    routing. *)

type range = {
  start_ : Jide_lang.Position.t;  (** the first position it covers *)
  end_ : Jide_lang.Position.t;  (** the position just past it *)
}
(** A range in a text, in the protocol's coordinates. *)

type location = { uri : string; range : range }
(** A range in one document. [uri] is the name the document was opened with,
    normalised through [Jide_lang.Uri], or the [file:] identifier of a file
    the engine read from disk. *)

type hover = { range : range option; contents : Types.markdown }
(** What to show and, when the engine knows it, the text the answer is about.
    [contents] is one MarkupContent, of the protocol's [markdown] kind. *)

type symbol = {
  name : string;  (** the name as written *)
  detail : string option;  (** a signature or a type, when the engine has one *)
  kind : Types.symbol_kind;
  range : range;  (** the whole item, which is what an outline collapses *)
  selection_range : range;  (** the name alone; [range] contains it *)
  children : symbol list;  (** the items nested in this one, in source order *)
}
(** One node of a document's symbol tree. *)

val definition :
  Engine.t -> uri:string -> position:Jide_lang.Position.t -> location list
(** [definition t ~uri ~position] is the declaration of the name at
    [position]: one location where the name leads to a declaration, empty
    where it leads nowhere or holds no name. *)

val references :
  Engine.t ->
  uri:string ->
  position:Jide_lang.Position.t ->
  include_declaration:bool ->
  location list
(** [references t ~uri ~position ~include_declaration] is every occurrence of
    the name at [position], in source order, the declaration among them when
    [include_declaration] is true and the engine can find it. Empty when the
    name has no declaration to collect occurrences of. *)

val hover :
  Engine.t -> uri:string -> position:Jide_lang.Position.t -> hover option
(** [hover t ~uri ~position] is what the name at [position] is, as markdown,
    or [None] when the position holds no name the engine can describe. *)

val symbols : Engine.t -> uri:string -> symbol list
(** [symbols t ~uri] is the outline of the document: its items in source
    order, namespaces nested, each with the range of the whole item and the
    range of its name. Empty for a document that is not open or does not
    parse. *)
