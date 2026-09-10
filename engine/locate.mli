(** Where a name is declared, every place it is written, the outline of a
    file, and what hover says about it.

    Every answer comes from the snapshot the engine keeps of a document: the
    index of each file the resolution reached, the answer the resolver gave
    for each occurrence in it and, where a check left them, the typed facts of
    a file (design sections 9 and 10). Nothing here parses, resolves or types
    a text again, so the four questions cannot disagree with each other or
    with the resolver: they read one table.

    Two interfaces live here, and they differ in their coordinates, not in
    what they answer:

    - {!definition_at}, {!references_at}, {!hover_at} and {!outline} are the
      engine's own: a {!t} the caller built, a [file], and a [byte] of that
      file. Every range they answer is a byte range of the text of the file it
      names, a leading byte-order mark included, exactly as {!Index.of_text}
      reports them, so a caller converts a range to a position with the bytes
      of that same file. The file in a {!Types.location} is the path the index
      holds, which is normalised, and it is a path rather than an identifier:
      a name in that file is a uri only once the protocol asks, which
      {!definition} and {!references} do above. A caller whose document has
      another spelling, or is not a file at all, maps the answer back by its
      own path.
    - {!definition}, {!references}, {!hover} and {!symbols} are the
      controller's: the engine, the [uri] the client opened a document with,
      and a position in the protocol's own coordinates. They read the document
      the client holds, convert the position to a byte with that document's
      line index, ask the byte-level question, and convert the answer's spans
      back into {!range} values. The controller reads only these, so what the
      engine can answer changes without a controller file changing.

    Where the compiler has an answer it wins, as it does for {!Resolve}: the
    typed facts of a checked file carry the variable's kind, its type and its
    annotations, and hover shows them; a file without facts, or a declaration
    the compiler never typed, is shown from the index.

    Nothing here raises, and an answer is never an error. What cannot be
    answered is the empty answer: [[]] from {!definition}, {!references} and
    {!symbols}, [None] from {!hover}. A position in whitespace, a position
    past the end of the text, a document that is not open and a file that does
    not parse are all empty answers, and the request that asked is answered
    [null] or []. *)

(* ------------------------------------------------------------------ *)
(* The answers the protocol asks for *)

type range = {
  start_ : Jide_lang.Position.t;  (** the first position it covers *)
  end_ : Jide_lang.Position.t;  (** the position just past it *)
}
(** A range in a text, in the protocol's coordinates: a zero-based line and a
    zero-based UTF-16 character within it. *)

type location = { uri : string; range : range }
(** A range in one document. [uri] is the name the client opened the document
    with when the range is in that document, and the [file:] identifier of the
    file the engine read when it is not. It is always a valid identifier of the
    document the range is in, and it is made in one place: from the path the
    index holds, once per answer. *)

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

(* ------------------------------------------------------------------ *)
(* The controller's seam *)

val definition :
  Engine.t -> uri:string -> position:Jide_lang.Position.t -> location list
(** [definition engine ~uri ~position] is the declaration of the name at
    [position]: one location where the name leads to a declaration, empty
    where it leads nowhere or holds no name. *)

val references :
  Engine.t ->
  uri:string ->
  position:Jide_lang.Position.t ->
  include_declaration:bool ->
  location list
(** [references engine ~uri ~position ~include_declaration] is every
    occurrence of the name at [position], in source order, the declaration
    among them when [include_declaration] is true and the engine can find it.
    Empty when the name has no declaration to collect occurrences of. *)

val hover :
  Engine.t -> uri:string -> position:Jide_lang.Position.t -> hover option
(** [hover engine ~uri ~position] is what the name at [position] is, as
    markdown, or [None] when the position holds no name the engine can
    describe. *)

val symbols : Engine.t -> uri:string -> symbol list
(** [symbols engine ~uri] is the outline of the document: its items in source
    order, namespaces nested, each with the range of the whole item and the
    range of its name. Empty for a document that is not open or does not
    parse. *)

(* ------------------------------------------------------------------ *)
(* The engine's own answers, in bytes *)

type t
(** The snapshot: one resolution and the typed facts that belong with it. *)

val of_resolve : ?facts:(string * Jide_jasmin.Facts.t) list -> Resolve.t -> t
(** [of_resolve ?facts resolved] is the snapshot of a resolution, with the
    typed facts of the files a check left them for. A [facts] entry whose
    file the resolution never reached is kept and never read. *)

val of_text :
  ?facts:(string * Jide_jasmin.Facts.t) list ->
  idirs:(string * string) list ->
  read:(string -> string option) ->
  file:string ->
  string ->
  t
(** [of_text ?facts ~idirs ~read ~file text] resolves [text] as [file] and
    every file it requires, through [read], and keeps [facts] beside it:
    {!of_resolve} of {!Resolve.of_text}. *)

val of_file :
  ?facts:(string * Jide_jasmin.Facts.t) list ->
  idirs:(string * string) list ->
  read:(string -> string option) ->
  string ->
  t option
(** [of_file ?facts ~idirs ~read file] is {!of_text} on the bytes [read] gives
    for [file], and [None] when it has none. *)

val with_facts : t -> file:string -> Jide_jasmin.Facts.t -> t
(** [with_facts t ~file facts] is [t] with the checked facts of [file]
    replaced by [facts]. The value returned is a new snapshot: the one given
    is unchanged. *)

val resolve : t -> Resolve.t
val entry : t -> string

val files : t -> Index.t list
(** Every file the resolution reached, the entry first. *)

val file : t -> string -> Index.t option
(** [file t path] is the index of [path] when the resolution reached it. *)

val facts : t -> string -> Jide_jasmin.Facts.t option
(** [facts t path] is what a check of [path] last left behind, when a caller
    supplied it. *)

val declaration_at : t -> file:string -> byte:int -> Index.declaration option
(** [declaration_at t ~file ~byte] is the declaration the name under the
    cursor stands for, which the three position questions below share. It is
    [None] for a byte that holds no occurrence, for an unresolved name, and
    for a name that is not a declaration at all: an intrinsic after [\#], an
    annotation label, or the [from] name of a require. *)

val definition_at : t -> file:string -> byte:int -> Types.location option
(** [definition_at t ~file ~byte] is where the declaration of the name under
    the cursor writes its name, in the file that declares it: another file
    when the name comes through a [require] or a namespace, and [None]
    whenever {!declaration_at} is [None]. A cursor on a declaration itself
    gives that declaration. *)

val references_at :
  t ->
  file:string ->
  byte:int ->
  include_declaration:bool ->
  Types.location list
(** [references_at t ~file ~byte ~include_declaration] is every occurrence in
    every file the resolution reached that names the declaration under the
    cursor: the entry file first and then the files it required, each file in
    source order. The declaration's own name is included only when
    [include_declaration] is true. The list is empty when {!declaration_at}
    is [None]. *)

val outline : t -> file:string -> Types.symbol list
(** [outline t ~file] is the outline of [file]: the index's own outline, with
    namespaces and requires nested, as the engine's protocol-neutral symbols.
    A declaration's detail is the signature or type the index cut from the
    source. Nothing is walked a second time. *)

val hover_at : t -> file:string -> byte:int -> Types.hover option
(** [hover_at t ~file ~byte] is what to say about the name under the cursor: a
    fenced block holding the declaration's signature or type, then one line
    per annotation it carries.

    From the typed facts when the compiler typed the declaration: a
    function's [fun_sig], a variable's kind, its type and its annotations.
    From the index otherwise: the signature or type its walk cut from the
    source. An annotation's value is shown only when it is text that is safe
    to display (printable, no backtick, at most 120 bytes); when it is not,
    the label alone is shown.

    An intrinsic, an annotation label and an include path are not
    declarations, so they hover as themselves. An unresolved name, a byte
    that holds no occurrence and a file the resolution never reached hover as
    [None]. *)
