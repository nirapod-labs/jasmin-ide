(** The engine: open documents, and the analysis of each one.

    Every function that takes a [uri] keys it through [Jide_lang.Uri], so two
    spellings of one document are one entry: a scheme written in another
    case, a percent-encoded path and the decoded one, and an empty authority
    and [localhost] all name the same document. *)

type t

type settings = {
  arch : Jide_jasmin.Adapter.arch;
  call_conv : Jide_jasmin.Adapter.call_conv;
  idirs : (string * string) list;  (** named include paths *)
}
(** The compiler settings a check runs under. The architecture and the
    calling convention are already validated: a value of this type is one the
    adapter knows, so the library's own setters never see an unknown name. *)

val default_settings : settings
(** x86-64, the Linux calling convention, no include path. *)

val settings : t -> settings

val set_settings : t -> settings -> unit
(** [set_settings t settings] is the settings every later check runs under.
    A settings change forgets the stored analyses, as [set_root] does: the
    architecture and the include paths are part of what an analysis says, and
    neither is part of the version-and-digest key it is found by. *)

val project : t -> Project.loaded option
(** The project file read last, when one was read. *)

val context_entry : t -> string option
(** The entry the client chose for the context rule, which replaces the
    project's list while it is set. *)

val set_context_entry : t -> string option -> unit
(** [set_context_entry t entry] makes [entry] the one entry every file is
    checked in the context of, or hands the rule back to the project with
    [None]. *)

val project_diagnostics : t -> Jide_lang.Diagnostic.t list
(** The problems the project file has, with the spans [Project.diagnostics]
    gives them. Empty when the file is fine or absent. *)

type checker =
  arch:Jide_jasmin.Adapter.arch ->
  call_conv:Jide_jasmin.Adapter.call_conv ->
  file:string ->
  text:string ->
  idirs:(string * string) list ->
  Jide_jasmin.Adapter.check_result
(** The check the engine runs on a text. Only a test replaces it: the server
    takes the adapter's. *)

type context_stage =
  entries:string list ->
  idirs:(string * string) list ->
  read:(string -> string option) ->
  string ->
  Context.t
(** The stage that works out the context of a document: [Context.of_file]
    with the engine's entries, include paths and reader. Only a test replaces
    it, to see what the engine does with a stage that fails. *)

val create : ?check:checker -> ?context:context_stage -> unit -> t
(** [create ?check ?context ()] is an empty store. [check] defaults to
    [Jide_jasmin.Adapter.check] and [context] to [Context.of_file]. *)

val set_root : t -> string option -> unit
(** [set_root t root] records the workspace root and reads
    [Project.file_name] inside it. The architecture, the calling convention
    and the include paths then come from that file where it sets them and
    from the defaults where it does not, and a value the adapter does not
    know is reported as a problem rather than reaching the library. [None]
    means the client named no root: the defaults apply and no file is read. *)

val root : t -> string option
val open_document : t -> uri:string -> version:int -> text:string -> unit

val change_document : t -> uri:string -> version:int -> text:string -> unit
(** Replace the text of an open document.

    A change is applied to a document that [open_document] created and only
    to it: a change for a uri that was never opened, or was closed, is
    ignored and never creates the document. The client owns the set of open
    documents, so the store must not invent one from a change. *)

val close_document : t -> uri:string -> unit
val document : t -> uri:string -> Document.t option
val documents : t -> Document.t list

val max_bytes : int
(** The largest text that is analysed: 8 MiB. A larger one is not parsed and
    gets one diagnostic that says so. *)

type snapshot = {
  index : Index.t;
      (** The index over the analysed text's untyped tree (design section 9):
          every identifier occurrence, every declaration with its name and
          item ranges, the outline and the [require] edges. Its
          [Index.file] is the document's path, or its uri when it has none,
          and its [Index.text] is the text the analysis read, byte-order mark
          included, so a byte of {!Document.t} is used as it comes. *)
  resolve : Resolve.t;
      (** What each occurrence names, and the scope at every byte the walk
          reached, over the analysed file and every file it requires. *)
  facts : Jide_jasmin.Facts.t;
      (** The typed facts the check of this document left: the items that
          typed, with their variables, signatures and annotations. It holds
          the items of this document only, which is the one file a signature
          help of its own calls needs. *)
}
(** What the last analysis of a document built from it, as one value. The
    three fields are exactly the arguments {!Complete.completion} and
    {!Complete.signature_help} take, so a caller reads this and answers
    without touching the compiler, the parser or a file. *)

type analysis = {
  uri : string;  (** the name the document was opened with *)
  version : int;  (** the client's version of the text analysed *)
  key : int * string;  (** the version and digest the analysis belongs to *)
  text : string;  (** the text that was analysed, byte-order mark removed *)
  bom : int;  (** bytes removed from the front of the document: 0 or 3 *)
  snapshot : snapshot;
      (** the index, the resolver and the typed facts built from the text
          analysed, kept so that a later request reads them instead of
          building them again *)
  context : Context.t;
      (** The entry this file is checked in the context of, worked out from
          the project and the [require] lines of the files on disk or in an
          open buffer; the file alone when the stage that works it out
          fails *)
  diagnostics : Jide_lang.Diagnostic.t list;
      (** Spans index the document's own text, so a reader never has to know
          about the byte-order mark. Errors come before warnings, then in
          source order. *)
}

val check : t -> uri:string -> analysis option
(** [check t ~uri] analyses the open document [uri] when its version or a
    digest of its text changed since the last analysis, and returns the
    analysis stored before otherwise, which costs a digest of the text.

    The analysis is total: a document over [max_bytes] is not parsed, and an
    exception anywhere in the pipeline — the check, the mapping of its
    diagnostics, the context — becomes one [internal] diagnostic, so the
    value returned is never an exception and the next call still works.
    [None] means no such document is open. *)

val analysis : t -> uri:string -> analysis option
(** The analysis stored for [uri], without doing any work. *)

val snapshot : t -> uri:string -> snapshot option
(** [snapshot t ~uri] is the index, the resolver and the typed facts the last
    analysis of [uri] built, or [None] when no analysis is stored for it.

    It reads what the last check left and does no work of its own: it does not
    parse, type, resolve or read a file, and it never calls the check, so a
    caller may ask on every keystroke (design section 5, section 10: completion
    and signature help never trigger a check). It costs one hash lookup of the
    uri, and it cannot raise.

    The value is the last analysis's even when the document's text has changed
    since, which is the point: an answer is always available while the client
    is typing. A caller that must know how old it is compares the stored
    analysis's {!analysis.key} with {!analysis_key} of the current text; the
    two differ exactly when a check would do work.

    [None] means the document has never been analysed, not that the analysis
    failed. A document over {!max_bytes}, and one whose pipeline raised, have a
    snapshot too, of an empty index over the empty text, so a caller need not
    tell "nothing to show" from "nothing analysed". *)

val context : t -> uri:string -> Context.t option
(** The context the last analysis of [uri] used, without doing any work.
    [None] means no such document is open or it has not been analysed. *)

val analysis_key : t -> uri:string -> (int * string) option
(** [analysis_key t ~uri] is the version and a digest of the text of the open
    document [uri], so work whose bytes did not change can be skipped. The
    digest is the hexadecimal MD5 from [Stdlib.Digest]: it is stable across
    runs and platforms, needs no extra library, and detects a change rather
    than resisting an attacker. [None] means no such document is open. *)

val analysis_count : t -> int
(** How many analyses this engine has run, which a test uses to see that a
    repeated [check] did no work. *)

val diagnostics_of_check :
  ?bom:int ->
  file:string ->
  text:string ->
  Jide_jasmin.Adapter.check_result ->
  Jide_lang.Diagnostic.t list
(** The diagnostics of one adapter result, as the engine reports them: the
    warnings that belong to [file] become diagnostics, errors come before
    warnings and each group is in source order. [text] is the analysed text
    and [bom] the bytes removed from the front of the document, which the
    compiler never saw; a default of 0 shifts nothing. *)
