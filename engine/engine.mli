(** The engine: open documents, and the analysis of each one.

    Every function that takes a [uri] keys it through [Jide_lang.Uri], so two
    spellings of one document are one entry: a scheme written in another
    case, a percent-encoded path and the decoded one, and an empty authority
    and [localhost] all name the same document. The document keeps the URI it
    was opened with; only the key is normalised.

    A document is analysed under the project file nearest above it, found by
    walking up from its own directory (design note, section 3). Two documents
    in one workspace may therefore be under two different projects, and one
    under neither is analysed with the defaults and told nothing about it. *)

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
(** The settings of the workspace root's own project, over the base. It is
    what a caller asks when it is not looking at one document; a check uses
    the project above the document it is checking. *)

val set_settings : t -> settings -> unit
(** [set_settings t settings] is the base every later check starts from: the
    client's own choice, which a project file overrides. A settings change
    forgets the stored analyses, as [set_root] does: the architecture and the
    include paths are part of what an analysis says, and neither is part of
    the version-and-digest key it is found by. *)

val project : t -> Project.loaded option
(** The project file above the workspace root, when one was found. *)

val context_entry : t -> string option
(** The entry the client chose for the context rule, which replaces the
    project's list while it is set. *)

val set_context_entry : t -> string option -> unit
(** [set_context_entry t entry] makes [entry] the one entry every file is
    checked in the context of, or hands the rule back to the project with
    [None]. *)

val project_diagnostics : t -> Jide_lang.Diagnostic.t list
(** The problems the workspace root's project file has, with the spans
    [Project.diagnostics] gives them. Empty when the file is fine or absent. *)

type checker =
  context:string list ->
  arch:Jide_jasmin.Adapter.arch ->
  call_conv:Jide_jasmin.Adapter.call_conv ->
  file:string ->
  text:string ->
  idirs:(string * string) list ->
  Jide_jasmin.Adapter.check_result
(** The check the engine runs on a text, in the context the stage below works
    out: the files the compiler enters before the document, outermost first,
    with the empty list for a file checked on its own. Only a test replaces
    it: the server takes the adapter's. *)

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
(** [set_root t root] records the workspace root and reads the project file at
    it or above it, so that [project] and [project_diagnostics] report it.
    [None] means the client named no root: no project file is read here, and
    every document falls back on [settings] and the defaults; [resolve]
    refuses every path until a root is recorded.

    Every check finds its own document's project by walking up from the
    document, whether or not a root was named: a file under a
    [jasmin.project] is analysed under it. The architecture, the calling
    convention and the include paths come from that file where it sets them
    and from the defaults where it does not, and a value the adapter does not
    know is reported as a problem rather than reaching the library. *)

val root : t -> string option
(** The root [set_root] recorded, unchanged. *)

val open_document : t -> uri:string -> version:int -> text:string -> unit
(** [open_document t ~uri ~version ~text] records an open document. [uri] is
    the name the document keeps; the store keys it on the normalised
    identifier, so a second spelling replaces the first rather than creating
    a second document. *)

val change_document : t -> uri:string -> version:int -> text:string -> unit
(** Replace the text of an open document.

    A change is applied to a document that [open_document] created and only
    to it: a change for a uri that was never opened, or was closed, is
    ignored and never creates the document. The client owns the set of open
    documents, so the store must not invent one from a change. *)

val close_document : t -> uri:string -> unit
(** Remove the document [uri] names, whichever spelling of the identifier the
    client uses. A close for a document that is not open does nothing. *)

val document : t -> uri:string -> Document.t option
(** The open document [uri] names, found through the normalised key. *)

val documents : t -> Document.t list
(** Every open document, one per normalised identifier. *)

val text : t -> uri:string -> string option
(** [text t ~uri] is the text of the open document when there is one, and
    otherwise the text of the file the URI names. It is [None] when there is
    neither: no open document and no readable file. A closed document is read
    from disk again. *)

val version : t -> uri:string -> int option
(** [version t ~uri] is the version the client gave an open document, and 0
    for a file on disk. It is [None] exactly when there is neither an open
    document nor a readable file. A file's status decides, not its bytes, so a
    path that exists but cannot be read gives [None] here without a read of
    the file. *)

val resolve : t -> string -> (string, string) result
(** [resolve t path] is the absolute, normalised path of [path] inside the
    workspace root, or [Error message] when it would lie outside it.

    A relative path is joined to the root and an absolute path is accepted
    only when it is already inside the root. The empty path resolves to the
    root itself. [.] and [..] are resolved in the text, so a path that does
    not exist yet still resolves; for the part of the path that does exist,
    symbolic links are followed as well, and a link that leaves the root is
    refused. A link whose target does not exist is not followed. Names are
    compared byte for byte: a path that does not exist and differs from the
    root only in case is refused, which is the safe direction; a path that
    does exist is compared by its real path, so the volume's own case rules
    apply. Every path is refused when no root is recorded, and when the
    recorded root is not absolute. *)

val read_file : t -> string -> string option
(** [read_file t path] is the text of [path] as a reader of the workspace
    finds it: the editor's buffer when the file is open, the disk otherwise,
    and [None] when neither has it. An answer about a name that comes through
    a [require] is read from the same text the check was given, so a buffer
    the client has not saved is what the answer describes. *)

val resolution : t -> uri:string -> Resolve.t option
(** [resolution t ~uri] is the resolution of the open document [uri]: its own
    file and every file it requires, each parsed and walked, with what every
    name in them stands for. It is what the four navigation answers read, and
    it is kept until something it was built from changes — a document
    opening, changing or closing, the settings, the root or the context entry
    — so asking twice does not parse the project twice.

    [None] means no such document is open, or it is open under a scheme that
    names no file, in which case there is nothing to read a [require]
    relative to and the navigation answers about it are the empty answer. *)

val project_resolutions : t -> uri:string -> Resolve.t list
(** [project_resolutions t ~uri] is the resolution of every entry file of the
    project the open document [uri] is under, the document itself left out:
    the entries the context rule uses ({!context_entry} when the client chose
    one, the project's list otherwise, and every [.jazz] file under the root
    when the project lists none). Each is read like {!resolution}, from the
    open buffers first, under the document's include paths, and kept until
    something it was built from changes. A rename reads them, because a
    declaration a document holds may be used by any entry that reads it. The
    empty list when the document is not open. *)

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
  arch : Jide_jasmin.Adapter.arch;
      (** The architecture the document was analysed under: its project's, or
          the default when its project names none. An intrinsic call is
          answered from the table of this architecture. *)
}
(** What the last analysis of a document built from it, as one value. The
    fields are the arguments {!Complete.completion} and
    {!Complete.signature_help} take, with the architecture a completion or a
    signature help answers under, so a caller reads this and answers without
    touching the compiler, the parser or a file. *)

type required = {
  uri : string;
      (** The uri the file is named by: the uri of the open document that
          holds it when it is open, so a client that has it open publishes
          against the text it shows, and a [file] uri made of its path
          otherwise. A path the check reported relative to the document's own
          is resolved against that document first, so the two spellings of one
          file are one uri. *)
  text : string;
      (** The bytes [diagnostics] index: the ones the check read, which are the
          file's on disk. The line index a caller publishes with is built from
          this text, never from a buffer that may have changed since. *)
  diagnostics : Jide_lang.Diagnostic.t list;
      (** What the check reported in the file itself, at the place it reported
          it: errors before warnings, then in source order. *)
}
(** A file reached through a [require] that holds an error, and that error.
    The caller publishes each diagnostic against {!required.uri} with the
    range {!required.text} gives it, which is where the compiler prints it,
    rather than against the document the check was asked about. *)

type analysis = {
  uri : string;  (** the name the document was opened with *)
  version : int;  (** the client's version of the text analysed *)
  key : int * string;  (** the version and digest the analysis belongs to *)
  text : string;  (** the text that was analysed, byte-order mark removed *)
  bom : int;  (** bytes removed from the front of the document: 0 or 3 *)
  snapshot : snapshot;
      (** the index, the resolver and the typed facts built from the text
          analysed, kept so that a later request reads them instead of
          building them again. The facts of the check are its [facts] and
          nothing else holds a second copy of them, so hover and the other
          navigation answers read the one the text was checked with *)
  context : Context.t;
      (** The entry this file is checked in the context of, worked out from
          the project and the [require] lines of the files on disk or in an
          open buffer; the file alone when the stage that works it out
          fails *)
  diagnostics : Jide_lang.Diagnostic.t list;
      (** Spans index the document's own text, so a reader never has to know
          about the byte-order mark. Errors come before warnings, then in
          source order. A diagnostic of a file the document requires is not
          here: that file and its diagnostics are in {!analysis.required}. *)
  required : required list;
      (** The files the document requires that hold an error, each with the
          uri it is published against. Empty when every file it reads is
          fine. *)
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
(** [snapshot t ~uri] is the index, the resolver, the typed facts and the
    architecture the last analysis of [uri] built, or [None] when no analysis
    is stored for it.

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
(** [analysis_key t ~uri] is the version and a digest of the text [text]
    returns, whether that is an open buffer's or a file's, so work whose bytes
    did not change can be skipped. The digest is the hexadecimal MD5 from
    [Stdlib.Digest]: it is stable across runs and platforms, needs no extra
    library, and detects a change rather than resisting an attacker. The key
    is [None] exactly when [text] is [None].

    The version half is not a document identity: it is the client's version
    for an open document and 0 for a file on disk, so it is not comparable
    across an open and a closed period. The pair is a change detector for one
    normalised uri, not a name for the document. *)

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
    compiler never saw; a default of 0 shifts nothing. A diagnostic of a file
    reached through a [require] is not one of these: {!analysis.required}
    names its file and carries it. *)
