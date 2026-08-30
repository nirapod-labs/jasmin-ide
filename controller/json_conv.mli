(** The JSON the protocol carries, built from the language's own values.

    Nothing else in the controller writes JSON for a document: a range is
    converted from bytes to UTF-16 positions here, once, and every caller
    passes the line index of the text the offsets belong to. A span outside
    that text is clamped by the line index, so a diagnostic that outlived
    the text it describes still produces a well-formed range.

    The severity numbers are the protocol's: 1 error, 2 warning, 3
    information, 4 hint. [delay_of_settings] reads a setting, so it goes the
    other way, from the client's JSON to a number the server can use; it
    checks the shape only, not the range. *)

val position_json : Jide_lang.Position.t -> Yojson.Safe.t
(** [position_json position] is [{"line":..., "character":...}]. *)

val range_json : Jide_lang.Line_index.t -> Jide_lang.Span.t -> Yojson.Safe.t
(** [range_json lines span] is [{"start":..., "end":...}] with both ends
    converted from byte offsets to UTF-16 positions in [lines]. *)

val severity_json : Jide_lang.Diagnostic.severity -> Yojson.Safe.t
(** [severity_json severity] is the protocol's number for it. *)

val diagnostic_json :
  uri:string ->
  Jide_lang.Line_index.t ->
  Jide_lang.Diagnostic.t ->
  Yojson.Safe.t
(** [diagnostic_json ~uri lines diagnostic] is one diagnostic object:
    [range], [severity], [source], [message], and [code] when the compiler
    gave one. A related span becomes an entry of [relatedInformation] under
    [uri], because every span of a diagnostic is in the file it belongs to. *)

val publish_params :
  uri:string ->
  version:int option ->
  lines:Jide_lang.Line_index.t ->
  Jide_lang.Diagnostic.t list ->
  Yojson.Safe.t
(** [publish_params ~uri ~version ~lines diagnostics] is the parameter
    object of a [textDocument/publishDiagnostics] notification: the file,
    the version of the text the diagnostics describe when it is known, and
    the full set. An empty list clears the file's diagnostics. *)

val completion_json : Jide_engine.Types.completion_item list -> Yojson.Safe.t
(** [completion_json items] is the answer of [textDocument/completion]: one
    object per item, each with its label and its kind, and with its detail,
    documentation, insert text and sort text only where the engine gave them.
    An absent field is left out rather than written null, because the
    protocol tells the two apart. The items keep the engine's order, which is
    by label; the protocol reads them as a list of the same weight and lets
    the client decide otherwise. *)

val signature_help_json : Jide_engine.Types.signature_help -> Yojson.Safe.t
(** [signature_help_json help] is the answer of [textDocument/signatureHelp]:
    the signatures with their parameters and documentation, and the active
    signature and argument as zero-based indexes into them. A signature the
    engine gave no parameters for carries no [parameters] field, because an
    empty list would say the call takes no argument. *)

val delay_of_settings : Yojson.Safe.t -> int option
(** [delay_of_settings settings] is the value of [diagnostics.delay] in an
    editor's settings object, or [None] when the object does not carry one.
    The keys are looked for under [jasminIde], then at the top level, then as
    a dotted name ([diagnostics.delay]), because clients differ in how deep
    they nest a section. The number is returned as written: the caller
    clamps it to the range it accepts. *)
