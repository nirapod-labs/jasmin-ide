(** Warnings the compiler prints, read back as values and published as
    diagnostics.

    The library has no warning hook: [Utils.warning] prints through
    [Format.eprintf] to [Format.err_formatter]. A check runs under
    {!with_capture}, which points that formatter at a buffer, and {!parse}
    reads the location and the message back out of the captured text.

    The printer writes a location as [Location.pp_iloc] does, one line per
    entry of the stack: ["file", line N (a-b)] first, [from ...] for the
    entries above it, then [warning: ...] on a line of its own because the
    enclosing box is vertical. The single-line form
    ["file", line N (a-b): warning: ...] is accepted too. *)

type t = {
  file : string;
      (** The file the printer named, or the file being checked when it
          named none. *)
  line : int;
      (** The compiler's own line number, which starts at 1; 0 when the
          printer wrote no location. *)
  start_column : int;  (** Bytes from the start of the line, as printed. *)
  end_column : int;
  message : string;  (** The text of the warning, without its location. *)
}
(** One warning as the printer wrote it, with the [.<uid>] suffix of the
    message stripped: the compiler adds one to the name of an unused
    variable, and it changes at every check. *)

val with_capture : (unit -> 'a) -> 'a * string
(** [with_capture f] runs [f] with the output functions of
    [Format.err_formatter] redirected to a buffer, and answers [f]'s result
    together with the captured text. The formatter is restored on every
    path, an exception included. *)

val parse : ?file:string -> string -> t list
(** [parse ?file text] is the warnings in [text], in the order they were
    printed. [file] is the name a warning that printed no location is
    given, and defaults to [""].

    Only a line that starts with a quoted file name supplies a location, so
    a message that quotes one stays a message, and a stacked location keeps
    its first entry, which is the one the warning is about. *)

type placed = { file : string; diagnostic : Jide_lang.Diagnostic.t }
(** A warning converted to a diagnostic, with the file it belongs to: the
    one it named, or the checked file when it named none. The span indexes
    the checked text only when [file] is that text's file, so the caller
    keeps the entries whose [file] is the document it asked about. *)

val span : file:string -> text:string -> t -> Jide_lang.Span.t
(** [span ~file ~text warning] is the byte range [warning] points at in
    [text], which is the text of [file].

    The range starts at the first byte of the warning's line plus its start
    column and ends there plus its end column, each clamped to the line, and
    the line is clamped to the text. A warning with a dummy location, or one
    for another file, has the empty span: it has no place in this text. *)

val to_diagnostic : file:string -> text:string -> t -> Jide_lang.Diagnostic.t
(** [to_diagnostic ~file ~text warning] is [warning] as a diagnostic:
    severity [Warning], code ["warning"], source ["jasmin"], the span from
    {!span} and the message as parsed. *)

val to_diagnostics : file:string -> text:string -> t list -> placed list
(** [to_diagnostics ~file ~text warnings] converts every warning, in order.
    A warning repeated with the same span and message in the same file is
    kept once, because a repeated item prints its warning twice; the file is
    part of that identity so a warning for another file, which carries the
    empty span, cannot suppress one of the checked file's. *)
