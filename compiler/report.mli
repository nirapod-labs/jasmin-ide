(** The verdict of a check, as the checker prints it.

    The text is the file's own bytes: line and column are 1-based, and a
    column counts bytes from the start of its line, which is the compiler's
    own convention ([Location.pp_loc] prints the same column zero-based). *)

val of_check :
  file:string ->
  text:string ->
  Jide_jasmin.Adapter.check_result ->
  Jide_lang.Diagnostic.t list
(** [of_check ~file ~text result] is the diagnostics to print for [file],
    errors first and then warnings, each group in source order. A warning
    the compiler printed for another file, which a [require] can reach, is
    dropped: its location is not in [text]. A warning that names no location
    is kept, because the adapter puts it on [file] and line 0. *)

val line_column : text:string -> Jide_lang.Span.t -> int * int
(** [line_column ~text span] is the 1-based line and the 1-based byte column
    of the start of [span] in [text], clamped to the text. *)

val kind : Jide_lang.Diagnostic.t -> string
(** The word between the position and the message: the diagnostic's code,
    ["warning"] for a warning and ["internal"] for an error without a code. *)

val escape : string -> string
(** [escape message] puts [message] on one line: a backslash, a newline, a
    carriage return and a tab become two characters, [\\], [\n], [\r] and
    [\t]. A compiler message can span lines, and the output is one line per
    diagnostic. *)

val line : file:string -> text:string -> Jide_lang.Diagnostic.t -> string
(** [line ~file ~text diagnostic] is one output line:
    [<file>:<line>:<column>: <kind>: <message>]. *)

val lines :
  file:string -> text:string -> Jide_lang.Diagnostic.t list -> string list
(** [lines ~file ~text diagnostics] is [line] for each diagnostic. *)

val required_lines : Jide_jasmin.Adapter.check_result -> string list
(** [required_lines result] is one output line per diagnostic the check found
    in a file reached through a [require]. Each line names that file and its
    column counts bytes of that file's own text, because the span indexes it
    and not the document: this is where the compiler prints the error too. *)

val summary : file:string -> Jide_lang.Diagnostic.t list -> string
(** [summary ~file diagnostics] is ["<file>: ok"] when no diagnostic has
    error severity, and ["<file>: N error"] or ["<file>: N errors"]
    otherwise. A warning does not fail a file. *)

val internal : string -> Jide_lang.Diagnostic.t
(** [internal message] is the error-severity diagnostic for a failure that is
    not the compiler's, such as a file that cannot be read. It sits at the
    first byte of the file, so it prints at line 1, column 1. *)
