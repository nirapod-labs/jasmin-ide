(** The only interface the engine uses to reach Jasmin.

    Everything here is total: a failure in the library becomes a
    diagnostic, never an exception. *)

type arch = Arch.t
type call_conv = Arch.call_conv

val arch_names : string list
val arch_of_string : string -> arch option
val call_conv_of_string : string -> call_conv option

val jasmin_version : string
(** The Jasmin version the server was built against. *)

val max_bytes : int
(** The largest document this adapter will parse. A buffer above it is
    refused with one [size] diagnostic by {!parse}, and so by {!check},
    which reaches {!parse} through its own item loop, instead of producing a
    syntax error.

    A caller that applies its own admission limit must keep it at or below
    this one: the adapter answers {!parse} with empty items and a refusal for
    anything larger, so a document a caller admitted above this ceiling
    would be reported as too large after the caller had promised it an
    analysis. The engine's [Engine.max_bytes] is that caller's limit and the
    engine's tests hold the two together.

    The adapter also refuses a buffer over its token ceiling, well before
    this byte count for real Jasmin; that refusal says so in tokens, so it
    cannot be mistaken for a byte limit. *)

type parse_result = Parse.t

val parse : name:string -> string -> parse_result

type required = {
  file : string;
      (** The path the compiler prints for it, which is the one the [require]
          resolved to and not a normalised copy: a document named
          [entry.jazz] reaches [consts.jinc], not [/where/it/sits]. *)
  text : string;  (** the bytes of [file], which [diagnostics] index *)
  diagnostics : Jide_lang.Diagnostic.t list;
      (** What the compiler reported in [file] itself, at the place it
          reported it: an error in a file reached through a [require] is
          never printed at the require item instead. *)
  site : int option;
      (** The byte of the document's item whose typing reached [file] first,
          which is where in the document's order the compiler meets its
          errors; [None] for a file of the context chain, which the compiler
          enters before the document. *)
}
(** A file reached through a [require] that holds an error, and that error.
    The caller prints each diagnostic against [text] and with [file] as its
    name, which is where the compiler prints it.

    A [require] written inside a namespace is not reported this way: its
    file's items are spliced into the namespace's item, which the library
    types as one item, so the file's offsets reach the loop with nothing to
    map them to and the item says the file has errors instead. *)

type check_result = {
  diagnostics : Jide_lang.Diagnostic.t list;
      (** Diagnostics whose span indexes [file]. An error the compiler
          reports in a file reached through a [require] is not here: that
          file and its diagnostic are in {!required} instead. *)
  warnings : Warnings.t list;
      (** Each warning names the file the compiler printed it for, or [file]
          when it printed no location, so the engine keeps the ones for the
          document it asked about. *)
  facts : Facts.t;
      (** What the items that typed left behind: their variables, their
          function signatures, their annotations and, for the outline, their
          names and ranges. Every range in it indexes [file] and every value
          is plain data, so the engine reads it without handling the
          compiler's program representation. *)
  required : required list;
      (** The files reached through a [require] that hold an error, in the
          order their first error was found. *)
}

val jasmin_path : unit -> (string * string) list option
(** The named include paths of the [JASMINPATH] environment variable, read as
    the compiler reads it: [NAME=PATH] entries separated by [:]. [Some []]
    when it is not set, and [None] when an entry has no [=], a value the
    compiler warns about and ignores. *)

val with_jasmin_path : (string * string) list -> (string * string) list
(** [with_jasmin_path idirs] is [idirs] followed by the bindings of
    [JASMINPATH] it does not already hold, which is the order the compiler
    binds them in. {!check} applies it itself; a caller that resolves a
    [from NAME require] on its own, such as a context walk or the navigation
    resolver, applies it to agree with the check. *)

val check :
  ?resolve:(string -> string option) ->
  ?context:string list ->
  arch ->
  call_conv ->
  file:string ->
  text:string ->
  idirs:(string * string) list ->
  check_result
(** [check ~resolve ~context arch call_conv ~file ~text ~idirs] parses and
    types [text] as [file]. The named include paths are [idirs] followed by
    those of [JASMINPATH] ({!with_jasmin_path}), as the compiler binds them,
    and an ill-formed value draws the compiler's own warning. When [text]
    parses without an error, the compiler's
    own preprocessing (parameter substitution, then the final typing pass) runs
    over what typed, so a program the compiler rejects before its checkpoint
    is rejected here too. [resolve] answers with the bytes of a file a
    [require] item names, or [None] when it has none; the engine passes its
    open buffers first and falls back to disk, which is what the default does.

    [context] is the chain of files the compiler would enter before [file] when
    it is not an entry, outermost first; the file is checked in the
    environment the last of them builds up to the [require] that names it.
    The empty list, the default, is a file checked on its own.

    A [require] inside a namespace is read through [resolve] too: the adapter
    reads the file itself and types its items in the namespace, because the
    library's own include path reads the disk and cannot be reached from
    here. A missing file, a cycle and a file that does not type are
    diagnostics of the require item, and the names a file that did type
    declares, or that one which did not would have declared, are not reported
    as unknown afterwards. A file that cannot be read at all is the one
    exception to the last clause at the top level: nothing is known about a
    file that has no bytes, so a reference to a name only it would have
    declared is reported as well. Inside a namespace the opposite is the
    safer answer, because every name under it is the unread file's as far as
    the document can tell, so those references are suppressed instead. *)
