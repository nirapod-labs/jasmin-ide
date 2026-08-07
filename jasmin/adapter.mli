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

type check_result = {
  diagnostics : Jide_lang.Diagnostic.t list;
      (** Diagnostics whose span indexes [file]. An error the compiler
          reports in a file reached through a [require] is not returned with
          its own offsets: the require item says the file has one instead. *)
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
}

val check :
  ?resolve:(string -> string option) ->
  arch ->
  call_conv ->
  file:string ->
  text:string ->
  idirs:(string * string) list ->
  check_result
(** [check ~resolve arch call_conv ~file ~text ~idirs] parses and types
    [text] as [file]. [resolve] answers with the bytes of a file a [require]
    item names, or [None] when it has none; the engine passes its open
    buffers first and falls back to disk, which is what the default does. A
    [require] inside a namespace is read through [resolve] too: the adapter
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
