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

type parse_result = Parse.t

val parse : name:string -> string -> parse_result

type check_result = {
  diagnostics : Jide_lang.Diagnostic.t list;
      (** Diagnostics whose span indexes [file]. An error the compiler
          reports in a file reached through a [require] is not returned: its
          offsets belong to that file, not to this document. *)
  warnings : Warnings.t list;
      (** Each warning names the file the compiler printed it for, or [file]
          when it printed no location, so the engine keeps the ones for the
          document it asked about. *)
}

val check :
  arch ->
  call_conv ->
  file:string ->
  text:string ->
  idirs:(string * string) list ->
  check_result
