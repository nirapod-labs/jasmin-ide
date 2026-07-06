(** The only interface the engine uses to reach Jasmin.

    Everything here is total: a failure in the library becomes a
    diagnostic, never an exception. *)

type arch = Arch.t
type call_conv = Arch.call_conv

val arch_names : string list
val arch_of_string : string -> arch option
val call_conv_of_string : string -> call_conv option

(** The Jasmin version the server was built against. *)
val jasmin_version : string

type parse_result = Parse.t

val parse : name:string -> string -> parse_result

type check_result = {
  diagnostics : Jide_lang.Diagnostic.t list;
  warnings : Warnings.t list;
}

val check :
  arch ->
  call_conv ->
  file:string ->
  text:string ->
  idirs:(string * string) list ->
  check_result
