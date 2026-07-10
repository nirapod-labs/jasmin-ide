(** The command line of the batch checker. *)

type t = {
  arch : Jide_jasmin.Adapter.arch;  (** The target architecture. *)
  call_conv : Jide_jasmin.Adapter.call_conv;  (** The calling convention. *)
  idirs : (string * string) list;
      (** The named include paths, as [name=path] pairs, in the order given. *)
  files : string list;  (** The sources to check, in the order given. *)
}

type parsed =
  | Run of t  (** A complete command line. *)
  | Help  (** The caller asked for the usage text. *)
  | Invalid of string  (** A usage error, with the reason. *)

val usage : string
(** The usage text, one line per entry, without a trailing newline. *)

val parse : string list -> parsed
(** [parse arguments] reads the arguments after the [check] mode word. The
    default architecture is [x86-64] and the default calling convention is
    [linux]. A repeated option wins over the earlier one and a repeated
    [-I] adds another include path. An argument after [--] is a file even
    when it starts with a dash. *)
