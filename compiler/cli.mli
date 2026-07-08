(** The [check] mode of the server binary. *)

val run : out:(string -> unit) -> err:(string -> unit) -> string list -> int
(** [run ~out ~err arguments] checks every file named by [arguments], which
    are the words after the [check] mode word. It writes one line per
    diagnostic and one summary line per file with [out], reports a usage
    error with [err], and returns the exit status: 0 when no file has an
    error-severity diagnostic, 1 when one does and 2 when the command line
    itself is wrong. A file that cannot be read becomes an error-severity
    diagnostic of kind [internal], so it fails the run with 1. *)

val main : string list -> int
(** [main arguments] is [run] over the standard output and standard error of
    the process. *)
