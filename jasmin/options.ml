open Jasmin
(** The compiler options a check runs with.

    These are the settings the compiler's own negative tests apply, so that
    a diagnostic from the server matches one from [jasminc] run that way:
    quiet output, export renaming on and every warning enabled. Every
    setting is a global in the library, so this is applied before each
    check; it is idempotent.

    [Utils.set_warn_recoverable] is left at its default, [false]. It turns a
    [PedanticPretyping] diagnostic into a warning instead of a [typing
    error], and the release's own negative tests turn it off for the files
    they expect such an error from. With it on, this check accepts those
    files; for a non-inlined function with a stack parameter or result it
    also reaches the [assert false] after the result check in
    [pretyping.ml] instead of reporting the error that check stands for. *)

let apply () =
  Glob_options.verbosity := 0;
  Glob_options.introduce_export_renaming := true;
  Utils.set_all_warnings ()
