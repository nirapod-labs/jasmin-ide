open Jasmin
(** The compiler options a check runs with.

    These are the settings the compiler's own negative tests apply, so that
    a diagnostic from the server matches one from [jasminc] run that way:
    quiet output, export renaming on, recoverable warnings and every
    warning enabled. Every setting is a global in the library, so this is
    applied before each check; it is idempotent. *)

let apply () =
  Glob_options.verbosity := 0;
  Glob_options.introduce_export_renaming := true;
  Utils.set_warn_recoverable true;
  Utils.set_all_warnings ()
