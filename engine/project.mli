(** The project file, [jasmin-project.json] at the workspace root.

    Every field is optional. A file that cannot be read or does not parse is
    reported and never stops the server: the defaults apply and the rest of
    the program keeps running. An architecture or a calling convention the
    adapter does not know is refused here, so a value of the type below is
    one the library's own setters accept. *)

type t = {
  arch : Jide_jasmin.Adapter.arch option;
  call_conv : Jide_jasmin.Adapter.call_conv option;
  idirs : (string * string) list;
      (** named include paths, resolved against the project file's directory *)
  entries : string list;
      (** entry files, resolved against the project file's directory *)
}

val empty : t
(** No field set. *)

val file_name : string
(** [jasmin-project.json]. *)

type problem = { span : Jide_lang.Span.t; message : string }
(** Something wrong in the file, at the bytes it is wrong at: the quoted name
    of a field the file does not hold, or the value that cannot be used. *)

type loaded = {
  path : string option;  (** the file the text came from, when one was read *)
  text : string;  (** the text, byte-order mark removed *)
  bom : int;  (** bytes removed from the front of the file: 0 or 3 *)
  project : t;  (** what could be read; the defaults for the rest *)
  problems : problem list;  (** what could not, in the order they were found *)
}

val load : string -> loaded
(** [load path] reads the file at [path]. A missing file is not a problem:
    the result is [empty] with no text and no problem. Any other read
    failure is one problem, because a file the workspace holds and the
    server cannot read is worth saying. Relative paths inside the file are
    resolved against the directory holding [path]. *)

val parse : ?path:string -> string -> loaded
(** [parse ?path text] reads a project file from [text] without asking the
    filesystem. [path] names the file the text came from, for relative
    paths and for the diagnostics. *)

val diagnostics : uri:string -> loaded -> Jide_lang.Diagnostic.t list
(** [diagnostics ~uri loaded] is one error diagnostic per problem, with
    [uri] naming the project file. Empty when the file is fine or absent. *)
