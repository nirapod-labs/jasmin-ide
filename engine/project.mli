(** The project file, [jasmin.project], found by walking up from the file
    being analysed.

    It is an INI file with one optional [[jasmin]] section, one
    [key = value] per line, whole-line [\#] and [;] comments and repeated keys
    that accumulate in file order. Every key is optional. A file that cannot
    be read or does not parse is reported and never stops the server: the
    defaults apply and the rest of the program keeps running. An architecture
    or a calling convention the adapter does not know is refused here, so a
    value of the type below is one the library's own setters accept. *)

type t = {
  arch : Jide_jasmin.Adapter.arch option;
  call_conv : Jide_jasmin.Adapter.call_conv option;
  idirs : (string * string) list;
      (** named include paths, resolved against the project file's directory
          and in the order the file gives them *)
  entries : string list;
      (** entry files, resolved against the project file's directory. At most
          one, because [entry] is a single-valued key *)
}

val empty : t
(** No field set. *)

val file_name : string
(** [jasmin.project]. *)

type problem = { span : Jide_lang.Span.t; message : string }
(** Something wrong in the file, at the bytes it is wrong at: the key it does
    not know, or the value it cannot use. A span indexes the file's own bytes,
    byte-order mark included. *)

type loaded = {
  path : string option;  (** the file the text came from, when one was read *)
  text : string;  (** the text, byte-order mark removed *)
  bom : int;  (** bytes removed from the front of the file: 0 or 3 *)
  project : t;  (** what could be read; the defaults for the rest *)
  problems : problem list;  (** what could not, in the order they were found *)
}

val absent : loaded
(** A file there is none of: no path, no text, nothing set and no problem.
    This is what [load] gives for a missing file and [load_above] gives for a
    directory with no project file above it. *)

val load : string -> loaded
(** [load path] reads the file at [path]. A missing file is not a problem:
    the result is [absent]. Any other read failure is one problem, because a
    file the workspace holds and the server cannot read is worth saying.
    Relative paths inside the file are resolved against the directory holding
    [path], and the ones it names are checked for existence there. *)

val find_above : string -> string option
(** [find_above directory] is the path of the first [jasmin.project] in
    [directory] or in the nearest directory above it. The filesystem root stops
    the walk. *)

val load_above : string -> loaded
(** [load_above directory] is [load] of [find_above directory]. A directory
    with no such file above it is the ordinary case: the result is [absent]
    and no problem is made. *)

val parse : ?path:string -> string -> loaded
(** [parse ?path text] reads a project file from [text] without asking which
    file it came from. [path] names that file, for the relative paths, for the
    existence checks and for the caller's diagnostics; without it a relative
    path is left as written and nothing is checked on disk. *)

val diagnostics : uri:string -> loaded -> Jide_lang.Diagnostic.t list
(** [diagnostics ~uri loaded] is one error diagnostic per problem, with
    [uri] naming the project file. Empty when the file is fine or absent. *)
