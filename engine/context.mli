(** The context a file is checked in.

    A [.jinc] file is usually not checkable alone: it uses parameters,
    functions and named include paths that the file requiring it set up
    first. So a file is checked in the context of an entry, and this module
    works out which one:

    - an entry file, meaning one the project lists in [entries] or, when it
      lists none, any [.jazz] file, is checked on its own;
    - any other file is checked in the context of the first entry that
      requires it, directly or through other files;
    - a file no entry requires is checked on its own.

    Everything here is a path question: which file names which other file.
    Nothing is parsed and nothing is typed. *)

type t = {
  file : string;  (** the file the context is for, as a normalised path *)
  entry : string option;
      (** the entry it is checked in the context of; [None] means alone *)
  chain : string list;
      (** the entry first and [file] last; empty when the file is alone *)
}

val alone : string -> t
(** [alone file] is the context of a file that is checked on its own. *)

val file : t -> string
val entry : t -> string option
val chain : t -> string list
val is_alone : t -> bool

val normalise : string -> string
(** One spelling per file: absolute where it was absolute, with no [.] or
    [..] left. Nothing here asks the filesystem, so a symbolic link is not
    resolved. *)

val equal : string -> string -> bool
(** [equal a b] compares two paths through [normalise]. *)

val requires_of_text : string -> string list
(** The specifications of every [require] the text holds, in the order it
    holds them: [NAME:path] as written, or the plain path. Comments are
    skipped. *)

val entries_of_directory : string -> string list
(** Every [.jazz] file under a directory, sorted, which is the entry set of a
    project that lists none. Hidden directories are not searched, the walk
    stops at a depth of eight and a directory it cannot read is skipped. *)

val of_file :
  entries:string list ->
  idirs:(string * string) list ->
  read:(string -> string option) ->
  string ->
  t
(** [of_file ~entries ~idirs ~read file] is the context of [file]. [entries]
    are the project's entry files, in the order the project lists them;
    [idirs] are its named include paths; [read] gives the text of a file, so
    the caller decides whether an open buffer comes before the disk.
    [entries] may hold one file the caller uses instead of the project's
    list, which is how the client's own override works. *)

val describe : t -> string
(** One line saying where the file is checked, for a log or a hover. *)
