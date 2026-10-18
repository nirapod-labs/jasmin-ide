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
    [..] left. A relative path keeps the [..] that climb out of its start,
    which is what a path to a file above the working directory needs.
    Nothing here asks the filesystem, so a symbolic link is not resolved. *)

val equal : string -> string -> bool
(** [equal a b] is true when [a] and [b] name one file: their absolute
    spellings are equal, so a relative path and the absolute one to the same
    file compare equal. *)

val absolute : string -> string
(** [absolute path] is [path] taken from the working directory when it is
    relative, then normalised. *)

type require = {
  from : string option;
      (** the include path name of a [from NAME require], or [None] for a bare
          [require] *)
  written : string;  (** the path the require writes, as it is written *)
}

val requires_of_text : string -> require list
(** The requires of a text, in the order it holds them. A bare require names
    a file beside the file that writes it; a [from NAME] one names a file
    beside the directory [NAME] is bound to, which the caller supplies to
    {!of_file}. Comments are skipped. *)

val search_root :
  ?workspace:string -> ?project:string -> string -> string option
(** [search_root ?workspace ?project file] is the directory the entries of
    [file]'s context are searched under. [project] is the directory of the
    nearest project file above [file], and [workspace] the directory the
    client opened; each is used only when it holds [file], and the project
    wins when both do. [None] when neither holds [file]: the search has no
    root, and the caller chooses what to search instead.

    The result is written as [file] is: relative to the working directory
    when [file] is relative, unless that would climb out of it, in which
    case it is absolute. *)

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
    [idirs] are its named include paths, which a [from NAME require] reads
    through and the compiler reads the same way; [read] gives the text of a
    file, so the caller decides whether an open buffer comes before the disk.
    [entries] may hold one file the caller uses instead of the project's
    list, which is how the client's own override works. *)

val describe : t -> string
(** One line saying where the file is checked, for a log or a hover. *)
