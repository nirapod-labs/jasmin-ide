(** What the corpus harnesses share: reading the command line, walking a
    Jasmin corpus, running the compiler and the checker, running checks in
    parallel, and printing the result.

    Every harness uses the same exit status: 0 when the two sides agree, 1
    when they do not, 2 when the run could not be made (a bad argument, a
    missing corpus or compiler, a check that did not finish). *)

(** {1 Messages} *)

val name : string ref
(** The harness's name, the prefix of every line it prints. *)

val say : ('a, unit, string, unit) format4 -> 'a
(** [say "..."] prints one line, [name: ...], on stdout. *)

val warn : ('a, unit, string, unit) format4 -> 'a
(** The same on stderr. *)

val die : ('a, unit, string, 'b) format4 -> 'a
(** [die "..."] prints the line on stderr and exits 2. *)

val share : int -> int -> string
(** [share part whole] is ["part of whole (p%)"], or ["n/a"] for no whole. *)

(** {1 The command line} *)

type args = {
  positional : string list;
  value : string -> string option;  (** The value of [--key value]. *)
  flag : string -> bool;  (** Whether [--key] was given. *)
  values : string -> string list;  (** Every value of a repeated option. *)
}

val parse_args :
  usage:string ->
  flags:string list ->
  options:string list ->
  string list ->
  args
(** Reads the arguments after the program name. [-h] and [--help] print
    [usage] and exit 0; an unknown option, or an option without its value,
    prints it and exits 2. *)

val int_arg : args -> string -> default:int -> int
(** A non-negative integer option; anything else exits 2. *)

val jobs : args -> int
(** [--jobs N], at least one, default 4. *)

(** {1 The corpus} *)

val directory : string -> string
(** The absolute path of a directory that must exist; exits 2 otherwise. *)

val tests_root : string -> string
(** A release root, its [compiler] directory or its [compiler/tests]
    directory, as the [tests] directory. Exits 2 when none of the three. *)

val files : ?suffixes:string list -> string -> string list
(** Every file under a directory with one of [suffixes] (default
    [.jazz]), as paths relative to it, in byte order. Entries starting with
    [.] are skipped. *)

val arch_of : string -> string
(** The architecture a corpus path names: its deepest component among
    [x86-64], [arm-m4], [armv8a], [riscv] and [risc-v]; [x86-64] when none.
    [common] counts as none here. *)

val arches_of : string -> string list
(** As {!arch_of}, except that a path whose deepest such component is
    [common] is checked on the three architectures the release checks it on:
    [x86-64], [arm-m4] and [riscv]. *)

val group_of : string -> string
(** The folder a check is reported under: the first component of the path,
    or the first two under [sct-checker] and [safety]. *)

type check = { label : string; path : string; arch : string }

val read_checks : string -> check list
(** A check list: one [label<TAB>path<TAB>architecture] line per check.
    Lines with fewer than two fields are skipped; an empty architecture is
    [x86-64]. Exits 2 when the file cannot be read or holds no check. *)

(** {1 Programs} *)

type output = { status : int; out : string; err : string }
(** [status] is the exit code, or 128 plus the signal for a program a
    signal ended. *)

val run :
  ?cwd:string ->
  ?env:string array ->
  ?merge:bool ->
  string ->
  string list ->
  output
(** [run program arguments] runs [program] with stdin closed and collects
    its two streams; with [merge] both go to [out], in the order written. A
    program that cannot be started has status 127. *)

val executable : what:string -> string -> string
(** The absolute path of a program, looked for on [PATH] when it names no
    directory. Exits 2, naming [what], when there is none. *)

val compiler : string option -> string * string
(** The reference compiler and the line [-version] prints. [None], a path
    that does not run, and a compiler that prints no version all exit 2:
    a misnamed compiler never turns a comparison into a skip. *)

val built : string -> string
(** [built path] is [path] in the build directory this program runs from
    (the [_build/default] above it), for another program of the same build.
    Exits 2 when it is not there. *)

(** {1 Work} *)

val parallel : jobs:int -> ('a -> 'b) -> 'a list -> ('b, string) result list
(** [parallel ~jobs f items] computes [f item] for every item in a process
    of its own, at most [jobs] at once, and returns the results in order. A
    process that raises, exits or is killed gives [Error] with the reason,
    never a result. *)

val temp_dir : string -> string
(** A fresh directory under [TMPDIR], removed when the program exits. *)

val remove_tree : string -> unit
val read_file : string -> string
val write_file : string -> string -> unit
