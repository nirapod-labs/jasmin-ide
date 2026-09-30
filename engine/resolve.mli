(** What each identifier occurrence in a file names.

    The rules are pretyping's, because they are the language's: a name is
    defined before it is used, there are no forward declarations and no
    recursion for functions, a declaration in a block ends with the block, a
    [for] variable must already exist, a namespace is searched from the
    innermost outward, a file is read once per namespace, and an alias is
    used where it was declared.

    Where the compiler has an answer it wins: [Env.Funs.find] for a
    function, and a typed variable use's [v_dloc]. The typed facts of design
    section 9 are exposed by the adapter lane, and until that interface
    lands the resolver is the only answer for every name. The interface here
    does not change when it does: a [Resolved] declaration is the same
    (file, byte offset) either way, so the compiler's answer can replace the
    resolver's without touching a consumer.

    The identity of a declaration is (absolute file, byte offset of the
    name), never a compiler unique id: the ids change at every re-check. *)

type answer =
  | Resolved of Index.declaration
  | Intrinsic  (** a name after [\#]: a fixed intrinsic, not a declaration *)
  | Annotation  (** an annotation label: not a symbol *)
  | Include_path  (** the [from NAME] of a require: a configuration name *)
  | Unresolved

type t

val of_text :
  idirs:(string * string) list ->
  read:(string -> string option) ->
  file:string ->
  string ->
  t
(** [of_text ~idirs ~read ~file text] resolves [text] as [file] and every
    file it requires, through [read]. [idirs] are the named include paths a
    [from NAME] require uses. Every [byte] below is a byte of the document as
    the caller holds it, a leading byte-order mark included. Nothing here
    raises. *)

val of_file :
  idirs:(string * string) list ->
  read:(string -> string option) ->
  string ->
  t option
(** [of_file ~idirs ~read file] is {!of_text} on the bytes [read] gives for
    [file], and [None] when it has none. *)

val entry : t -> string

val files : t -> Index.t list
(** Every file the resolution reached, entry first and in the order it was
    first read. *)

val file : t -> string -> Index.t option
(** [file t path] is the index of [path] when the resolution reached it. *)

val answer_at :
  t -> file:string -> byte:int -> (Index.occurrence * answer) option
(** [answer_at t ~file ~byte] is the occurrence holding [byte] and what it
    names, when the resolution reached that file and an occurrence holds the
    byte. A file read into more than one namespace has one answer per
    reading (see {!visit_answers}); this one is the first reading's. *)

val answers : t -> string -> (Index.occurrence * answer) list
(** [answers t file] is every occurrence of [file] with its answer, in
    source order, the first reading's. An occurrence the resolver never
    reached is [Unresolved]. *)

val visit_answers :
  t -> file:string -> byte:int -> Index.occurrence option * answer list
(** [visit_answers t ~file ~byte] is the occurrence holding [byte] in [file],
    if one holds it, and what each reading of [file] names there: one answer
    per time the resolution read the file, in the order it read it. A file
    that a namespace reads is read once per namespace, and the namespaces may
    hold different declarations, so the readings may disagree. The list is
    empty when the resolution never reached the file. *)

val loaded : t -> (string * string) list
(** The (namespace key, file) pairs the resolver read once each, for the
    tests of the require rule. *)

type binding = { name : string; declaration : Index.declaration }
(** One name in scope and what it names. [name] is the name a use of it has
    to write where it is in scope, which is not always the declaration's own
    [Index.name]: a namespace merges its contents into its parent qualified
    with its own name, so outside the namespace [A::mul] is the name in scope
    for a function written [mul] inside [namespace A]. *)

type scope = { types : binding list; funs : binding list; vars : binding list }
(** The declarations in scope at one byte, one list per name space the
    compiler keeps: type aliases, functions and variables. Each list holds
    one entry per visible declaration, the innermost frame's names first and
    in order within a frame; a name declared twice in nested frames is
    listed once, with the declaration a lookup would find. A function a
    [require] brought in is in [funs]: the walk of a required file leaves its
    declarations in the frames of the file that required it. *)

val scope_at : t -> file:string -> byte:int -> scope option
(** [scope_at t ~file ~byte] is the scope the walk had reached at that byte
    of [file], or [None] when the resolution never reached the file. The
    scope is the resolver's own, read from the environment it built as it
    walked: it is not computed again from the declarations. A file read into
    more than one namespace answers from the last visit. *)

val declaration_of_answer : answer -> Index.declaration option
