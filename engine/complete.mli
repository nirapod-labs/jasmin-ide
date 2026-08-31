(** Completion and signature help, from a snapshot.

    Neither function here parses, resolves or checks a document. The input is
    the index of the file the client is asking about, the resolver over it
    and, for signature help, the typed facts of the last check; every answer
    is read from those three. A request therefore costs no compiler work and
    answers while the file is broken.

    The parser drops an unfinished call, so the call signature help is asked
    about is read from the text before the offset and never from the tree. *)

type kind =
  | Keyword  (** a word the lexer has a token for *)
  | Function
  | Parameter
  | Variable
  | Global
  | Type_alias
  | Annotation_key  (** a label of an annotation, after [#] *)

type item = { label : string; kind : kind; detail : string option }
(** One completion candidate. [detail] is the declaration's own rendering
    when the index has one: a function's signature as written, a variable's
    storage and type, an alias's definition. It is a substring of the file,
    never invented text. *)

type completion = {
  items : item list;
  prefix : string;  (** the word being typed, from its start to the offset *)
  span : Jide_lang.Span.t;
      (** The range of that word, so a client can replace it. *)
}

type trigger =
  | Hash
  | Colon
      (** The character a client triggered the request with, when it names one.
    Design section 10 gives completion the two trigger characters [#] and
    [:], and both ask for the annotation keys: after [#] they are the labels
    of the annotation being written, and a [:] is the fixed list the design
    assigns to that trigger. *)

val keywords : string list
(** The lexer's own keyword table, in the lexer's order and without the
    wildcard [_], which is written where a value is dropped and never typed
    as a name. *)

val annotation_keys : string list
(** The annotation labels the compiler reads, sorted: [stacksize],
    [calldepth], [ct], [sct] and the rest. *)

val completion :
  ?trigger:trigger ->
  index:Index.t ->
  resolve:Resolve.t ->
  byte:int ->
  unit ->
  completion
(** [completion ~index ~resolve ~byte ()] is what the client shows at [byte] of
    [index]'s file.

    After a [#] the candidates are the annotation keys. Everywhere else they
    are the lexer's keywords and the names the resolver's scope holds at that
    byte: the type aliases, the functions and the variables, which is where a
    function a [require] brought in already is. The list is filtered by the
    word being typed and sorted by label; an empty prefix lists everything.
    Nothing is looked up that is not in [index] and [resolve]: there is no
    re-check, and no file is read. *)

type signature = {
  label : string;  (** the signature as the editor prints it, callee first *)
  parameters : string list;  (** one label per parameter, in order *)
  active : int;
      (** The 0-based parameter the offset is in, from the commas the scan
          met: the parser drops an unfinished call, so the text is the only
          witness. A count past the last parameter is reported as it is. *)
  callee : string;  (** the name as it is written at the call *)
}

val signature_help :
  ?facts:Jide_jasmin.Facts.t ->
  index:Index.t ->
  resolve:Resolve.t ->
  byte:int ->
  unit ->
  signature option
(** [signature_help ~facts ~index ~resolve ~byte ()] is the signature of the
    call whose argument list holds [byte], when there is one.

    The parameter types are the checked file's own: the [fun_sig] the
    compiler left for the declaration the resolver names, matched by the
    declaration's identity, its file and the byte where its name starts.
    When the facts do not hold one (a declaration in a required file, or a
    check that failed before typing it) the signature is the declaration's
    own source text from the index, and its parameters are the groups
    between its parentheses. [None] means no call, or a call whose callee
    names no function the resolver reaches.

    An intrinsic call ([#spill(x)]) is answered by the intrinsic table, which
    is not in this tree yet, so it reports [None] for now. *)
