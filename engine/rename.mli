(** Rename a symbol, by identity.

    The identity of a symbol is the absolute file and the byte offset of its
    declaration's name, never a compiler unique id (design section 9). Both
    functions here read the index of the file the client is in, the
    resolution over that file and, for the project's other files, a
    resolution per file; neither parses, resolves or checks anything itself,
    so a rename costs no compiler work and answers from the snapshot.

    Every occurrence the rename rewrites was resolved, at its own byte, to
    the very declaration being renamed. Nothing is matched by name alone
    outside the resolver's reach, so a same-named declaration in another
    block, another file or another namespace keeps its own occurrences.

    Three kinds of name are refused because they are not symbols of the
    program: a keyword, an intrinsic ([#spill(x)]) and an annotation label
    ([[#ct]]). A namespace's own name is refused too, and not for taste: a
    qualified name [A::x] has one range for the whole name, so the index
    holds no range for the [A] a rename of the namespace would have to
    rewrite, and rewriting the whole name would delete the qualification. *)

type edit = { span : Jide_lang.Span.t; new_text : string }
(** One replacement: the bytes [span] covers become [new_text]. *)

type edits = (string * edit list) list
(** Edits grouped by file, one group per file, each group in source order.
    The files are the ones the search met first: the document, then the files
    its resolution read, then the project's files in the order they were
    given. The path is the one the index holds. *)

val valid_name : string -> bool
(** [valid_name name] is true when [name] is one Jasmin identifier as the
    lexer defines one: a letter or [_], then letters, digits and [_]. A
    keyword is not one, because the lexer would read it as its own token,
    and neither is a qualified name. *)

val prepare :
  index:Index.t ->
  resolve:Resolve.t ->
  byte:int ->
  (Jide_lang.Span.t * Index.kind) option
(** [prepare ~index ~resolve ~byte] is the range a rename at [byte] would
    rewrite and the kind of the declaration it belongs to, or [None].

    [None] for a position that holds no identifier (a keyword, a literal, the
    inside of a comment or a string, blank space), for an intrinsic, for an
    annotation label, for an include path, for a name the resolver found no
    declaration for, for a namespace's own name, for the namespace part of a
    qualified name (the [A] of [A::x], whose last segment is the name a
    rename would rewrite), and for a qualified name written with spaces
    around [::], whose last segment has no range.

    The range is the last segment of a qualified name when no space surrounds
    its [::], and the whole identifier otherwise, which is the rule the index
    uses for the same names. A caller that shows the current name in its
    input box reads it from the text with this range. *)

val rename :
  files:Resolve.t list ->
  index:Index.t ->
  resolve:Resolve.t ->
  byte:int ->
  new_name:string ->
  broken:(string -> bool) ->
  (edits, string) result
(** [rename ~files ~index ~resolve ~byte ~new_name ~broken] is every
    occurrence of the identity at [byte], in the document, in the files its
    resolution read and in the project's files, as edits grouped by file in
    source order; or a refusal with the reason to show the user.

    [files] holds one resolution per project file the caller knows about;
    each is searched over every file it reached. A file reached by more than
    one resolution is searched once, through the first resolution that
    reached it, so the document's own resolution is the one its answers come
    from. A project file no resolution reached contributes no edit: its
    occurrences cannot be attributed to an identity, and a rename that
    guessed would rename a stranger's declaration.

    [broken path] is true when [path] has recovered syntax errors. The rename
    is refused when any file in [resolve]'s require closure is broken, which
    is design section 10's rule: the index of a file recovery rewrote is not
    one to edit other files from.

    The refusals are, in this order:

    - the new name is not a valid Jasmin identifier;
    - a file in the require closure of the document has recovered syntax
      errors;
    - the position holds no name, or the name it holds is a keyword, an
      intrinsic, an annotation label, an include path or a namespace's own
      name, or it is the namespace part of a qualified name, or it names no
      declaration;
    - an occurrence of the symbol is a qualified name written with spaces
      around [::], whose last segment has no range of its own, so rewriting
      it would delete the qualification;
    - the new name is already the name of a different declaration in the
      scope of one of the occurrences. That is the rule that keeps a rename
      safe: the scope the resolver built at that byte is asked whether
      [new_name] names something else there, and a qualified occurrence is
      asked about the qualified name it would become ([A::new], not [new]).

    A rename to the name the symbol already has is answered with the edits it
    would make, which replace each occurrence with itself. *)
