(** Rename a symbol, by identity.

    The identity of a symbol is the absolute file and the byte offset of its
    declaration's name, never a compiler unique id. Both
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

val reserved_function_name : string -> bool
(** [reserved_function_name name] is true for the names the compiler reads a
    call to as something other than a function call: its flag combinations
    ([_EQ], [_uLT], ...), which it also refuses as a function's name, and the
    predicates an assertion is written with. No function is renamed to one. *)

val prepare :
  files:Resolve.t list ->
  index:Index.t ->
  resolve:Resolve.t ->
  byte:int ->
  broken:(string -> bool) ->
  (Jide_lang.Span.t * Index.kind, string) result
(** [prepare ~files ~index ~resolve ~byte ~broken] is the range a rename at
    [byte] would rewrite and the kind of the declaration it belongs to, or the
    reason no rename is offered.

    It refuses with the same reasons and the same words as {!rename}, in the
    same order, for everything that does not depend on the new name: a file in
    the require closure that has syntax errors, a position that holds no
    identifier (a keyword, a literal, the inside of a comment or a string,
    blank space), an intrinsic, an annotation label, an include path, a name
    no resolution found a declaration for, an occurrence that means the
    declaration at one require site of its file and another declaration at
    another, a namespace's own name, the namespace part of a qualified name
    (the [A] of [A::x], whose last segment is the name a rename would
    rewrite), and a qualified name written with spaces around [::], whose
    last segment has no range. A caller that offers a rename only when this
    answers [Ok] never shows its input box for a refusal that does not depend
    on the new name. The reasons that do (an invalid identifier, a reserved
    function name, and every change of meaning the rename would make) are
    decided by {!rename}, which is the first to hold the name.

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
    occurrence of the declaration at [byte], in the document, in the files its
    resolution read and in the files every resolution in [files] read, as
    edits grouped by file in source order; or a refusal with the reason to
    show the user.

    [resolve] is the document's own resolution and [files] holds one
    resolution per project file the caller knows about, typically the entries
    that read the document. The name at [byte] is resolved by the first of
    them that resolves it, so a file checked in the context of an entry is
    renamed from the names that entry gives it. A file every resolution read
    with the same bytes is searched through all of them. A project file no
    resolution reached contributes no edit: its occurrences cannot be
    attributed to a declaration, and a rename that guessed would rename a
    stranger's.

    A declaration is its text, so a file read into two namespaces has both of
    its instances renamed by the one edit, and every use of either instance
    with them.

    [broken path] is true when the caller knows [path] to be unfit to edit
    from; a file whose parse reported an error is refused without it.

    The refusals are, in this order:

    - the new name is not a valid Jasmin identifier;
    - a file in the require closure of the document has syntax errors;
    - the position holds no name, or the name it holds is a keyword, an
      intrinsic, an annotation label, an include path or a namespace's own
      name, or it is the namespace part of a qualified name, or it names no
      declaration;
    - an occurrence in a file that more than one reading brings in (two
      namespaces, or two entries) means the declaration at one and another
      declaration at the other. Such a file has one text, so an edit of it
      would change both meanings; the refusal names them;
    - an occurrence of the symbol is a qualified name written with spaces
      around [::], whose last segment has no range of its own, so rewriting
      it would delete the qualification;
    - the declaration is a function and the new name is
      {!reserved_function_name};
    - the rename would change the meaning of the program. The edits are made
      in memory and every resolution that reads an edited file is walked
      again ({!Resolve.replay}); the rename is answered only when every
      occurrence in each of its files means, at every site, the declaration
      it meant before, and the compiler would report the same declarations
      as declared twice ({!Resolve.duplicates}). A name the new one would
      capture or shadow, a second function or alias of one name, and a
      variable declared twice in one frame are each such a change, refused
      with the declaration it collides with; so is a rename that would end a
      duplicate the compiler reports now, because the renamed program would
      then not give the same verdict, and a use of the new name that has no
      declaration now and would have one. A file that the rename would make
      parse differently is refused too, although a valid identifier never
      does that.

    A rename to the name the symbol already has is answered with the edits it
    would make, which replace each occurrence with itself. *)
