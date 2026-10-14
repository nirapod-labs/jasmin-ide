(** Document highlights: every occurrence of one symbol in one file.

    Read from the index and the resolver, never from a check, so the highlights
    of a document are answered from the snapshot the client already has.

    The occurrence's role is what the index records: a [Declared] occurrence
    is where the name is introduced, a [Var] occurrence is a variable read or
    written, a [Call] is a function used, and a [Type] is an alias used. The
    index does not record whether a variable occurrence reads or writes it:
    [lvalues] and [use_ident] both write [Var], and there is nothing else in
    the occurrence to tell them apart. So a variable's occurrences are all
    reported as [Text]; a caller that needs read and write apart has to ask
    the syntax tree for the assignment's place, which is a change in [Index],
    not here. The [Read] and [Write] constructors exist so the protocol's
    three kinds have one spelling in the engine, and no occurrence is
    reported with either today. *)

type kind =
  | Text  (** an occurrence whose role the index does not distinguish *)
  | Read  (** never reported today; see the module comment *)
  | Write  (** never reported today; see the module comment *)

type highlight = { span : Jide_lang.Span.t; kind : kind }

val at : index:Index.t -> resolve:Resolve.t -> byte:int -> highlight list
(** [at ~index ~resolve ~byte] is every occurrence of the symbol at [byte] in
    [index]'s file, in source order, including the declaration that [byte] is
    on. The symbol is taken by identity, so a same-named declaration in
    another block of the same file is not included.

    Empty for a position that holds no name, for the namespace part of a
    qualified name, and for a name the resolver found no declaration for: an
    unresolved name has no identity to highlight. An intrinsic is highlighted
    at every occurrence of the same intrinsic, because an intrinsic is one
    symbol of the language; likewise an annotation label at every occurrence
    of the same label. A namespace's own name is not highlighted: a qualified
    name carries one range for the whole name, so the occurrences a client
    would want lit are not the ones the index can name by that identity. *)
