(** What a check of a file leaves behind for the engine.

    Every value here is plain data: a string, a variant or a
    [Jide_lang.Span.t]. The engine reads the facts of a checked file
    without handling the compiler's program representation, and the
    compiler's unique identifiers never appear: a declaration is identified
    by its file and the byte where its name starts. *)

type storage =
  | Reg
  | Stack
  | Param
  | Global
  | Inline
      (** The storage class of a variable: a register, a stack slot, a parameter
    fixed by the caller, a global variable, or a value the compiler
    substitutes wherever it is used. *)

type pointer =
  | Direct
  | Pointer_constant
  | Pointer_writable
      (** Whether a variable holds a plain value or a pointer, and whether a
    pointer may be written through. *)

type var_kind = { storage : storage; pointer : pointer }
(** The compiler's variable kind, as data. *)

val string_of_var_kind : var_kind -> string
(** [string_of_var_kind kind] is the kind written the way the compiler
    prints it: ["reg"], ["reg mut ptr"], ["stack"], ["stack const ptr"],
    ["param"], ["global"] or ["inline"]. *)

type annotation = {
  a_key : string;
  a_value : string option;
  a_span : Jide_lang.Span.t;
}
(** One annotation, for example [#[ct="public -> public"]]. [a_key] is the
    annotation's name, ["ct"] in the example, and [a_value] the value it
    carries, written out: the string of a quoted value (["public -> public"]),
    the name of an identifier ([bar] in [#[foo=bar]]), the digits of an
    integer ([7] in [#[foo=7]]) or the width of a width attribute ([u32] in
    [#[foo=u32]]). [a_value] is [None] when the annotation has no value, as
    [#[inline]] has none, and when its value is a structured one,
    [#[foo={...}]]. [a_span] is the range of the value, or of the name when
    the annotation has no value. *)

type var = {
  v_name : string;
  v_kind : var_kind;
  v_ty : string;
  v_dloc : Jide_lang.Span.t;
  v_dfile : string;
  v_annot : annotation list;
}
(** A variable the compiler declared. [v_ty] is its type as the compiler
    prints it, for example ["u64"], ["bool"] or ["u64[8]"]. [v_dloc] is the
    range of the declared name and [v_dfile] the file that range indexes. *)

type fun_sig = { fs_in : string list; fs_out : string list }
(** A function's signature: the types of its arguments and of its results,
    printed as [v_ty] is. *)

type use = {
  u_span : Jide_lang.Span.t;
  u_name : string;
  u_item : Jide_lang.Span.t;
  u_decl : Jide_lang.Span.t;
  u_dfile : string;
  u_file : string;
  u_reading : string;
}
(** One variable occurrence in a typed body, for example the [a] in
    [b = a]. [u_span] is the range of the occurrence itself and [u_file] the
    file that range indexes: the checked file for an occurrence the file
    writes, and a required file for one the parser spliced into the checked
    file from a [require]. [u_name] is the name the compiler holds the
    variable by at it. [u_item] is the range of the item the occurrence
    belongs to, that item's {!i_span}. [u_decl] is the range of the
    declaration the compiler bound the occurrence to, which is the {!v_dloc}
    of that variable, and [u_dfile] the file the range indexes; together they
    are the declaration's identity. [u_reading] is the namespace path the
    occurrence's item was typed in, the path joined by [::] and empty at the
    top level: a file a namespace requires is typed once per namespace, so a
    use is one reading of its byte, and the reading tells the readings apart. An occurrence the compiler made up itself,
    which has no place in the source, is not reported. *)

type call = {
  c_span : Jide_lang.Span.t option;
  c_callee : string;
  c_item : Jide_lang.Span.t;
  c_range : Jide_lang.Span.t;
  c_file : string;
}
(** One call in a typed body, for example the [add (s)] of [s = add(s)].
    [c_callee] is the name the compiler typed the call against. [c_span] is
    the range that name was written at, and it is [None] when the syntax holds
    no such range for the call. [c_range] is the range of the whole instruction
    that makes the call, so [c_span] lies inside it, and [c_item] is the range
    of the item the instruction belongs to, that item's {!i_span}. The
    compiler's typed call keeps the function's name and not the range it was
    written at, so [c_span] comes from the syntax instruction with the same
    range. One instruction can make several calls, a declaration with several
    initialisers does, and those calls are paired with the syntax in the order
    they are written. A call with no name range is still reported, so that a
    reader counts it.

    [c_file] is the file both ranges index. It is the checked file for a call
    the file writes itself, and the required file for a call written in a
    [.jinc] the checked file pulls in: the parser splices a required file's
    items into the requiring one, and their ranges stay in the required file's
    own bytes. A reader that takes the ranges for the checked file's bytes
    reads another file's text, so the field is what keeps the two apart. *)

type item_kind = Function | Param | Global

type item = {
  i_kind : item_kind;
  i_span : Jide_lang.Span.t;
  i_name : string;
  i_name_span : Jide_lang.Span.t;
  i_annot : annotation list;
  i_sig : fun_sig option;
  i_vars : var list;
}
(** One item of the checked file. [i_span] is the item's range and
    [i_name_span] the range of the name it declares. [i_sig] is set for a
    function. [i_vars] holds the variables the item declares, a function's
    parameters and results included. A declaration with neither an
    initialiser nor a use is not there: the compiler's typing discards it,
    and these facts are the compiler's answer. *)

type t = { items : item list; uses : use list; calls : call list }
(** [items] are the items of the checked file, in source order, and they all
    belong to that file. [uses] are the variable occurrences the bodies of the
    checked file's items hold, and the bodies of the files its top-level
    [require] items reach, each file's uses in source order by the byte an
    occurrence starts at. A use names its file in {!use.u_file} and the
    namespace it was typed in in {!use.u_reading}, so a use of a required file
    is told apart from the document's own. A parameter and a global have no body
    and add none. [calls] are the calls the checked file's bodies make, including
    those of a file a namespace splices in, as {!call.c_file} says; a file a
    top-level [require] reaches contributes no calls and no items. An item the
    compiler never typed, and a body it never reached, add none. *)

val empty : t

val merge : t -> t -> t
(** [merge earlier later] holds both sets of items and both sets of uses and
    calls, in that order. *)
