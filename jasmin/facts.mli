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
    annotation's name, ["ct"] in the example, and [a_value] the string it
    carries; [a_value] is [None] for every other form of value. [a_span] is
    the range of the value, or of the name when the annotation has no
    value. *)

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

type t = { items : item list }
(** [items] are the items of one checked file, in source order, and they
    all belong to that file. *)

val empty : t

val merge : t -> t -> t
(** [merge earlier later] holds both sets of items, in that order. *)
