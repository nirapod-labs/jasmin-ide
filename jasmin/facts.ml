type storage = Reg | Stack | Param | Global | Inline
type pointer = Direct | Pointer_constant | Pointer_writable
type var_kind = { storage : storage; pointer : pointer }

let string_of_var_kind (kind : var_kind) : string =
  let storage =
    match kind.storage with
    | Reg -> "reg"
    | Stack -> "stack"
    | Param -> "param"
    | Global -> "global"
    | Inline -> "inline"
  in
  match kind.pointer with
  | Direct -> storage
  | Pointer_constant -> storage ^ " const ptr"
  | Pointer_writable -> storage ^ " mut ptr"

type annotation = {
  a_key : string;
  a_value : string option;
  a_span : Jide_lang.Span.t;
}

type var = {
  v_name : string;
  v_kind : var_kind;
  v_ty : string;
  v_dloc : Jide_lang.Span.t;
  v_dfile : string;
  v_annot : annotation list;
}

type fun_sig = { fs_in : string list; fs_out : string list }

type use = {
  u_span : Jide_lang.Span.t;
  u_name : string;
  u_item : Jide_lang.Span.t;
  u_decl : Jide_lang.Span.t;
  u_dfile : string;
}

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

type t = { items : item list; uses : use list }

let empty = { items = []; uses = [] }

let merge (earlier : t) (later : t) =
  { items = earlier.items @ later.items; uses = earlier.uses @ later.uses }
