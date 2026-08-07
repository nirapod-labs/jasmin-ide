open Jasmin
module L = Location
module P = Prog
module S = Syntax

(** [span location] is the range [location] names, clamped to the start of
    the file. *)
let span (location : L.t) : Jide_lang.Span.t = Error_span.of_location location

let string_of_pty (ty : P.pty) : string =
  Format.asprintf "%a" (Printer.pp_ptype ~debug:false) ty

let string_of_epty (ty : P.epty) : string =
  Format.asprintf "%a" (Printer.pp_eptype ~debug:false) ty

let pointer_of (reference : Wsize.reference) : Facts.pointer =
  match reference with
  | Wsize.Direct -> Facts.Direct
  | Wsize.Pointer Wsize.Constant -> Facts.Pointer_constant
  | Wsize.Pointer Wsize.Writable -> Facts.Pointer_writable

let kind_of (kind : Wsize.v_kind) : Facts.var_kind =
  match kind with
  | Wsize.Const -> { Facts.storage = Facts.Param; pointer = Facts.Direct }
  | Wsize.Stack reference ->
      { Facts.storage = Facts.Stack; pointer = pointer_of reference }
  | Wsize.Reg (_, reference) ->
      { Facts.storage = Facts.Reg; pointer = pointer_of reference }
  | Wsize.Inline -> { Facts.storage = Facts.Inline; pointer = Facts.Direct }
  | Wsize.Global -> { Facts.storage = Facts.Global; pointer = Facts.Direct }

(** [value_of attribute] is the value [attribute] carries, written out as the
    compiler holds it. Every scalar form is kept: a quoted string, an
    identifier, an integer and a width. A structured value has no single
    string, and an annotation written without a value carries none.

    A quoted value is a [PAexpr] string in the syntax tree and arrives here as
    [Astring], because the compiler's own [pannot_to_annotation] turns
    [PEstring] into [Astring] (vendor/jasmin/src/functionAnnotations.ml), which
    is the same value the keyword form arrives as: [#[ct="public -> public"]]
    and [#[returnaddress=stack]] are both read this way. *)
let value_of (attribute : Annotations.simple_attribute) : string option =
  match attribute with
  | Annotations.Astring value -> Some value
  | Aid name -> Some name
  | Aint number -> Some (Z.to_string number)
  | Aws width -> Some (Annotations.string_of_ws width)
  | Astruct _ -> None

let annotation_of (key : Annotations.pident)
    (attribute : Annotations.attribute option) : Facts.annotation =
  let a_key = L.unloc key in
  let a_span, a_value =
    match attribute with
    | Some { L.pl_desc = value; L.pl_loc = location } ->
        (span location, value_of value)
    | None -> (span (L.loc key), None)
  in
  { Facts.a_key; a_value; a_span }

let annotations_of (annotations : Annotations.annotations) :
    Facts.annotation list =
  List.map (fun (key, attribute) -> annotation_of key attribute) annotations

let var_of (v : P.pvar) : Facts.var =
  {
    Facts.v_name = v.P.v_name;
    v_kind = kind_of v.P.v_kind;
    v_ty = string_of_pty v.P.v_ty;
    v_dloc = span v.P.v_dloc;
    v_dfile = v.P.v_dloc.loc_fname;
    v_annot = annotations_of v.P.v_annot;
  }

(** [dedupe key elements] keeps the first element of each [key]. *)
let dedupe (key : 'a -> 'b) (elements : 'a list) : 'a list =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun element ->
      let k = key element in
      if Hashtbl.mem seen k then false
      else (
        Hashtbl.add seen k ();
        true))
    elements

let var_key (v : P.pvar) : string * string * int =
  (v.P.v_name, v.P.v_dloc.loc_fname, v.P.v_dloc.loc_bchar)

(** [declared_in ~file ~item_span v] is true when [v] is declared inside
    [item_span] in [file]. *)
let declared_in ~(file : string) ~(item_span : Jide_lang.Span.t) (v : P.pvar) :
    bool =
  let location = v.P.v_dloc in
  (not (L.isdummy location))
  && String.equal location.loc_fname file
  && Jide_lang.Span.contains item_span ~byte:location.loc_bchar

(* Every variable a typed body mentions. One occurrence is both a candidate
   declaration, because the facts' variables are read off the same list by
   keeping the ones declared inside the item, and a use, because the
   compiler keeps the range the occurrence is written at beside the variable
   it bound it to. That range is what a resolver's answer is compared with.
   A declaration that has no initialiser and no use leaves no instruction
   behind, so it cannot be seen here; that is the compiler's own answer, not
   a choice. *)
type occurrences = { vars : P.pvar list; uses : (L.t * P.pvar) list }

let no_occurrence : occurrences = { vars = []; uses = [] }

let add_use (found : occurrences) (location : L.t) (variable : P.pvar) :
    occurrences =
  { vars = variable :: found.vars; uses = (location, variable) :: found.uses }

let rec expr_vars (found : occurrences) (e : P.pexpr) : occurrences =
  match e with
  | P.Pconst _ | P.Pbool _ -> found
  | P.Parr_init (_, length) -> length_vars found length
  | P.Pvar variable -> variable_vars found variable
  | P.Pget (_, _, _, variable, index) ->
      expr_vars (variable_vars found variable) index
  | P.Psub (_, _, length, variable, index) ->
      length_vars (expr_vars (variable_vars found variable) index) length
  | P.Pload (_, _, pointer) -> expr_vars found pointer
  | P.Papp1 (_, argument) -> expr_vars found argument
  | P.Papp2 (_, left, right) -> expr_vars (expr_vars found left) right
  | P.PappN (_, arguments) -> List.fold_left expr_vars found arguments
  | P.Pif (ty, condition, then_, else_) ->
      expr_vars
        (expr_vars (expr_vars (type_vars found ty) condition) then_)
        else_

and length_vars (found : occurrences) (length : P.pexpr_) : occurrences =
  match length with P.PE e -> expr_vars found e

and type_vars (found : occurrences) (ty : P.pty) : occurrences =
  match ty with
  | P.Bty _ -> found
  | P.Arr (_, length) -> length_vars found length

and variable_vars (found : occurrences) (variable : P.pexpr_ P.ggvar) :
    occurrences =
  add_use found (L.loc variable.P.gv) (L.unloc variable.P.gv)

and var_i_vars (found : occurrences) (variable : P.pexpr_ P.gvar_i) :
    occurrences =
  add_use found (L.loc variable) (L.unloc variable)

and lval_vars (found : occurrences) (lval : P.plval) : occurrences =
  match lval with
  | P.Lnone (_, ty) -> type_vars found ty
  | P.Lvar variable -> var_i_vars found variable
  | P.Lmem (_, _, _, pointer) -> expr_vars found pointer
  | P.Laset (_, _, _, variable, index) ->
      expr_vars (var_i_vars found variable) index
  | P.Lasub (_, _, length, variable, index) ->
      length_vars (expr_vars (var_i_vars found variable) index) length

and lvals_vars (found : occurrences) (lvals : P.plvals) : occurrences =
  List.fold_left lval_vars found lvals

and assertion_vars (found : occurrences) (assertion : P.pexpr_ P.gassert) :
    occurrences =
  match assertion with
  | P.Pexpr e -> expr_vars found e
  | P.PappN_safety (_, arguments) -> List.fold_left expr_vars found arguments
  | P.Pis_var_init variable -> var_i_vars found variable
  | P.Pis_mem_init (left, right) -> expr_vars (expr_vars found left) right
  | P.Pand (left, right) -> assertion_vars (assertion_vars found left) right

and instr_vars (found : occurrences) (instr : (unit, 'asm) P.pinstr_r) :
    occurrences =
  match instr with
  | P.Cassgn (lval, _, ty, e) ->
      expr_vars (type_vars (lval_vars found lval) ty) e
  | P.Copn (lvals, _, _, arguments) ->
      List.fold_left expr_vars (lvals_vars found lvals) arguments
  | P.Csyscall (lvals, _, arguments) ->
      List.fold_left expr_vars (lvals_vars found lvals) arguments
  | P.Cassert (_, assertion) -> assertion_vars found assertion
  | P.Cif (condition, then_, else_) ->
      stmt_vars (stmt_vars (expr_vars found condition) then_) else_
  | P.Cfor (variable, (_, start, stop), body) ->
      stmt_vars
        (expr_vars (expr_vars (var_i_vars found variable) start) stop)
        body
  | P.Cwhile (_, first, condition, _, second) ->
      stmt_vars (stmt_vars (expr_vars found condition) first) second
  | P.Ccall (lvals, _, arguments) ->
      List.fold_left expr_vars (lvals_vars found lvals) arguments

and stmt_vars (found : occurrences) (body : (unit, 'asm) P.pstmt) : occurrences
    =
  List.fold_left (fun found instr -> instr_vars found instr.P.i_desc) found body

(** [uses_of ~item_span found] is the body's occurrences as facts, oldest
    first. An occurrence the compiler made up itself has no place in the
    source and is left out: its location is dummy. *)
let uses_of ~(item_span : Jide_lang.Span.t) (found : occurrences) :
    Facts.use list =
  let use_of (location : L.t) (variable : P.pvar) : Facts.use option =
    if L.isdummy location then None
    else
      Some
        {
          Facts.u_span = span location;
          u_name = variable.P.v_name;
          u_item = item_span;
          u_decl = span variable.P.v_dloc;
          u_dfile = variable.P.v_dloc.loc_fname;
        }
  in
  List.filter_map
    (fun (location, variable) -> use_of location variable)
    found.uses
  |> List.stable_sort (fun (left : Facts.use) (right : Facts.use) ->
         Stdlib.Int.compare left.Facts.u_span.Jide_lang.Span.start_byte
           right.Facts.u_span.Jide_lang.Span.start_byte)

let function_item ~(file : string) ~(env : 'asm Pretyping.Env.env)
    ~(item_span : Jide_lang.Span.t) ~(name_span : Jide_lang.Span.t)
    (func : (unit, 'asm) P.pfunc) : Facts.t =
  let body = stmt_vars no_occurrence func.P.f_body in
  (* A function's results are the variables [return] names, and the compiler
     keeps the range of each name there, so a result is an occurrence at its
     [return] as well as the declaration it stands for. *)
  let returned =
    List.fold_left
      (fun found result -> add_use found (L.loc result) (L.unloc result))
      body func.P.f_ret
  in
  let candidates =
    func.P.f_args @ List.map L.unloc func.P.f_ret @ returned.vars
  in
  let declared =
    dedupe var_key (List.filter (declared_in ~file ~item_span) candidates)
  in
  let signature =
    match Pretyping.Env.Funs.find func.P.f_name.P.fn_name env with
    | Some (_, fun_sig) ->
        Some
          {
            Facts.fs_in = List.map string_of_epty fun_sig.Pretyping.fs_tin;
            fs_out = List.map string_of_epty fun_sig.Pretyping.fs_tout;
          }
    | None -> None
  in
  {
    Facts.items =
      [
        {
          Facts.i_kind = Facts.Function;
          i_span = item_span;
          i_name = func.P.f_name.P.fn_name;
          i_name_span = name_span;
          i_annot = annotations_of func.P.f_annot.FInfo.f_user_annot;
          i_sig = signature;
          i_vars = List.map var_of declared;
        };
      ];
    uses = uses_of ~item_span returned;
  }

let variable_item (kind : Facts.item_kind) ~(item_span : Jide_lang.Span.t)
    ~(name_span : Jide_lang.Span.t) (v : P.pvar) : Facts.item =
  {
    Facts.i_kind = kind;
    i_span = item_span;
    i_name = v.P.v_name;
    i_name_span = name_span;
    i_annot = annotations_of v.P.v_annot;
    i_sig = None;
    i_vars = [ var_of v ];
  }

(** [leaves item] is [item] with namespaces flattened: the items a fact can
    be made for, in source order. *)
let rec leaves (item : S.pitem L.located) : S.pitem L.located list =
  match L.unloc item with
  | S.PNamespace (_, items) -> List.concat_map leaves items
  | _ -> [ item ]

let decl_location (decl : (unit, 'asm) P.pmod_item) : L.t option =
  match decl with
  | P.MIfun func -> Some func.P.f_loc
  | P.MIparam (v, _) -> Some v.P.v_dloc
  | P.MIglobal (v, _) -> Some v.P.v_dloc

(** [declares item_span decl] is true when [decl] is the typed declaration
    of the item occupying [item_span]. A function's typed location is the
    item's, and a parameter's or a global's is its name's, which lies
    inside it. *)
let declares ~(item_span : Jide_lang.Span.t) (decl : (unit, 'asm) P.pmod_item) :
    bool =
  match decl_location decl with
  | None -> false
  | Some location ->
      (not (L.isdummy location))
      && Jide_lang.Span.contains item_span ~byte:location.loc_bchar

let item_of_leaf ~(file : string) ~(env : 'asm Pretyping.Env.env)
    ~(decls : (unit, 'asm) P.pmod_item list) (leaf : S.pitem L.located) :
    Facts.item option =
  let item_span = span (L.loc leaf) in
  let find () = List.find_opt (declares ~item_span) decls in
  match L.unloc leaf with
  | S.PFundef definition -> (
      match find () with
      | Some (P.MIfun func) ->
          Some
            (function_item ~file ~env ~item_span
               ~name_span:(span (L.loc definition.S.pdf_name))
               func)
      | _ -> None)
  | S.PParam definition -> (
      match find () with
      | Some (P.MIparam (v, _)) ->
          Some
            (variable_item Facts.Param ~item_span
               ~name_span:(span (L.loc definition.S.ppa_name))
               v)
      | _ -> None)
  | S.PGlobal definition -> (
      match find () with
      | Some (P.MIglobal (v, _)) ->
          Some
            (variable_item Facts.Global ~item_span
               ~name_span:(span (L.loc definition.S.pgd_name))
               v)
      | _ -> None)
  | S.PTypeAlias _ | S.Pexec _ | S.Prequire _ | S.PNamespace _ -> None

let of_item ~(file : string) ~(env : 'asm Pretyping.Env.env)
    ~(item : S.pitem L.located) ~(decls : (unit, 'asm) P.pmod_item list) :
    Facts.t =
  let items =
    List.filter_map
      (fun leaf -> item_of_leaf ~file ~env ~decls leaf)
      (leaves item)
  in
  { Facts.items }
