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

let annotation_of (key : Annotations.pident)
    (attribute : Annotations.attribute option) : Facts.annotation =
  let a_key = L.unloc key in
  let a_span, a_value =
    match attribute with
    | Some { L.pl_desc = Annotations.Astring value; L.pl_loc = location } ->
        (span location, Some value)
    | Some { L.pl_loc = location; _ } -> (span location, None)
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

(* The variables a typed body mentions. A declaration that has no
   initialiser and no use leaves no instruction behind, so it cannot be
   seen here; that is the compiler's own answer, not a choice. *)
let rec expr_vars (found : P.pvar list) (e : P.pexpr) : P.pvar list =
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

and length_vars (found : P.pvar list) (length : P.pexpr_) : P.pvar list =
  match length with P.PE e -> expr_vars found e

and type_vars (found : P.pvar list) (ty : P.pty) : P.pvar list =
  match ty with
  | P.Bty _ -> found
  | P.Arr (_, length) -> length_vars found length

