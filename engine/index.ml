(* The index of one untyped tree: identifiers, declarations and requires.

   Section 9 of the design asks for the syntax tree the compiler discards.
   The walk here is deliberately syntax-shaped: it visits every [pident] the
   grammar has, in source order, and never asks what a name means. The
   resolver does that, over the same tree, with pretyping's rules. *)

open Jasmin

type kind =
  | Function
  | Parameter
  | Variable
  | Global
  | Namespace
  | Type_alias
  | Exec
  | Require
  | Intrinsic
  | Annotation
  | Include
  | Unknown

type role =
  | Call
  | Var
  | Type
  | Prim
  | Annot
  | Include_path
  | Declared of kind

type occurrence = {
  span : Jide_lang.Span.t;
  last : Jide_lang.Span.t;
  name : string;
  role : role;
  value : string option;
}

type declaration = {
  kind : kind;
  name : string;
  file : string;
  name_span : Jide_lang.Span.t;
  item_span : Jide_lang.Span.t;
  detail : string option;
}

type item = {
  item_kind : kind;
  item_name : string option;
  item_name_span : Jide_lang.Span.t option;
  item_span : Jide_lang.Span.t;
  item_children : item list;
}

type require = {
  from : (string * Jide_lang.Span.t) option;
  written : string;
  written_span : Jide_lang.Span.t;
  item_span : Jide_lang.Span.t;
}

type t = {
  file : string;
  text : string;
  items : Syntax.pprogram;
  outline : item list;
  declarations : declaration list;
  occurrences : occurrence list;
  requires : require list;
}

(* ------------------------------------------------------------------ *)
(* Ranges *)

let span_of_location (location : Location.t) : Jide_lang.Span.t =
  let start = max 0 location.Location.loc_bchar in
  let stop = max start location.Location.loc_echar in
  Jide_lang.Span.make ~start_byte:start ~end_byte:stop

let span_of_ident (ident : string Location.located) =
  span_of_location (Location.loc ident)

let text_of_span (text : string) (span : Jide_lang.Span.t) =
  let length = String.length text in
  let start = max 0 (min span.Jide_lang.Span.start_byte length) in
  let stop = max start (min span.Jide_lang.Span.end_byte length) in
  String.sub text start (stop - start)

let collapse (text : string) =
  let buffer = Buffer.create (min (String.length text) 256) in
  let pending = ref false in
  String.iter
    (fun character ->
      match character with
      | ' ' | '\t' | '\r' | '\n' ->
          if Buffer.length buffer > 0 then pending := true
      | _ ->
          if !pending then Buffer.add_char buffer ' ';
          pending := false;
          Buffer.add_char buffer character)
    text;
  let text = Buffer.contents buffer in
  if String.length text > 240 then String.sub text 0 237 ^ "..." else text

(* One spelling per path, as Context writes it. Index does not depend on
   Context (Context is about files, this module about a tree), so the small
   fold is repeated here and the two are tested to agree. *)
let normalise (path : string) =
  let absolute = String.length path > 0 && path.[0] = '/' in
  let parts = String.split_on_char '/' path in
  let rec fold stack = function
    | [] -> List.rev stack
    | ("" | ".") :: rest -> fold stack rest
    | ".." :: rest -> (
        match stack with [] -> fold stack rest | _ :: stack -> fold stack rest)
    | part :: rest -> fold (part :: stack) rest
  in
  let body = String.concat "/" (fold [] parts) in
  if absolute then "/" ^ body else if body = "" then "." else body

(* [A::B::x] has one range in the compiler's location. The last segment is
   read from the source bytes: it ends where the identifier ends and starts
   one segment length before it, and only when the two bytes before it are
   [::]. A space around [::] gives the whole range, as the design says. *)
let last_segment (text : string) (span : Jide_lang.Span.t) (name : string) =
  let name_length = String.length name in
  let rec find index =
    if index < 0 then None
    else if
      index + 1 < name_length && name.[index] = ':' && name.[index + 1] = ':'
    then Some index
    else find (index - 1)
  in
  match find (name_length - 2) with
  | None -> span
  | Some index ->
      let segment = name_length - index - 2 in
      let start = span.Jide_lang.Span.end_byte - segment in
      let preceding = start - 2 in
      if
        start >= span.Jide_lang.Span.start_byte
        && preceding >= span.Jide_lang.Span.start_byte
        && preceding + 1 < String.length text
        && text.[preceding] = ':' && text.[preceding + 1] = ':'
      then Jide_lang.Span.make ~start_byte:start ~end_byte:span.Jide_lang.Span.end_byte
      else span

(* ------------------------------------------------------------------ *)
(* Storage and types as text, for hover *)

let writable = function
  | None -> ""
  | Some `Constant -> " const"
  | Some `Writable -> " mut"

let pointer_text = function
  | `Direct -> ""
  | `Pointer mode -> writable mode ^ " ptr"

let storage_text (storage : Syntax.pstorage) =
  match storage with
  | `Reg pointer -> "reg" ^ pointer_text pointer
  | `Stack pointer -> "stack" ^ pointer_text pointer
  | `Inline -> "inline"
  | `Global -> "global"

(* ------------------------------------------------------------------ *)
(* The walk *)

type state = {
  file : string;
  text : string;
  occurrences : occurrence list ref;
  declarations : declaration list ref;
  requires : require list ref;
}

let add_occurrence state occurrence =
  state.occurrences := occurrence :: !(state.occurrences)

let occurrence_of_ident state ~(role : role) ~value
    (ident : string Location.located) =
  let span = span_of_ident ident in
  let name = Location.unloc ident in
  {
    span;
    last = last_segment state.text span name;
    name;
    role;
    value;
  }

let use_ident state role ident =
  add_occurrence state (occurrence_of_ident state ~role ~value:None ident)

let add_declaration state ~(kind : kind) ?detail
    (ident : string Location.located) ~item_span =
  let name_span = span_of_ident ident in
  let name = Location.unloc ident in
  let declaration =
    { kind; name; file = state.file; name_span; item_span; detail }
  in
  state.declarations := declaration :: !(state.declarations);
  add_occurrence state
    {
      span = name_span;
      last = name_span;
      name;
      role = Declared kind;
      value = None;
    };
  declaration

let type_text state (ty : Syntax.ptype) =
  collapse (text_of_span state.text (span_of_location (Location.loc ty)))

let stotype_text state (storage, ty : Syntax.pstotype) =
  collapse (storage_text storage ^ " " ^ type_text state ty)

(* The parts of a function's signature, rendered from its own type ranges.
   The typed program would give a real [fun_sig]; until that is exposed the
   syntax holds the same words, and hover shows them as the user wrote
   them. *)
let signature_text state (pf : Syntax.pfundef) =
  let argument (_, (stotype, names)) =
    stotype_text state stotype
    ^ " " ^ String.concat ", " (List.map Location.unloc names)
  in
  let arguments = List.map argument pf.pdf_args in
  let returns =
    match pf.pdf_rty with
    | None -> ""
    | Some returns ->
        let one (_, stotype) = stotype_text state stotype in
        " -> " ^ String.concat ", " (List.map one returns)
  in
  collapse
    (Printf.sprintf "fn %s(%s)%s" (Location.unloc pf.pdf_name)
       (String.concat ", " arguments) returns)

let rec expression state (pe : Syntax.pexpr) =
  match Location.unloc pe with
  | Syntax.PEParens inner -> expression state inner
  | PEVar ident -> use_ident state Var ident
  | PEGet (_aligned, _access, _size, ident, index, length) ->
      use_ident state Var ident;
      expression state index;
      Option.iter (expression state) length
  | PEFetch (_aligned, _size, address) -> expression state address
  | PEpack (_size, elements) -> List.iter (expression state) elements
  | PEstring _ | PEBool _ | PEInt _ -> ()
  | PECall (callee, arguments) ->
      use_ident state Call callee;
      List.iter (expression state) arguments
  | PECombF (callee, arguments) ->
      use_ident state Call callee;
      List.iter (expression state) arguments
  | PEPrim (intrinsic, arguments) ->
      use_ident state Prim intrinsic;
      List.iter (expression state) arguments
  | PEOp1 (_operation, operand) -> expression state operand
  | PEOp2 (_operation, (left, right)) ->
      expression state left;
      expression state right
  | PEIf (condition, when_true, when_false) ->
      expression state condition;
      expression state when_true;
      expression state when_false

and ptype state (ty : Syntax.ptype) =
  match Location.unloc ty with
  | Syntax.TBool | TInt | TWord _ -> ()
  | TArray (size, length) ->
      sizetype state size;
      expression state length
  | TAlias ident -> use_ident state Type ident

and sizetype state (size : Syntax.psizetype) =
  match size with
  | Syntax.TypeWsize _ -> ()
  | TypeSizeAlias ident -> use_ident state Type ident

and annotations state (labels : Syntax.pannotations) =
  List.iter
    (fun ((label : string Location.located), attribute) ->
      let value =
        match attribute with
        | Some attribute -> (
            match Location.unloc attribute with
            | Syntax.PAstring value -> Some value
            | _ -> None)
        | None -> None
      in
      add_occurrence state (occurrence_of_ident state ~role:Annot ~value label);
      Option.iter (attribute_ state) attribute)
    labels

and attribute_ state (attribute : Syntax.pattribute) =
  match Location.unloc attribute with
  | Syntax.PAstring _ | PAws _ -> ()
  | PAstruct inner -> annotations state inner
  | PAexpr pe -> expression state pe

and lvalue state (lv : Syntax.plvalue) =
  match Location.unloc lv with
  | Syntax.PLIgnore -> ()
  | PLVar ident -> use_ident state Var ident
  | PLArray (_aligned, _access, _size, ident, index, length) ->
      use_ident state Var ident;
      expression state index;
      Option.iter (expression state) length
  | PLMem (_aligned, _size, address) -> expression state address

and lvalues state ((annotations_, values) : Syntax.plvals) =
  Option.iter
    (fun located -> annotations state (Location.unloc located))
    annotations_;
  List.iter (lvalue state) values

and instruction state
    ((annotations_, (instr : Syntax.pinstr_r Location.located)) :
      Syntax.pinstr) =
  annotations state annotations_;
  let item_span = span_of_location (Location.loc instr) in
  match Location.unloc instr with
  | Syntax.PIArrayInit ident -> use_ident state Var ident
  | PIAssign (targets, _operator, value, condition) ->
      lvalues state targets;
      expression state value;
      Option.iter (expression state) condition
  | PIAssert (_message, condition) -> expression state condition
  | PIIf (condition, when_true, when_false) ->
      expression state condition;
      block state when_true;
      Option.iter (block state) when_false
  | PIFor (ident, (_direction, first, last), body) ->
      use_ident state Var ident;
      expression state first;
      expression state last;
      block state body
  | PIWhile (before, condition, after) ->
      Option.iter (block state) before;
      expression state condition;
      Option.iter (block state) after
  | PIdecl (stotype, names) ->
      ptype state (snd stotype);
      let detail = stotype_text state stotype in
      List.iter
        (fun name ->
          ignore
            (add_declaration state ~kind:Variable ~detail name ~item_span))
        names
  | PIdeclinit (stotype, inits) ->
      ptype state (snd stotype);
      let detail = stotype_text state stotype in
      List.iter
        (fun (init : (string Location.located * Syntax.pexpr) Location.located)
           ->
          let name, value = Location.unloc init in
          ignore (add_declaration state ~kind:Variable ~detail name ~item_span);
          expression state value)
        inits

and block state (block : Syntax.pblock) =
  List.iter (instruction state) (Location.unloc block)

(* The declarations in one item, and its outline node. The two are made
   together because both read the same locations; the outline node carries
   the name range the protocol asks for and the declaration carries the item
   range. *)
let rec item state (located : Syntax.pitem Location.located) : item =
  let item_span = span_of_location (Location.loc located) in
  match Location.unloc located with
  | Syntax.PParam pp ->
      ptype state pp.ppa_ty;
      expression state pp.ppa_init;
      let detail = collapse ("param " ^ type_text state pp.ppa_ty) in
      let declaration =
        add_declaration state ~kind:Parameter ~detail pp.ppa_name ~item_span
      in
      {
        item_kind = Parameter;
        item_name = Some declaration.name;
        item_name_span = Some declaration.name_span;
        item_span;
        item_children = [];
      }
  | PGlobal global ->
      annotations state global.pgd_annot;
      ptype state global.pgd_type;
      (match global.pgd_val with
      | Syntax.GEexpr value -> expression state value
      | GEarray elements -> List.iter (expression state) elements);
      let detail =
        collapse ("global " ^ type_text state global.pgd_type)
      in
      let declaration =
        add_declaration state ~kind:Global ~detail global.pgd_name ~item_span
      in
      {
        item_kind = Global;
        item_name = Some declaration.name;
        item_name_span = Some declaration.name_span;
        item_span;
        item_children = [];
      }
  | Pexec exec ->
      use_ident state Call exec.pex_name;
      {
        item_kind = Exec;
        item_name = Some (Location.unloc exec.pex_name);
        item_name_span = Some (span_of_ident exec.pex_name);
        item_span;
        item_children = [];
      }
  | PTypeAlias (name, annotations_, ty) ->
      annotations state annotations_;
      ptype state ty;
      let detail =
        collapse
          (Printf.sprintf "type %s = %s" (Location.unloc name)
             (type_text state ty))
      in
      let declaration =
        add_declaration state ~kind:Type_alias ~detail name ~item_span
      in
      {
        item_kind = Type_alias;
        item_name = Some declaration.name;
        item_name_span = Some declaration.name_span;
        item_span;
        item_children = [];
      }
  | Prequire (from, files) ->
      Option.iter (use_ident state Include_path) from;
      let children =
        List.concat_map
          (fun (file : string Location.located) ->
            let written = Location.unloc file in
            let written_span = span_of_ident file in
            state.requires :=
              {
                from =
                  Option.map
                    (fun ident ->
                      (Location.unloc ident, span_of_ident ident))
                    from;
                written;
                written_span;
                item_span;
              }
              :: !(state.requires);
            [
              {
                item_kind = Require;
                item_name = Some written;
                item_name_span = Some written_span;
                item_span;
                item_children = [];
              };
            ])
          files
      in
      {
        item_kind = Require;
        item_name = None;
        item_name_span = None;
        item_span;
        item_children = children;
      }
  | PNamespace (name, inner) ->
      let declaration =
        add_declaration state ~kind:Namespace name ~item_span
      in
      {
        item_kind = Namespace;
        item_name = Some declaration.name;
        item_name_span = Some declaration.name_span;
        item_span;
        item_children = List.map (item state) inner;
      }
  | PFundef function_ ->
      let detail = signature_text state function_ in
      annotations state function_.pdf_annot;
      List.iter
        (fun ((annotations_, (stotype, names)) :
               Syntax.pannotations * Syntax.paramdecls) ->
          annotations state annotations_;
          ptype state (snd stotype);
          List.iter
            (fun name ->
              ignore
                (add_declaration state ~kind:Parameter
                   ~detail:(stotype_text state stotype)
                   name ~item_span))
            names)
        function_.pdf_args;
      Option.iter
        (List.iter (fun (annotations_, (storage, ty)) ->
             annotations state annotations_;
             ptype state (snd (storage, ty))))
        function_.pdf_rty;
      List.iter (instruction state) function_.pdf_body.pdb_instr;
      (match Location.unloc function_.pdf_body.pdb_ret with
      | None -> ()
      | Some returns -> List.iter (use_ident state Var) returns);
      let declaration =
        add_declaration state ~kind:Function ~detail function_.pdf_name
          ~item_span
      in
      {
        item_kind = Function;
        item_name = Some declaration.name;
        item_name_span = Some declaration.name_span;
        item_span;
        item_children = [];
      }

(* ------------------------------------------------------------------ *)
(* Ordering and identity *)

