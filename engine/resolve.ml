(* The resolver: pretyping's naming rules over the untyped tree.

   The environment mirrors the library's: a stack of frames, one per
   namespace, each with the three name spaces pretyping keeps (types,
   functions, variables). A function body adds one more frame on top for its
   parameters and locals, and drops it at the end, which is how a block
   scopes a declaration without the parser recording block ranges: the
   threads of the environment are the blocks. A namespace merges its
   contents into its parent when it ends, qualified with its own name, and a
   lookup tries each frame from the innermost outward with the whole
   qualified name, exactly as [Pretyping.Env] does.

   Everything is immutable except the answer table: a declaration is
   (file, name byte range, item byte range), and the compiler's unique ids
   never enter the result. *)

open Jasmin

module Names = Map.Make (String)

type answer =
  | Resolved of Index.declaration
  | Intrinsic
  | Annotation
  | Include_path
  | Unresolved

type frame = {
  types : Index.declaration Names.t;
  funs : Index.declaration Names.t;
  vars : Index.declaration Names.t;
}

let empty_frame = { types = Names.empty; funs = Names.empty; vars = Names.empty }

type env = {
  frames : frame list;  (* innermost first, the bottom frame last *)
  namespace : string list;  (* outermost first, for the loaded-once key *)
  file : string;
  index : Index.t;
  dir : string;
  chain : string list;
}

type t = {
  entry : string;
  indexes : (string, Index.t) Hashtbl.t;
  answers : (string * int, answer) Hashtbl.t;
  order : Index.t list;  (* entry first *)
  loaded : (string * string) list;
}

type session = {
  idirs : (string * string) list;
  read : string -> string option;
  indexes : (string, Index.t) Hashtbl.t;
  answers : (string * int, answer) Hashtbl.t;
  mutable order_rev : Index.t list;
  mutable loaded : (string * string) list;
}

let normalise = Index.normalise

let span_of_ident (ident : string Location.located) =
  let location = Location.loc ident in
  let start = max 0 location.Location.loc_bchar in
  let stop = max start location.Location.loc_echar in
  Jide_lang.Span.make ~start_byte:start ~end_byte:stop

let span_of_location (location : Location.t) =
  let start = max 0 location.Location.loc_bchar in
  let stop = max start location.Location.loc_echar in
  Jide_lang.Span.make ~start_byte:start ~end_byte:stop

(* ------------------------------------------------------------------ *)
(* Environment *)

let current env =
  match env.frames with frame :: _ -> frame | [] -> empty_frame

let replace_current env frame =
  match env.frames with
  | _ :: rest -> { env with frames = frame :: rest }
  | [] -> { env with frames = [ frame ] }

let find (project : frame -> Index.declaration Names.t) name env =
  let rec loop = function
    | [] -> None
    | frame :: rest -> (
        match Names.find_opt name (project frame) with
        | Some declaration -> Some declaration
        | None -> loop rest)
  in
  loop env.frames

let find_type env name = find (fun frame -> frame.types) name env
let find_fun env name = find (fun frame -> frame.funs) name env
let find_var env name = find (fun frame -> frame.vars) name env

let push_var env declaration =
  let frame = current env in
  replace_current env
    { frame with vars = Names.add declaration.Index.name declaration frame.vars }

let push_fun env declaration =
  (* The library refuses a second function of a name anywhere in scope and
     keeps the first, so a later call still resolves to it. *)
  match find_fun env declaration.Index.name with
  | Some _ -> env
  | None ->
      let frame = current env in
      replace_current env
        {
          frame with
          funs = Names.add declaration.Index.name declaration frame.funs;
        }

let push_type env declaration =
  match find_type env declaration.Index.name with
  | Some _ -> env
  | None ->
      let frame = current env in
      replace_current env
        {
          frame with
          types = Names.add declaration.Index.name declaration frame.types;
        }

let merge_frame prefix frame parent =
  let merge prefix_map parent_map =
    Names.fold
      (fun key declaration map ->
        Names.add (prefix ^ "::" ^ key) declaration map)
      prefix_map parent_map
  in
  {
    types = merge frame.types parent.types;
    funs = merge frame.funs parent.funs;
    vars = merge frame.vars parent.vars;
  }

let push_namespace env name =
  {
    env with
    frames = empty_frame :: env.frames;
    namespace = env.namespace @ [ name ];
  }

let exit_namespace env =
  match (env.frames, List.rev env.namespace) with
  | frame :: parent :: rest, name :: outer ->
      {
        env with
        frames = merge_frame name frame parent :: rest;
        namespace = List.rev outer;
      }
  | _ -> env

(* ------------------------------------------------------------------ *)
(* Answers *)

let record session file span answer =
  let key = (file, span.Jide_lang.Span.start_byte) in
  if not (Hashtbl.mem session.answers key) then Hashtbl.add session.answers key answer

let index_of session file text =
  match Hashtbl.find_opt session.indexes file with
  | Some index -> index
  | None ->
      let index, _diagnostics = Index.of_text ~file text in
      Hashtbl.add session.indexes file index;
      session.order_rev <- index :: session.order_rev;
      index

let declaration_at env span =
  match Index.find_declaration env.index ~name_start:span.Jide_lang.Span.start_byte with
  | Some declaration -> declaration
  | None ->
      (* The walk marks every declaration it meets; this arm only keeps a
         range bug from raising. *)
      {
        Index.kind = Index.Unknown;
        name = "";
        file = env.file;
        name_span = span;
        item_span = span;
        detail = None;
      }

let declare session env (kind : Index.kind) ident ~item_span =
  let span = span_of_ident ident in
  let declaration = declaration_at env span in
  let declaration =
    if declaration.Index.kind = Index.Unknown then
      { declaration with Index.kind; name = Location.unloc ident; item_span }
    else declaration
  in
  record session env.file span (Resolved declaration);
  declaration

(* ------------------------------------------------------------------ *)
(* Uses *)

type use_role = Use_call | Use_var | Use_type | Use_prim | Use_annot | Use_include

let use session env (role : use_role) (ident : string Location.located) =
  let name = Location.unloc ident in
  let answer =
    match role with
    | Use_call -> (
        match find_fun env name with
        | Some declaration -> Resolved declaration
        | None -> Unresolved)
    | Use_var -> (
        match find_var env name with
        | Some declaration -> Resolved declaration
        | None -> Unresolved)
    | Use_type -> (
        match find_type env name with
        | Some declaration -> Resolved declaration
        | None -> Unresolved)
    | Use_prim -> Intrinsic
    | Use_annot -> Annotation
    | Use_include -> Include_path
  in
  record session env.file (span_of_ident ident) answer

let ignored session env (role : use_role) (ident : string Location.located) =
  use session env role ident;
  env

(* ------------------------------------------------------------------ *)

let rec expression session env (pe : Syntax.pexpr) =
  match Location.unloc pe with
  | Syntax.PEParens inner -> expression session env inner
  | PEVar ident -> ignored session env Use_var ident
  | PEGet (_aligned, _access, _size, ident, index, length) ->
      let env = ignored session env Use_var ident in
      let env = expression session env index in
      (match length with
      | None -> env
      | Some length -> expression session env length)
  | PEFetch (_aligned, _size, address) -> expression session env address
  | PEpack (_size, elements) ->
      List.fold_left (expression session) env elements
  | PEstring _ | PEBool _ | PEInt _ -> env
  | PECall (callee, arguments) ->
      let env = ignored session env Use_call callee in
      List.fold_left (expression session) env arguments
  | PECombF (callee, arguments) ->
      let env = ignored session env Use_call callee in
      List.fold_left (expression session) env arguments
  | PEPrim (intrinsic, arguments) ->
      let env = ignored session env Use_prim intrinsic in
      List.fold_left (expression session) env arguments
  | PEOp1 (_operation, operand) -> expression session env operand
  | PEOp2 (_operation, (left, right)) ->
      expression session (expression session env left) right
  | PEIf (condition, when_true, when_false) ->
      let env = expression session env condition in
      let env = expression session env when_true in
      expression session env when_false

and ptype session env (ty : Syntax.ptype) =
  match Location.unloc ty with
  | Syntax.TBool | TInt | TWord _ -> env
  | TArray (size, length) ->
      let env = sizetype session env size in
      expression session env length
  | TAlias ident -> ignored session env Use_type ident

and sizetype session env (size : Syntax.psizetype) =
  match size with
  | Syntax.TypeWsize _ -> env
  | TypeSizeAlias ident -> ignored session env Use_type ident

and annotations session env (labels : Syntax.pannotations) =
  List.fold_left
    (fun env ((label : string Location.located), attribute) ->
      let env = ignored session env Use_annot label in
      match attribute with
      | None -> env
      | Some attribute -> attribute_value session env attribute)
    env labels

and attribute_value session env (attribute : Syntax.pattribute) =
  match Location.unloc attribute with
  | Syntax.PAstring _ | PAws _ -> env
  | PAstruct inner -> annotations session env inner
  | PAexpr pe -> expression session env pe

and lvalue session env (lv : Syntax.plvalue) =
  match Location.unloc lv with
  | Syntax.PLIgnore -> env
  | PLVar ident -> ignored session env Use_var ident
  | PLArray (_aligned, _access, _size, ident, index, length) ->
      let env = ignored session env Use_var ident in
      let env = expression session env index in
      (match length with
      | None -> env
      | Some length -> expression session env length)
  | PLMem (_aligned, _size, address) -> expression session env address

and lvalues session env ((annotation, values) : Syntax.plvals) =
  let env =
    match annotation with
    | None -> env
    | Some located -> annotations session env (Location.unloc located)
  in
  List.fold_left (lvalue session) env values

and block session env (block : Syntax.pblock) =
  List.fold_left (instruction session) env (Location.unloc block)

