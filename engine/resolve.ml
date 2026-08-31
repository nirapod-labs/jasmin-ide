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

let empty_frame =
  { types = Names.empty; funs = Names.empty; vars = Names.empty }

type env = {
  frames : frame list; (* innermost first, the bottom frame last *)
  namespace : string list; (* outermost first, for the loaded-once key *)
  file : string;
  index : Index.t;
  dir : string;
  chain : string list;
}

type t = {
  entry : string;
  indexes : (string, Index.t) Hashtbl.t;
  answers : (string * int, answer) Hashtbl.t;
  order : Index.t list; (* entry first *)
  loaded : (string * string) list;
  snapshots : (string, (int * frame list) array) Hashtbl.t;
      (* The environment each file's walk met, by byte, in walk order. *)
}

type session = {
  idirs : (string * string) list;
  read : string -> string option;
  indexes : (string, Index.t) Hashtbl.t;
  answers : (string * int, answer) Hashtbl.t;
  snapshots : (string, (int * frame list) list) Hashtbl.t; (* reversed *)
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

(* The environment at a byte offset, for completion. The frames are immutable
   and shared, so a mark costs one cons per frame list. Marks are written in
   the order the walk meets them and at offsets that never decrease, which is
   what lets [scope_at] take the last mark at or before the offset it is
   asked about. *)
let mark session env (byte : int) =
  let byte = max 0 byte in
  let previous =
    Option.value ~default:[] (Hashtbl.find_opt session.snapshots env.file)
  in
  Hashtbl.replace session.snapshots env.file ((byte, env.frames) :: previous)

let mark_start session env (span : Jide_lang.Span.t) =
  mark session env span.Jide_lang.Span.start_byte;
  env

let mark_end session env (span : Jide_lang.Span.t) =
  mark session env span.Jide_lang.Span.end_byte;
  env

let push_var env declaration =
  let frame = current env in
  replace_current env
    {
      frame with
      vars = Names.add declaration.Index.name declaration frame.vars;
    }

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

(* A declaration that is in scope from where it is written: the mark is the
   end of the name, and the environment after it is what completion sees. *)
let push_marked session push env declaration =
  let env = push env declaration in
  mark_end session env declaration.Index.name_span

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
  if not (Hashtbl.mem session.answers key) then
    Hashtbl.add session.answers key answer

let index_of session file text =
  match Hashtbl.find_opt session.indexes file with
  | Some index -> index
  | None ->
      let index, _diagnostics = Index.of_text ~file text in
      Hashtbl.add session.indexes file index;
      session.order_rev <- index :: session.order_rev;
      index

let declaration_at env span =
  match
    Index.find_declaration env.index ~name_start:span.Jide_lang.Span.start_byte
  with
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

type use_role =
  | Use_call
  | Use_var
  | Use_type
  | Use_prim
  | Use_annot
  | Use_include

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
  | PEGet (_aligned, _access, _size, ident, index, length) -> (
      let env = ignored session env Use_var ident in
      let env = expression session env index in
      match length with
      | None -> env
      | Some length -> expression session env length)
  | PEFetch (_aligned, _size, address) -> expression session env address
  | PEpack (_size, elements) -> List.fold_left (expression session) env elements
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
  | PLArray (_aligned, _access, _size, ident, index, length) -> (
      let env = ignored session env Use_var ident in
      let env = expression session env index in
      match length with
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

and block session env (body : Syntax.pblock) =
  List.fold_left (instruction session) env (Location.unloc body)

(* A block scopes its declarations: the frame it built is dropped at its end,
   so the environment after it is the one before it, and that is what a mark
   at the block's end records. *)
and scoped_block session env (body : Syntax.pblock) =
  ignore (block session env body);
  mark_end session env (span_of_location (Location.loc body))

and instruction session env
    ((annotations_, (instr : Syntax.pinstr_r Location.located)) : Syntax.pinstr)
    =
  let item_span = span_of_location (Location.loc instr) in
  let env = mark_start session env item_span in
  let env = annotations session env annotations_ in
  let env =
    match Location.unloc instr with
    | Syntax.PIArrayInit ident -> ignored session env Use_var ident
    | PIAssign (targets, _operator, value, condition) -> (
        let env = lvalues session env targets in
        let env = expression session env value in
        match condition with
        | None -> env
        | Some condition -> expression session env condition)
    | PIAssert (_message, condition) -> expression session env condition
    | PIIf (condition, when_true, when_false) ->
        let env = expression session env condition in
        ignore (scoped_block session env when_true);
        (match when_false with
        | None -> ()
        | Some when_false -> ignore (scoped_block session env when_false));
        env
    | PIFor (ident, (_direction, first, last), body) ->
        let env = ignored session env Use_var ident in
        let env = expression session env first in
        let env = expression session env last in
        ignore (scoped_block session env body);
        env
    | PIWhile (before, condition, after) ->
        (match before with
        | None -> ()
        | Some before -> ignore (scoped_block session env before));
        let env = expression session env condition in
        (match after with
        | None -> ()
        | Some after -> ignore (scoped_block session env after));
        env
    | PIdecl (stotype, names) ->
        List.fold_left
          (fun env name ->
            let env = ptype session env (snd stotype) in
            let declaration =
              declare session env Index.Variable name ~item_span
            in
            push_marked session push_var env declaration)
          env names
    | PIdeclinit (stotype, inits) ->
        List.fold_left
          (fun env
               (init :
                 (string Location.located * Syntax.pexpr) Location.located) ->
            let name, value = Location.unloc init in
            let before = env in
            let env = ptype session env (snd stotype) in
            let declaration =
              declare session env Index.Variable name ~item_span
            in
            let env = push_marked session push_var env declaration in
            (* The initialising expression is typed before the name is in
               scope, as [tt_annot_decl] does. *)
            ignore (expression session before value);
            env)
          env inits
  in
  mark_end session env item_span

(* ------------------------------------------------------------------ *)
(* Items *)

let resolve_path ~idirs ~dir ~from written =
  let base =
    match from with
    | None -> dir
    | Some name -> (
        match List.assoc_opt name idirs with
        | Some directory -> directory
        | None -> dir)
  in
  let path =
    if Filename.is_relative written then Filename.concat base written
    else written
  in
  normalise path

let max_chain = 64

let rec item session env (located : Syntax.pitem Location.located) : env =
  let item_span = span_of_location (Location.loc located) in
  let env = mark_start session env item_span in
  let env =
    match Location.unloc located with
    | Syntax.PParam pp ->
        let env = ptype session env pp.ppa_ty in
        let env = expression session env pp.ppa_init in
        let declaration =
          declare session env Index.Parameter pp.ppa_name ~item_span
        in
        push_var env declaration
    | PGlobal global ->
        let env = annotations session env global.pgd_annot in
        let env = ptype session env global.pgd_type in
        let env =
          match global.pgd_val with
          | Syntax.GEexpr value -> expression session env value
          | GEarray values -> List.fold_left (expression session) env values
        in
        let declaration =
          declare session env Index.Global global.pgd_name ~item_span
        in
        push_var env declaration
    | Pexec exec ->
        ignore (use session env Use_call exec.pex_name);
        env
    | PTypeAlias (name, annotations_, ty) ->
        let env = annotations session env annotations_ in
        let env = ptype session env ty in
        let declaration =
          declare session env Index.Type_alias name ~item_span
        in
        push_type env declaration
    | Prequire (from, files) ->
        let env =
          match from with
          | None -> env
          | Some ident -> ignored session env Use_include ident
        in
        let from = Option.map Location.unloc from in
        List.fold_left
          (fun env (file : string Location.located) ->
            require_file session env ~from (Location.unloc file))
          env files
    | PNamespace (name, items) ->
        let namespace = Location.unloc name in
        ignore (declare session env Index.Namespace name ~item_span);
        let inner = push_namespace env namespace in
        let inner = mark_end session inner (span_of_ident name) in
        let resolved = List.fold_left (item session) inner items in
        exit_namespace resolved
    | PFundef function_ ->
        let env = annotations session env function_.pdf_annot in
        let local = { env with frames = empty_frame :: env.frames } in
        let local = mark_end session local (span_of_ident function_.pdf_name) in
        let local =
          List.fold_left
            (fun env
                 ((annotations_, (stotype, names)) :
                   Syntax.pannotations * Syntax.paramdecls) ->
              let env = annotations session env annotations_ in
              let env = ptype session env (snd stotype) in
              List.fold_left
                (fun env name ->
                  let declaration =
                    declare session env Index.Parameter name ~item_span
                  in
                  push_marked session push_var env declaration)
                env names)
            local function_.pdf_args
        in
        let local =
          match function_.pdf_rty with
          | None -> local
          | Some returns ->
              List.fold_left
                (fun env (annotations_, stotype) ->
                  let env = annotations session env annotations_ in
                  ptype session env (snd stotype))
                local returns
        in
        let final =
          List.fold_left (instruction session) local
            function_.pdf_body.pdb_instr
        in
        (match Location.unloc function_.pdf_body.pdb_ret with
        | None -> ()
        | Some returns ->
            List.iter
              (fun ident -> ignore (use session final Use_var ident))
              returns);
        (* The function is declared after its body: pretyping pushes it into
           the environment from before the parameters, so a body cannot call
           itself. *)
        let declaration =
          declare session env Index.Function function_.pdf_name ~item_span
        in
        push_fun env declaration
  in
  mark_end session env item_span

and require_file session env ~from written =
  let path = resolve_path ~idirs:session.idirs ~dir:env.dir ~from written in
  let namespace_key = String.concat "::" env.namespace in
  if List.mem (namespace_key, path) session.loaded then env
  else if List.length env.chain >= max_chain then env
  else (
    session.loaded <- (namespace_key, path) :: session.loaded;
    match session.read path with
    | None -> env
    | Some text ->
        let index = index_of session path text in
        let inner =
          {
            env with
            file = path;
            index;
            dir = Filename.dirname path;
            chain = path :: env.chain;
          }
        in
        (* A file read into a namespace holds that namespace's names: the
           marks of this visit replace the ones an earlier visit left, so the
           last visit is the one a scope question answers from. *)
        Hashtbl.replace session.snapshots path [];
        let resolved = List.fold_left (item session) inner index.Index.items in
        (* The frames the included file built stay; the file it came from is
           back for everything after the require. *)
        {
          resolved with
          file = env.file;
          index = env.index;
          dir = env.dir;
          chain = env.chain;
        })

(* ------------------------------------------------------------------ *)

let of_text ~idirs ~read ~file text : t =
  let session =
    {
      idirs;
      read;
      indexes = Hashtbl.create 16;
      answers = Hashtbl.create 256;
      snapshots = Hashtbl.create 16;
      order_rev = [];
      loaded = [];
    }
  in
  let file = normalise file in
  let index = index_of session file text in
  (* The document itself is loaded in its own namespace, as the adapter's
     [enter_file] does before it types the first item. *)
  session.loaded <- [ ("", file) ];
  let env =
    {
      frames = [ empty_frame ];
      namespace = [];
      file;
      index;
      dir = Filename.dirname file;
      chain = [ file ];
    }
  in
  Hashtbl.replace session.snapshots file [];
  mark session env 0;
  ignore (List.fold_left (item session) env index.Index.items);
  {
    entry = file;
    indexes = session.indexes;
    answers = session.answers;
    order = List.rev session.order_rev;
    loaded = List.rev session.loaded;
    snapshots =
      Hashtbl.fold
        (fun file marks snapshots ->
          Hashtbl.add snapshots file (Array.of_list (List.rev marks));
          snapshots)
        session.snapshots
        (Hashtbl.create (Hashtbl.length session.snapshots));
  }

let of_file ~idirs ~read file =
  match read (normalise file) with
  | None -> None
  | Some text -> Some (of_text ~idirs ~read ~file:(normalise file) text)

let entry (t : t) = t.entry
let files (t : t) = t.order
let file (t : t) path = Hashtbl.find_opt t.indexes (normalise path)

let answer_at (t : t) ~file ~byte =
  let file = normalise file in
  match Hashtbl.find_opt t.indexes file with
  | None -> None
  | Some index -> (
      match Index.find_occurrence index ~byte with
      | None -> None
      | Some occurrence -> (
          let key = (file, occurrence.Index.span.Jide_lang.Span.start_byte) in
          match Hashtbl.find_opt t.answers key with
          | Some answer -> Some (occurrence, answer)
          | None -> Some (occurrence, Unresolved)))

let answers (t : t) path =
  let file = normalise path in
  match Hashtbl.find_opt t.indexes file with
  | None -> []
  | Some index ->
      List.map
        (fun occurrence ->
          let key = (file, occurrence.Index.span.Jide_lang.Span.start_byte) in
          let answer =
            match Hashtbl.find_opt t.answers key with
            | Some answer -> answer
            | None -> Unresolved
          in
          (occurrence, answer))
        index.Index.occurrences

let loaded (t : t) = t.loaded

let declaration_of_answer = function
  | Resolved declaration -> Some declaration
  | Intrinsic | Annotation | Include_path | Unresolved -> None

(* ------------------------------------------------------------------ *)
(* The scope at a byte *)

type binding = { name : string; declaration : Index.declaration }
type scope = { types : binding list; funs : binding list; vars : binding list }

(* The declarations one name space holds at a set of frames, innermost frame
   first and names in order within a frame. A name declared twice is listed
   once, with the declaration the lookup would find. The name is the frame's
   own key, so a name a namespace merged into its parent is listed as the
   qualified name a lookup there has to write. *)
let visible (project : frame -> Index.declaration Names.t) frames =
  let seen = Hashtbl.create 16 in
  let rec loop acc = function
    | [] -> List.rev acc
    | frame :: rest ->
        let acc =
          Names.fold
            (fun name declaration acc ->
              if Hashtbl.mem seen name then acc
              else (
                Hashtbl.add seen name ();
                { name; declaration } :: acc))
            (project frame) acc
        in
        loop acc rest
  in
  loop [] frames

let scope_at (t : t) ~file ~byte =
  match Hashtbl.find_opt t.snapshots (normalise file) with
  | None -> None
  | Some marks ->
      let frames = ref [] in
      Array.iter
        (fun (at, snapshot) -> if at <= byte then frames := snapshot)
        marks;
      let frames = !frames in
      Some
        {
          types = visible (fun frame -> frame.types) frames;
          funs = visible (fun frame -> frame.funs) frames;
          vars = visible (fun frame -> frame.vars) frames;
        }
