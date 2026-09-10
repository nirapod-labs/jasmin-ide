(* Navigation and hover over the snapshot, and the seam the controller calls.

   Section 9 of the design keeps a syntax index and a resolver beside the
   compiler's typed program, and section 10 answers definition, references,
   the outline and hover from them. This module is the reading side of that:
   the snapshot is built once, and each question is a lookup in it.

   Nothing here parses, resolves or types. The one thing a caller brings is
   the typed facts of a checked file, which win over the index where the
   compiler has an answer.

   Two interfaces live here. They differ in their coordinates, not in what
   they answer, and the second is written over the first:

   - the engine's own, in bytes of a named file: {!definition_at},
     {!references_at}, {!hover_at} and {!outline}, over a {!t} a caller built
     from a text it chose;
   - the controller's, in the protocol's positions: {!definition},
     {!references}, {!hover} and {!symbols}. Each takes the engine and a
     [uri], reads the document the client holds, converts the position to a
     byte with that document's line index, asks the byte-level question, and
     converts the answer's spans back into ranges.

   The controller reads only the second, so what the engine can answer
   changes without a controller file changing. *)

(* The answers the protocol asks for. A position is a zero-based line and a
   UTF-16 character; a range is two of them. The conversion from the byte
   spans the resolver works in happens below, in one place, because only the
   engine holds the text of every file an answer can name: a location inside a
   required file cannot be turned into a range by a layer that never read that
   file. *)
type range = { start_ : Jide_lang.Position.t; end_ : Jide_lang.Position.t }
type location = { uri : string; range : range }
type hover = { range : range option; contents : Types.markdown }

type symbol = {
  name : string;
  detail : string option;
  kind : Types.symbol_kind;
  range : range;
  selection_range : range;
  children : symbol list;
}

type t = {
  resolve : Resolve.t;
  facts : (string, Jide_jasmin.Facts.t) Hashtbl.t;
      (* keyed by [Index.normalise], like every path here *)
}

(* ------------------------------------------------------------------ *)
(* Building a snapshot *)

let of_resolve ?(facts = []) (resolve : Resolve.t) =
  let table = Hashtbl.create 8 in
  List.iter
    (fun (file, facts) -> Hashtbl.replace table (Index.normalise file) facts)
    facts;
  { resolve; facts = table }

let of_text ?facts ~idirs ~read ~file text =
  of_resolve ?facts (Resolve.of_text ~idirs ~read ~file text)

let of_file ?facts ~idirs ~read file =
  match Resolve.of_file ~idirs ~read file with
  | None -> None
  | Some resolve -> Some (of_resolve ?facts resolve)

let with_facts t ~file facts =
  let table = Hashtbl.copy t.facts in
  Hashtbl.replace table (Index.normalise file) facts;
  { t with facts = table }

let resolve t = t.resolve
let entry t = Resolve.entry t.resolve
let files t = Resolve.files t.resolve
let file t path = Resolve.file t.resolve path
let facts t path = Hashtbl.find_opt t.facts (Index.normalise path)

(* ------------------------------------------------------------------ *)
(* The one question the position features share *)

(* The declaration the name at [byte] stands for. The resolver recorded an
   answer for every occurrence it walked, the declaration's own name
   included, so a cursor on a declaration gives that declaration and a
   cursor on a use gives what the use names. *)
let declaration_at t ~file ~byte =
  match Resolve.answer_at t.resolve ~file ~byte with
  | Some (_, Resolve.Resolved declaration) -> Some declaration
  | _ -> None

(* The byte-level answer about one name: the file the index holds and the
   span of the name in it. The file stays a path here; {!location_of} below is
   where the protocol's identifier is made from it. *)
let location (file : string) (span : Jide_lang.Span.t) : Types.location =
  { file; span }

(* ------------------------------------------------------------------ *)
(* Definition *)

(* The declaration writes its name inside its own file, so the answer is one
   range there: the file the declaration belongs to is the one a reader has
   to open, whether the name came through a namespace, a [require] or the
   entry itself. *)
let definition_at t ~file ~byte =
  match declaration_at t ~file ~byte with
  | None -> None
  | Some declaration ->
      Some (location declaration.Index.file declaration.Index.name_span)

(* ------------------------------------------------------------------ *)
(* References *)

(* Every file the resolution reached is scanned, because a declaration can be
   named from a file other than the one that declares it: from the entry, and
   from a required file that resolved with the entry's environment. The order
   is the resolution's own (entry first, then the order the files were first
   read) and, inside a file, the index's source order, so the answer is the
   same on every run. A place is answered once even if the index recorded it
   in two roles. *)
let references_at t ~file ~byte ~include_declaration =
  match declaration_at t ~file ~byte with
  | None -> []
  | Some target ->
      let seen = Hashtbl.create 16 in
      let fresh (index : Index.t) (span : Jide_lang.Span.t) =
        let key =
          ( index.file,
            span.Jide_lang.Span.start_byte,
            span.Jide_lang.Span.end_byte )
        in
        if Hashtbl.mem seen key then false
        else (
          Hashtbl.add seen key ();
          true)
      in
      List.concat_map
        (fun (index : Index.t) ->
          List.filter_map
            (fun ((occurrence : Index.occurrence), answer) ->
              match answer with
              | Resolve.Resolved declaration
                when Index.equal_declaration declaration target -> (
                  match occurrence.role with
                  | Index.Declared _ when not include_declaration -> None
                  | _ ->
                      if fresh index occurrence.span then
                        Some (location index.file occurrence.span)
                      else None)
              | _ -> None)
            (Resolve.answers t.resolve index.file))
        (Resolve.files t.resolve)

(* ------------------------------------------------------------------ *)
(* The outline *)

let symbol_kind (kind : Index.kind) : Types.symbol_kind =
  match kind with
  | Index.Function -> Types.Function
  | Parameter -> Parameter
  | Variable -> Variable
  | Global -> Global
  | Namespace -> Namespace
  | Type_alias -> Type_alias
  | Require -> Require
  | Exec -> Exec
  | Include -> Require
  | Intrinsic | Annotation | Unknown -> Unknown

(* A [require] item has no name of its own: its children are the files it
   names. The outline keeps the node, because that is the nesting the protocol
   shows, and calls it by the keyword that introduced it. *)
let require_name = "require"

(* The index already walked the tree into an outline, so the outline is read,
   not rebuilt: one node per item, its children below it, and the detail of a
   declaration looked up by the byte its name starts at. *)
let rec symbol_of_item (index : Index.t) (item : Index.item) : Types.symbol =
  let name_span =
    match item.item_name_span with Some span -> span | None -> item.item_span
  in
  let detail =
    match Index.find_declaration index ~name_start:name_span.start_byte with
    | Some declaration -> declaration.detail
    | None -> None
  in
  {
    Types.name = Option.value ~default:require_name item.item_name;
    detail;
    kind = symbol_kind item.item_kind;
    span = item.item_span;
    name_span;
    children = List.map (symbol_of_item index) item.item_children;
  }

let outline t ~file =
  match Resolve.file t.resolve file with
  | None -> []
  | Some index -> List.map (symbol_of_item index) index.Index.outline

(* ------------------------------------------------------------------ *)
(* Hover *)

(* An annotation's value is text a reader wrote, and hover is markdown: the
   value is shown only when every byte is printable, it is short enough to
   read, and it holds no backtick, which could end the fenced block it is
   shown in. Anything else is not displayed, and the label is shown alone. *)
let displayable (text : string) =
  String.length text <= 120
  && String.for_all
       (fun character ->
         let code = Char.code character in
         code >= 0x20 && code <> 0x7F && character <> '`')
       text

let annotation_line ~(key : string) (value : string option) =
  match value with
  | Some value when displayable value -> Printf.sprintf "#[%s=\"%s\"]" key value
  | _ -> Printf.sprintf "#[%s]" key

let annotation_lines (annotations : Jide_jasmin.Facts.annotation list) =
  List.map
    (fun (annotation : Jide_jasmin.Facts.annotation) ->
      annotation_line ~key:annotation.a_key annotation.a_value)
    annotations

let fence (lines : string list) : Types.markdown =
  { value = "```jasmin\n" ^ String.concat "\n" lines ^ "\n```" }

(* A function's signature as the compiler prints it: the argument types and
   the result types, without the parameters' names. *)
let function_line (name : string) (signature : Jide_jasmin.Facts.fun_sig) =
  let inputs = String.concat ", " signature.fs_in in
  let returns =
    match signature.fs_out with
    | [] -> ""
    | outputs -> " -> " ^ String.concat ", " outputs
  in
  Printf.sprintf "fn %s(%s)%s" name inputs returns

let variable_line (variable : Jide_jasmin.Facts.var) =
  Printf.sprintf "%s %s %s"
    (Jide_jasmin.Facts.string_of_var_kind variable.v_kind)
    variable.v_ty variable.v_name

(* A function the compiler typed: the item whose name starts where the
   declaration's does, with the signature it printed. *)
let function_at (facts : Jide_jasmin.Facts.t) (declaration : Index.declaration)
    =
  let name_start = declaration.name_span.Jide_lang.Span.start_byte in
  List.find_map
    (fun (item : Jide_jasmin.Facts.item) ->
      match (item.i_kind, item.i_sig) with
      | Jide_jasmin.Facts.Function, Some signature
        when item.i_name_span.Jide_lang.Span.start_byte = name_start ->
          Some (item, signature)
      | _ -> None)
    facts.Jide_jasmin.Facts.items

(* The variables of a file are the ones its items declare, a function's
   parameters and results included, so one lookup by the byte the name starts
   at finds a local, a parameter and a global alike. *)
let variable_of (facts : Jide_jasmin.Facts.t) (declaration : Index.declaration)
    =
  let name_start = declaration.name_span.Jide_lang.Span.start_byte in
  List.find_map
    (fun (item : Jide_jasmin.Facts.item) ->
      List.find_opt
        (fun (variable : Jide_jasmin.Facts.var) ->
          variable.v_dloc.Jide_lang.Span.start_byte = name_start)
        item.i_vars)
    facts.Jide_jasmin.Facts.items

(* What the typed facts say, when the compiler typed the declaration. *)
let lines_of_facts (facts : Jide_jasmin.Facts.t)
    (declaration : Index.declaration) =
  match function_at facts declaration with
  | Some (item, signature) ->
      Some (function_line item.i_name signature :: annotation_lines item.i_annot)
  | None -> (
      match variable_of facts declaration with
      | Some variable ->
          Some (variable_line variable :: annotation_lines variable.v_annot)
      | None -> None)

(* What the index says about the declaration's annotations. They were read
   off the walk, label and string together, so a name the compiler never
   typed still shows the `#[ct="..."]` it was written with. *)
let index_annotation_lines (declaration : Index.declaration) =
  List.map
    (fun (key, value) -> annotation_line ~key value)
    declaration.Index.annotations

(* What the index says: the signature or the type its walk cut from the
   source. The detail of a variable, a parameter or a global is the storage
   and the type without the name, so the name is put back on. *)
let line_of_index (declaration : Index.declaration) =
  let name = declaration.name in
  match declaration.detail with
  | Some detail when not (String.equal (String.trim detail) "") -> (
      match declaration.kind with
      | Index.Variable | Parameter | Global -> detail ^ " " ^ name
      | _ -> detail)
  | _ -> (
      match declaration.kind with
      | Index.Function -> "fn " ^ name
      | Index.Parameter -> "param " ^ name
      | Index.Global -> "global " ^ name
      | Index.Namespace -> "namespace " ^ name
      | Index.Type_alias -> "type " ^ name
      | Index.Exec -> "exec " ^ name
      | Index.Require -> "require " ^ name
      | _ -> name)

(* The lines hover shows for a declaration: its signature or type first, then
   its annotations.

   The compiler's answer wins where it exists, and it carries annotations of
   its own ([#[returnaddress=stack]] reaches [Facts] as [Some "stack"], and
   so does a quoted string). It does not carry every declaration: a local
   with no initialiser and no use leaves nothing behind, and neither does an
   item that failed to type. So the index is asked for a line when the
   compiler has none, and its annotations are taken whenever the facts gave
   none of their own, which is the case for a declaration the compiler never
   reached and for the statement annotations of a local. *)
let lines_of_declaration t (declaration : Index.declaration) =
  let from_facts =
    match Hashtbl.find_opt t.facts (Index.normalise declaration.file) with
    | None -> None
    | Some facts -> lines_of_facts facts declaration
  in
  let first, annotation_lines =
    match from_facts with
    | Some (signature_or_type :: annotations) -> (signature_or_type, annotations)
    | Some [] | None -> (line_of_index declaration, [])
  in
  let annotation_lines =
    match annotation_lines with
    | [] -> index_annotation_lines declaration
    | annotations -> annotations
  in
  first :: annotation_lines

(* What to say about the name at a byte: the declaration's signature or type
   and its annotations, when it is a declaration, and the name itself when it
   is a primitive, an annotation label or an include path. *)
let hover_at t ~file ~byte =
  match Resolve.answer_at t.resolve ~file ~byte with
  | None -> None
  | Some (occurrence, answer) -> (
      let span = occurrence.Index.span in
      match answer with
      | Resolve.Resolved declaration ->
          Some
            {
              Types.span;
              contents = [ fence (lines_of_declaration t declaration) ];
            }
      | Intrinsic ->
          (* The grammar drops the [\#] that introduces a primitive, so its
             own range covers the name alone and the marker is put back. *)
          Some
            {
              Types.span;
              contents = [ fence [ "#" ^ occurrence.name; "a primitive" ] ];
            }
      | Annotation ->
          Some
            {
              Types.span;
              contents =
                [
                  fence
                    [ annotation_line ~key:occurrence.name occurrence.value ];
                ];
            }
      | Include_path ->
          Some
            {
              Types.span;
              contents = [ fence [ occurrence.name; "an include path" ] ];
            }
      | Unresolved -> None)

(* ------------------------------------------------------------------ *)
(* The controller's seam: the protocol's positions over the engine *)

(* A range is two positions of one text, so it needs that text's line
   structure. Every span a snapshot reports indexes the text of the file it
   names, a leading byte-order mark included ([Index.text]), so the index is
   built over exactly those bytes and the two directions cannot disagree. *)
type coordinates = {
  lines : (string, Jide_lang.Line_index.t) Hashtbl.t;
      (* by the path of the file, as [Index.normalise] spells it *)
  entry : string; (* the document's own path, so the entry's ranges find it *)
  uri : string; (* the identifier the client opened the document with *)
}

let coordinates_of (snapshot : t) (document : Document.t) : coordinates =
  let lines = Hashtbl.create 8 in
  List.iter
    (fun (index : Index.t) ->
      Hashtbl.replace lines
        (Index.normalise index.file)
        (Jide_lang.Line_index.create index.text))
    (files snapshot);
  {
    lines;
    entry =
      (match Document.path document with
      | Some path -> Index.normalise path
      | None -> Index.normalise (entry snapshot));
    uri = Document.uri document;
  }

(* The identifier a path answers as: the name the client opened the document
   with while the path is that document, and the [file:] identifier of the
   path otherwise. It takes a path, never an identifier: the two sides are
   compared as paths, and the one conversion to a uri happens in the else
   branch, on a value no conversion has touched. *)
let uri_of_path coordinates (path : string) =
  if String.equal (Index.normalise path) coordinates.entry then coordinates.uri
  else Jide_lang.Uri.to_string (Jide_lang.Uri.of_path path)

let range_of coordinates (path : string) (span : Jide_lang.Span.t) : range =
  let blank = Jide_lang.Position.make ~line:0 ~character:0 in
  match Hashtbl.find_opt coordinates.lines (Index.normalise path) with
  | None -> { start_ = blank; end_ = blank }
  | Some lines ->
      {
        start_ = Jide_lang.Line_index.position_of_byte lines span.start_byte;
        end_ = Jide_lang.Line_index.position_of_byte lines span.end_byte;
      }

(* The protocol's location of one byte-level answer. The answer's file is the
   path the index holds, so both halves are read with that path: the range is
   looked up by it, and the identifier is made from it once. *)
let location_of coordinates (answer : Types.location) : location =
  {
    uri = uri_of_path coordinates answer.file;
    range = range_of coordinates answer.file answer.span;
  }

(* The outline's spans are the entry document's: {!outline} answers about one
   file, so every node is measured against that file's text. *)
let rec symbol_of coordinates (answer : Types.symbol) : symbol =
  {
    name = answer.name;
    detail = answer.detail;
    kind = answer.kind;
    range = range_of coordinates coordinates.entry answer.span;
    selection_range = range_of coordinates coordinates.entry answer.name_span;
    children = List.map (symbol_of coordinates) answer.children;
  }

(* The snapshot of the document the client holds, with the line structure that
   names it in the protocol's coordinates, and the byte a position in it names.

   The typed facts of the last analysis go in with it, so hover shows the
   compiler's signature, kind and annotations where the compiler typed the
   declaration. They are read only while the analysis describes the text the
   client holds: one that describes a version the client replaced would say
   what the text used to be. The two versions are the test, below, and it is
   the right one because the version numbers the text: a different number is
   different bytes.

   A position becomes a byte through [Document.byte_of_position], which clamps
   a line past the last one and a character past the end of a line. A position
   outside the text is therefore an answer about the end of it, never an
   error. *)
let with_snapshot (engine : Engine.t) ~uri ~position f =
  match (Engine.document engine ~uri, Engine.resolution engine ~uri) with
  | Some document, Some resolved ->
      let snapshot =
        match (Document.path document, Engine.analysis engine ~uri) with
        | Some path, Some analysis
          when analysis.Engine.version = Document.version document ->
            with_facts (of_resolve resolved) ~file:path
              analysis.Engine.snapshot.facts
        | _ -> of_resolve resolved
      in
      Some
        (f
           (coordinates_of snapshot document)
           (Document.byte_of_position document position)
           snapshot)
  | _ -> None

let definition engine ~uri ~position : location list =
  match
    with_snapshot engine ~uri ~position (fun coordinates byte (snapshot : t) ->
        match definition_at snapshot ~file:coordinates.entry ~byte with
        | None -> []
        | Some answer -> [ location_of coordinates answer ])
  with
  | Some answers -> answers
  | None -> []

let references engine ~uri ~position ~include_declaration : location list =
  match
    with_snapshot engine ~uri ~position (fun coordinates byte (snapshot : t) ->
        List.map (location_of coordinates)
          (references_at snapshot ~file:coordinates.entry ~byte
             ~include_declaration))
  with
  | Some answers -> answers
  | None -> []

let hover engine ~uri ~position : hover option =
  match
    with_snapshot engine ~uri ~position (fun coordinates byte (snapshot : t) ->
        Option.map
          (fun (answer : Types.hover) ->
            {
              range = Some (range_of coordinates coordinates.entry answer.span);
              contents =
                (match answer.contents with
                | block :: _ -> block
                | [] -> { Types.value = "" });
            })
          (hover_at snapshot ~file:coordinates.entry ~byte))
  with
  | Some answer -> answer
  | None -> None

let symbols engine ~uri : symbol list =
  match Engine.resolution engine ~uri with
  | None -> []
  | Some resolved -> (
      match Engine.document engine ~uri with
      | None -> []
      | Some document ->
          let snapshot = of_resolve resolved in
          let coordinates = coordinates_of snapshot document in
          List.map (symbol_of coordinates)
            (outline snapshot ~file:coordinates.entry))
