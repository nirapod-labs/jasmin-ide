type target = { uri : string; position : Jide_lang.Position.t }
type reference = { target : target; include_declaration : bool }

let ( let* ) = Result.bind

(* The reading. Every message says which member failed and how, because the
   text is what the client sees in the InvalidParams answer. *)

let object_of ~path = function
  | `Assoc fields -> Ok fields
  | _ -> Error (path ^ " must be an object")

let integer_of ~path = function
  | `Int value -> Ok value
  | `Intlit text -> (
      match int_of_string_opt text with
      | Some value -> Ok value
      | None -> Error (path ^ " is not a representable integer"))
  | _ -> Error (path ^ " must be an integer")

let integer_member ~path name fields =
  match List.assoc_opt name fields with
  | None -> Error (path ^ " is missing")
  | Some json ->
      let* value = integer_of ~path json in
      if value < 0 then Error (path ^ " must not be negative") else Ok value

let string_member ~path name fields =
  match List.assoc_opt name fields with
  | None -> Error (path ^ " is missing")
  | Some (`String value) -> Ok value
  | Some _ -> Error (path ^ " must be a string")

let boolean_member ~path name fields =
  match List.assoc_opt name fields with
  | None -> Error (path ^ " is missing")
  | Some (`Bool value) -> Ok value
  | Some _ -> Error (path ^ " must be a boolean")

let position_of_json ~path json =
  let* fields = object_of ~path json in
  let* line = integer_member ~path:(path ^ ".line") "line" fields in
  let* character =
    integer_member ~path:(path ^ ".character") "character" fields
  in
  Ok (Jide_lang.Position.make ~line ~character)

let text_document_of_fields fields =
  match List.assoc_opt "textDocument" fields with
  | None -> Error "textDocument is missing"
  | Some json ->
      let* document = object_of ~path:"textDocument" json in
      string_member ~path:"textDocument.uri" "uri" document

let target_of_fields fields =
  let* uri = text_document_of_fields fields in
  match List.assoc_opt "position" fields with
  | None -> Error "position is missing"
  | Some json ->
      let* position = position_of_json ~path:"position" json in
      Ok { uri; position }

let target json =
  let* fields = object_of ~path:"params" json in
  target_of_fields fields

let reference json =
  let* fields = object_of ~path:"params" json in
  let* target = target_of_fields fields in
  match List.assoc_opt "context" fields with
  | None -> Error "context is missing"
  | Some json ->
      let* context = object_of ~path:"context" json in
      let* include_declaration =
        boolean_member ~path:"context.includeDeclaration" "includeDeclaration"
          context
      in
      Ok { target; include_declaration }

let document json =
  let* fields = object_of ~path:"params" json in
  text_document_of_fields fields

(* The writing. A range is two positions, and Json_conv already writes one the
   way the protocol spells it. *)

let range_json (range : Jide_engine.Locate.range) =
  `Assoc
    [
      ("start", Json_conv.position_json range.start_);
      ("end", Json_conv.position_json range.end_);
    ]

let location_json (location : Jide_engine.Locate.location) =
  `Assoc [ ("uri", `String location.uri); ("range", range_json location.range) ]

let locations_json locations = `List (List.map location_json locations)

let definition_json = function
  | [] -> `Null
  | locations -> locations_json locations

let references_json locations = locations_json locations

let markdown_json (markdown : Jide_engine.Types.markdown) =
  `Assoc [ ("kind", `String "markdown"); ("value", `String markdown.value) ]

let hover_json = function
  | None -> `Null
  | Some (hover : Jide_engine.Locate.hover) ->
      let fields = [ ("contents", markdown_json hover.contents) ] in
      let fields =
        match hover.range with
        | Some range -> fields @ [ ("range", range_json range) ]
        | None -> fields
      in
      `Assoc fields

(* The protocol's SymbolKind numbers. The four the compiler cannot tell apart
   are [Variable]: a parameter, a local, a global and an unknown name are all
   a name that holds a value. A type alias is a [Struct], the protocol having
   no kind for an alias, and an [exec] is a [Function] because that is what it
   declares. *)
let symbol_kind_number (kind : Jide_engine.Types.symbol_kind) =
  match kind with
  | Function -> 12
  | Parameter -> 13
  | Variable -> 13
  | Global -> 13
  | Namespace -> 3
  | Type_alias -> 23
  | Require -> 2
  | Exec -> 12
  | File -> 1
  | Unknown -> 13

let rec symbol_json (symbol : Jide_engine.Locate.symbol) =
  let fields =
    [
      ("name", `String symbol.name);
      ("kind", `Int (symbol_kind_number symbol.kind));
      ("range", range_json symbol.range);
      ("selectionRange", range_json symbol.selection_range);
    ]
  in
  let fields =
    match symbol.detail with
    | Some detail -> fields @ [ ("detail", `String detail) ]
    | None -> fields
  in
  let fields =
    match symbol.children with
    | [] -> fields
    | children ->
        fields @ [ ("children", `List (List.map symbol_json children)) ]
  in
  `Assoc fields

let symbols_json symbols = `List (List.map symbol_json symbols)
