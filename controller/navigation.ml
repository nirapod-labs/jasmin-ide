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

