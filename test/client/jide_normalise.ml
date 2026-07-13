(* Normalised lines: "<id> <method-or-kind> <json>", with object keys sorted
   lexicographically at every depth and no spaces. A reply carries "result"
   or "error"; a server-sent notification carries "method" and prints its
   params. *)

let rec sort_keys = function
  | `Assoc fields ->
    let sorted =
      List.map (fun (name, value) -> (name, sort_keys value)) fields
      |> List.stable_sort (fun (left, _) (right, _) -> String.compare left right)
    in
    `Assoc sorted
  | `List items -> `List (List.map sort_keys items)
  | `Bool _ | `Float _ | `Int _ | `Intlit _ | `Null | `String _ as json -> json

let compact json = Yojson.Safe.to_string json

(* The id as the server sent it: a number verbatim, a string without its
   quotes, and "-" when the message carries no id. *)
let id_string = function
  | None -> "-"
  | Some `Null -> "-"
  | Some (`String id) -> id
  | Some json -> compact json

type message = { id : string; kind : string; body : string }

let is_reply = function
  | `Assoc fields -> List.mem_assoc "result" fields || List.mem_assoc "error" fields
  | `Bool _ | `Float _ | `Int _ | `Intlit _ | `List _ | `Null | `String _ -> false

let message_of_json = function
  | `Assoc fields -> (
    let id = id_string (List.assoc_opt "id" fields) in
    match List.assoc_opt "result" fields with
    | Some result -> Ok { id; kind = "result"; body = compact (sort_keys result) }
    | None -> (
      match List.assoc_opt "error" fields with
      | Some error -> Ok { id; kind = "error"; body = compact (sort_keys error) }
      | None -> (
        match List.assoc_opt "method" fields with
        | Some (`String method_name) ->
          let params = match List.assoc_opt "params" fields with Some params -> params | None -> `Null in
          Ok { id; kind = method_name; body = compact (sort_keys params) }
        | Some _ | None -> Error "the message has neither a result, an error nor a method")))
  | `Bool _ | `Float _ | `Int _ | `Intlit _ | `List _ | `Null | `String _ ->
    Error "the message is not a JSON object"

let line message = Printf.sprintf "%s %s %s" message.id message.kind message.body
