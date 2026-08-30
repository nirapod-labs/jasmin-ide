open Jide_lang

let position_json (position : Position.t) =
  `Assoc
    [ ("line", `Int position.line); ("character", `Int position.character) ]

let range_json (lines : Line_index.t) (span : Span.t) =
  `Assoc
    [
      ( "start",
        position_json (Line_index.position_of_byte lines span.start_byte) );
      ("end", position_json (Line_index.position_of_byte lines span.end_byte));
    ]

let severity_json (severity : Diagnostic.severity) =
  `Int
    (match severity with
    | Diagnostic.Error -> 1
    | Diagnostic.Warning -> 2
    | Diagnostic.Information -> 3
    | Diagnostic.Hint -> 4)

let related_json ~uri lines ((span, message) : Span.t * string) =
  `Assoc
    [
      ( "location",
        `Assoc [ ("uri", `String uri); ("range", range_json lines span) ] );
      ("message", `String message);
    ]

let diagnostic_json ~uri lines (diagnostic : Diagnostic.t) =
  let fields =
    [
      ("range", range_json lines diagnostic.span);
      ("severity", severity_json diagnostic.severity);
    ]
  in
  let fields =
    match diagnostic.code with
    | Some code -> fields @ [ ("code", `String code) ]
    | None -> fields
  in
  let fields = fields @ [ ("source", `String diagnostic.source) ] in
  let fields = fields @ [ ("message", `String diagnostic.message) ] in
  match diagnostic.related with
  | [] -> `Assoc fields
  | related ->
      `Assoc
        (fields
        @ [
            ( "relatedInformation",
              `List (List.map (related_json ~uri lines) related) );
          ])

let publish_params ~uri ~version ~lines diagnostics =
  let fields = [ ("uri", `String uri) ] in
  let fields =
    match version with
    | Some version -> fields @ [ ("version", `Int version) ]
    | None -> fields
  in
  `Assoc
    (fields
    @ [
        ( "diagnostics",
          `List (List.map (diagnostic_json ~uri lines) diagnostics) );
      ])

(* The answers of the two editing requests the engine can compute without a
   check. Both take the engine's own values and write the protocol's JSON;
   neither has a range to convert, because a completion item names its text
   and a signature names its parameters, not its bytes. *)

let markup_json (markdown : Jide_engine.Types.markdown) =
  `Assoc [ ("kind", `String "markdown"); ("value", `String markdown.value) ]

(* The protocol's numbers for a completion item's kind. A Jasmin register or
   stack variable is a variable, and a namespace is a module: the protocol
   has no closer kind for either. *)
let completion_kind_json (kind : Jide_engine.Types.completion_kind) =
  `Int
    (match kind with
    | Keyword -> 14
    | Function -> 3
    | Variable_ -> 6
    | Field -> 5
    | Module_ -> 9
    | Type_parameter -> 25
    | Snippet -> 15
    | Text -> 1)

(* Only [label] and [kind] are always there. The four optional fields are
   written when the engine has them and left out when it does not, because
   the protocol reads an absent field and a null one differently: an absent
   [detail] is "no detail", a null one is a detail that is null. *)
let completion_item_json (item : Jide_engine.Types.completion_item) =
  let optional name encode = function
    | Some value -> [ (name, encode value) ]
    | None -> []
  in
  `Assoc
    ([ ("label", `String item.label); ("kind", completion_kind_json item.kind) ]
    @ optional "detail" (fun text -> `String text) item.detail
    @ optional "documentation" markup_json item.documentation
    @ optional "insertText" (fun text -> `String text) item.insert_text
    @ optional "sortText" (fun text -> `String text) item.sort_text)

let completion_json (items : Jide_engine.Types.completion_item list) =
  `List (List.map completion_item_json items)

let signature_parameter_json (parameter : Jide_engine.Types.signature_parameter)
    =
  let fields = [ ("label", `String parameter.label) ] in
  match parameter.documentation with
  | Some documentation ->
      `Assoc (fields @ [ ("documentation", markup_json documentation) ])
  | None -> `Assoc fields

(* [label] is the whole signature as the editor prints it, callee and result
   included, so a client with no room for the parts still shows what the call
   takes. [parameters] is left out when the engine named none: an empty list
   would tell a client that the call takes no argument. *)
let signature_json (signature : Jide_engine.Types.signature) =
  let fields = [ ("label", `String signature.label) ] in
  let fields =
    match signature.documentation with
    | Some documentation ->
        fields @ [ ("documentation", markup_json documentation) ]
    | None -> fields
  in
  match signature.parameters with
  | [] -> `Assoc fields
  | parameters ->
      `Assoc
        (fields
        @ [
            ("parameters", `List (List.map signature_parameter_json parameters));
          ])

let signature_help_json (help : Jide_engine.Types.signature_help) =
  `Assoc
    [
      ("signatures", `List (List.map signature_json help.signatures));
      ("activeSignature", `Int help.active_signature);
      ("activeParameter", `Int help.active_parameter);
    ]

let int_of_json = function
  | `Int value -> Some value
  | `Intlit text -> int_of_string_opt text
  | `Float value -> Some (int_of_float value)
  | _ -> None

let rec value_at path json =
  match path with
  | [] -> Some json
  | key :: rest -> (
      match json with
      | `Assoc fields -> Option.bind (List.assoc_opt key fields) (value_at rest)
      | _ -> None)

(* The shapes a client's settings object has been seen in. A section named
   "diagnostics.delay" can only mean the setting, so the dotted names come
   first at their depth. The last groups are the packets that carry a
   settings object rather than being one: the parameters of a change
   notification hold it under "settings" and the parameters of an initialize
   request under "initializationOptions". *)
let delay_paths =
  [
    [ "jasminIde"; "diagnostics"; "delay" ];
    [ "diagnostics"; "delay" ];
    [ "diagnostics.delay" ];
    [ "delay" ];
    [ "settings"; "jasminIde"; "diagnostics"; "delay" ];
    [ "settings"; "diagnostics"; "delay" ];
    [ "settings"; "delay" ];
    [ "initializationOptions"; "jasminIde"; "diagnostics"; "delay" ];
    [ "initializationOptions"; "diagnostics"; "delay" ];
    [ "initializationOptions"; "delay" ];
  ]

let delay_of_settings settings =
  let rec first = function
    | [] -> None
    | path :: rest -> (
        match Option.bind (value_at path settings) int_of_json with
        | Some delay -> Some delay
        | None -> first rest)
  in
  first delay_paths
