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
