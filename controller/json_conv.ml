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

(* The answers of the editing requests. Each one converts the engine's own
   values to the protocol's JSON, and every range it writes goes through
   [range_json], so a span becomes a UTF-16 range in the line index of the
   text it belongs to and in no other way. Completion and signature help are
   the two with no range to convert: a completion item names its text and a
   signature names its parameters, not their bytes. *)

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

let text_edit_json (lines : Line_index.t) (edit : Jide_engine.Types.text_edit) =
  `Assoc
    [
      ("range", range_json lines edit.span); ("newText", `String edit.new_text);
    ]

let workspace_edit_json (edits : Features.edit list) =
  let change (part : Features.edit) =
    (part.file.uri, `List (List.map (text_edit_json part.file.lines) part.edits))
  in
  `Assoc [ ("changes", `Assoc (List.map change edits)) ]

(* The protocol's numbers for a symbol's kind. A parameter, a global, a
   register variable and a name the index could not classify are all reported
   as a variable, a type alias as a struct, and a require as a file: the
   protocol has no closer kind for any of them. *)
let symbol_kind_json (kind : Jide_engine.Types.symbol_kind) =
  `Int
    (match kind with
    | Function -> 12
    | Parameter -> 13
    | Variable -> 13
    | Global -> 13
    | Namespace -> 3
    | Type_alias -> 23
    | Require -> 1
    | Exec -> 12
    | File -> 1
    | Unknown -> 13)

let symbol_information_json ~container (file : Features.file)
    (symbol : Jide_engine.Types.symbol) =
  let fields =
    [ ("name", `String symbol.name); ("kind", symbol_kind_json symbol.kind) ]
  in
  let fields =
    match container with
    | Some name -> fields @ [ ("containerName", `String name) ]
    | None -> fields
  in
  `Assoc
    (fields
    @ [
        ( "location",
          `Assoc
            [
              ("uri", `String file.uri);
              ("range", range_json file.lines symbol.name_span);
            ] );
      ])

(* A symbol and everything nested in it, the nested ones naming the symbol
   they sit in: the engine nests a namespace's members in it, and a client
   that asks for the workspace's symbols expects all of them. *)
let rec symbol_entries ~container (file : Features.file)
    (symbol : Jide_engine.Types.symbol) =
  symbol_information_json ~container file symbol
  :: List.concat_map
       (symbol_entries ~container:(Some symbol.name) file)
       symbol.children

let symbols_json (symbols : Features.located_symbol list) =
  `List
    (List.concat_map
       (fun (located : Features.located_symbol) ->
         symbol_entries ~container:None located.file located.symbol)
       symbols)

let highlight_kind_json (kind : Jide_engine.Types.highlight_kind) =
  `Int (match kind with Text_ -> 1 | Read -> 2 | Write -> 3)

let highlights_json (lines : Line_index.t)
    (highlights : Jide_engine.Types.highlight list) =
  `List
    (List.map
       (fun (highlight : Jide_engine.Types.highlight) ->
         `Assoc
           [
             ("range", range_json lines highlight.span);
             ("kind", highlight_kind_json highlight.kind);
           ])
       highlights)

(* The legend. Its order is the encoding: a token type is the position of its
   name in [semantic_token_types], and a modifier is the bit of its position
   in [semantic_token_modifiers]. Both are advertised in this order, so a
   client reads a token's type and its modifiers the way they were written. *)
let semantic_token_types =
  [
    "namespace";
    "type";
    "function";
    "parameter";
    "variable";
    "property";
    "keyword";
  ]

let semantic_token_modifiers = [ "declaration"; "inline"; "export" ]

let semantic_token_type_name
    (token_type : Jide_engine.Types.semantic_token_type) =
  match token_type with
  | Token_namespace -> "namespace"
  | Token_type -> "type"
  | Token_function -> "function"
  | Token_parameter -> "parameter"
  | Token_variable -> "variable"
  | Token_property -> "property"
  | Token_keyword -> "keyword"

let index_of name names =
  let rec go index = function
    | [] -> None
    | candidate :: rest ->
        if String.equal candidate name then Some index else go (index + 1) rest
  in
  go 0 names

let modifier_mask modifiers =
  List.fold_left
    (fun mask modifier ->
      match index_of modifier semantic_token_modifiers with
      | Some index -> mask lor (1 lsl index)
      | None -> mask)
    0 modifiers

(* The tokens in source order, five integers each, each token given relative
   to the one before it: the line delta, the character or the character delta,
   the length in UTF-16 code units, the type, and the modifiers as a bit set.
   A token that has no length, one that overlaps the token before it and one
   whose type the legend does not name are dropped: the first two cannot be
   encoded, and the last has no number to carry. *)
let semantic_tokens_json (lines : Line_index.t)
    (tokens : Jide_engine.Types.semantic_token list) =
  let ordered =
    List.stable_sort
      (fun (left : Jide_engine.Types.semantic_token)
           (right : Jide_engine.Types.semantic_token) ->
        Span.compare left.span right.span)
      tokens
  in
  let rec go previous_line previous_character previous_end = function
    | [] -> []
    | (token : Jide_engine.Types.semantic_token) :: rest -> (
        let length = Line_index.utf16_length lines token.span in
        match
          ( index_of
              (semantic_token_type_name token.token_type)
              semantic_token_types,
            length )
        with
        | Some type_index, length
          when length > 0 && token.span.start_byte >= previous_end ->
            let start =
              Line_index.position_of_byte lines token.span.start_byte
            in
            let delta_line = start.line - previous_line in
            let delta_character =
              if delta_line = 0 then start.character - previous_character
              else start.character
            in
            delta_line :: delta_character :: length :: type_index
            :: modifier_mask token.modifiers
            :: go start.line start.character token.span.end_byte rest
        | Some _, _ | None, _ ->
            go previous_line previous_character previous_end rest)
  in
  `Assoc
    [ ("data", `List (List.map (fun value -> `Int value) (go 0 0 0 ordered))) ]

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
