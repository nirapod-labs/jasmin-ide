(* The conversion of a diagnostic to the protocol's JSON: the numbers the
   protocol assigns to a severity, the UTF-16 columns of a range over text
   that is not plain ASCII, the fields that are present only sometimes, and
   the settings reader that pulls the debounce out of a client's JSON. *)

open Jide_lang

let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let check_json name value expected =
  let expected = Yojson.Safe.from_string expected in
  let actual = Yojson.Safe.to_string value in
  let expected = Yojson.Safe.to_string expected in
  if actual <> expected then (
    incr failures;
    Printf.printf "FAIL %s\n  expected: %s\n  actual:   %s\n%!" name expected
      actual)

(* "aé" is three bytes and two UTF-16 code units, so a byte offset and a
   character offset differ after the first line. *)
let accented = "aé\nb"

let diagnostic ?code ?(related = []) ?(severity = Diagnostic.Error) ~start_byte
    ~end_byte message =
  Diagnostic.make ?code ~severity ~related
    (Span.make ~start_byte ~end_byte)
    message

let test_position () =
  check_json "position: zero"
    (Jide_controller.Json_conv.position_json
       (Position.make ~line:0 ~character:0))
    {|{"line":0,"character":0}|};
  check_json "position: another line"
    (Jide_controller.Json_conv.position_json
       (Position.make ~line:12 ~character:4))
    {|{"line":12,"character":4}|}

let test_range () =
  let lines = Line_index.create accented in
  check_json "range: ASCII prefix"
    (Jide_controller.Json_conv.range_json lines
       (Span.make ~start_byte:0 ~end_byte:1))
    {|{"start":{"line":0,"character":0},"end":{"line":0,"character":1}}|};
  check_json "range: over a two-byte character"
    (Jide_controller.Json_conv.range_json lines
       (Span.make ~start_byte:1 ~end_byte:3))
    {|{"start":{"line":0,"character":1},"end":{"line":0,"character":2}}|};
  check_json "range: across the line break"
    (Jide_controller.Json_conv.range_json lines
       (Span.make ~start_byte:0 ~end_byte:5))
    {|{"start":{"line":0,"character":0},"end":{"line":1,"character":1}}|};
  check_json "range: a span past the end is clamped"
    (Jide_controller.Json_conv.range_json lines
       (Span.make ~start_byte:1 ~end_byte:900))
    {|{"start":{"line":0,"character":1},"end":{"line":1,"character":1}}|}

let test_severity () =
  check_json "severity: error"
    (Jide_controller.Json_conv.severity_json Diagnostic.Error)
    "1";
  check_json "severity: warning"
    (Jide_controller.Json_conv.severity_json Diagnostic.Warning)
    "2";
  check_json "severity: information"
    (Jide_controller.Json_conv.severity_json Diagnostic.Information)
    "3";
  check_json "severity: hint"
    (Jide_controller.Json_conv.severity_json Diagnostic.Hint)
    "4"

let test_diagnostic () =
  let lines = Line_index.create accented in
  (* The default source is the language's own name, which the client shows
     beside the message. *)
  check_json "diagnostic: the least it can carry"
    (Jide_controller.Json_conv.diagnostic_json ~uri:"file:///a.jazz" lines
       (diagnostic ~start_byte:0 ~end_byte:1 "unexpected token"))
    {|{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":1}},"severity":1,"source":"jasmin","message":"unexpected token"}|};
  check_json "diagnostic: a code, a warning, and a related place"
    (Jide_controller.Json_conv.diagnostic_json ~uri:"file:///a.jazz" lines
       (diagnostic ~code:"typing" ~severity:Diagnostic.Warning
          ~related:[ (Span.make ~start_byte:4 ~end_byte:5, "declared here") ]
          ~start_byte:0 ~end_byte:1 "unused variable"))
    {|{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":1}},"severity":2,"code":"typing","source":"jasmin","message":"unused variable","relatedInformation":[{"location":{"uri":"file:///a.jazz","range":{"start":{"line":1,"character":0},"end":{"line":1,"character":1}}},"message":"declared here"}]}|}

let test_publish () =
  let lines = Line_index.create accented in
  let one = diagnostic ~start_byte:0 ~end_byte:1 "first" in
  let two = diagnostic ~start_byte:4 ~end_byte:5 "second" in
  check_json "publish: the version and the full set"
    (Jide_controller.Json_conv.publish_params ~uri:"file:///a.jazz"
       ~version:(Some 7) ~lines [ one; two ])
    {|{"uri":"file:///a.jazz","version":7,"diagnostics":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":1}},"severity":1,"source":"jasmin","message":"first"},{"range":{"start":{"line":1,"character":0},"end":{"line":1,"character":1}},"severity":1,"source":"jasmin","message":"second"}]}|};
  check_json "publish: an empty set clears, without a version"
    (Jide_controller.Json_conv.publish_params ~uri:"file:///a.jazz"
       ~version:None ~lines [])
    {|{"uri":"file:///a.jazz","diagnostics":[]}|}

let test_delay_of_settings () =
  let delay = Jide_controller.Json_conv.delay_of_settings in
  let of_string text = delay (Yojson.Safe.from_string text) in
  check "delay: the documented section"
    (of_string {|{"jasminIde":{"diagnostics":{"delay":300}}}|} = Some 300);
  check "delay: a plain section"
    (of_string {|{"diagnostics":{"delay":120}}|} = Some 120);
  check "delay: a dotted key" (of_string {|{"diagnostics.delay":90}|} = Some 90);
  check "delay: the value alone" (of_string {|{"delay":80}|} = Some 80);
  check "delay: a settings object in a notification"
    (of_string {|{"settings":{"diagnostics":{"delay":400}}}|} = Some 400);
  check "delay: a settings object in an initialize request"
    (of_string {|{"initializationOptions":{"diagnostics":{"delay":500}}}|}
    = Some 500);
  check "delay: a number written as a float"
    (of_string {|{"diagnostics":{"delay":300.0}}|} = Some 300);
  check "delay: absent" (of_string {|{"capabilities":{}}|} = None);
  check "delay: not a number"
    (of_string {|{"diagnostics":{"delay":"fast"}}|} = None);
  check "delay: not an object" (of_string {|[]|} = None);
  check "delay: a null section" (of_string {|{"diagnostics":null}|} = None)

let markdown value = { Jide_engine.Types.value }

let item ?detail ?documentation ?insert_text ?sort_text ~label ~kind () =
  {
    Jide_engine.Types.label;
    kind;
    detail;
    documentation;
    insert_text;
    sort_text;
  }

let parameter ?documentation label = { Jide_engine.Types.label; documentation }

let signature ?documentation ?(parameters = []) label =
  { Jide_engine.Types.label; documentation; parameters }

(* The completion answer: the two fields every item carries, the optional
   fields only when the engine has them, and the protocol's number for each
   kind the engine can return. *)
let test_completion () =
  check_json "completion: an item with only the required fields"
    (Jide_controller.Json_conv.completion_json
       [ item ~label:"fn" ~kind:Jide_engine.Types.Keyword () ])
    {|[{"label":"fn","kind":14}]|};
  check_json "completion: every optional field, in the protocol's names"
    (Jide_controller.Json_conv.completion_json
       [
         item ~label:"add" ~kind:Jide_engine.Types.Function
           ~detail:"fn add(reg u64 a, reg u64 b) -> reg u64"
           ~documentation:(markdown "twice") ~insert_text:"add"
           ~sort_text:"0001add" ();
       ])
    {|[{"label":"add","kind":3,"detail":"fn add(reg u64 a, reg u64 b) -> reg u64","documentation":{"kind":"markdown","value":"twice"},"insertText":"add","sortText":"0001add"}]|};
  check_json "completion: no item is an empty list, not null"
    (Jide_controller.Json_conv.completion_json [])
    {|[]|};
  (* Every kind the engine has, so a number that drifts is caught here. *)
  let kinds =
    [
      (Jide_engine.Types.Keyword, 14);
      (Function, 3);
      (Variable_, 6);
      (Field, 5);
      (Module_, 9);
      (Type_parameter, 25);
      (Snippet, 15);
      (Text, 1);
    ]
  in
  List.iter
    (fun (kind, expected) ->
      let json =
        Jide_controller.Json_conv.completion_json [ item ~label:"x" ~kind () ]
      in
      match json with
      | `List [ `Assoc fields ] -> (
          match List.assoc_opt "kind" fields with
          | Some (`Int actual) ->
              check
                (Printf.sprintf "completion: kind %d" expected)
                (actual = expected)
          | _ ->
              check
                (Printf.sprintf "completion: kind %d is a number" expected)
                false)
      | _ -> check (Printf.sprintf "completion: kind %d" expected) false)
    kinds

(* The signature-help answer: the signatures with their parameters, the
   active indexes as the engine gave them, and no [parameters] field for a
   signature the engine left without any. *)
let test_signature_help () =
  check_json "signature help: one signature with two parameters"
    (Jide_controller.Json_conv.signature_help_json
       {
         Jide_engine.Types.signatures =
           [
             signature "add(reg u64, reg u64) -> reg u64"
               ~parameters:[ parameter "reg u64"; parameter "reg u64" ];
           ];
         active_signature = 0;
         active_parameter = 1;
       })
    {|{"signatures":[{"label":"add(reg u64, reg u64) -> reg u64","parameters":[{"label":"reg u64"},{"label":"reg u64"}]}],"activeSignature":0,"activeParameter":1}|};
  check_json "signature help: a parameter's documentation"
    (Jide_controller.Json_conv.signature_help_json
       {
         Jide_engine.Types.signatures =
           [
             signature "spill(reg u64)" ~documentation:(markdown "spill one")
               ~parameters:
                 [ parameter ~documentation:(markdown "the value") "reg u64" ];
           ];
         active_signature = 0;
         active_parameter = 0;
       })
    {|{"signatures":[{"label":"spill(reg u64)","documentation":{"kind":"markdown","value":"spill one"},"parameters":[{"label":"reg u64","documentation":{"kind":"markdown","value":"the value"}}]}],"activeSignature":0,"activeParameter":0}|};
  check_json "signature help: no parameter field when the engine named none"
    (Jide_controller.Json_conv.signature_help_json
       {
         Jide_engine.Types.signatures = [ signature "f()" ];
         active_signature = 0;
         active_parameter = 0;
       })
    {|{"signatures":[{"label":"f()"}],"activeSignature":0,"activeParameter":0}|}

let () =
  test_position ();
  test_range ();
  test_severity ();
  test_diagnostic ();
  test_publish ();
  test_completion ();
  test_signature_help ();
  test_delay_of_settings ();
  if !failures = 0 then print_endline "all conversion tests passed"
  else (
    Printf.printf "%d conversion test(s) failed\n" !failures;
    exit 1)
