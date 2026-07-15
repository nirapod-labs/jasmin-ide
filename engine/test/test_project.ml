let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec from index =
    if index + fragment_length > text_length then false
    else if String.equal (String.sub text index fragment_length) fragment then
      true
    else from (index + 1)
  in
  fragment_length = 0 || from 0

let span text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec from index =
    if index + fragment_length > text_length then None
    else if String.equal (String.sub text index fragment_length) fragment then
      Some
        (Jide_lang.Span.make ~start_byte:index
           ~end_byte:(index + fragment_length))
    else from (index + 1)
  in
  from 0

let messages (loaded : Jide_engine.Project.loaded) =
  List.map
    (fun (problem : Jide_engine.Project.problem) -> problem.message)
    loaded.problems

let one_message (loaded : Jide_engine.Project.loaded) =
  match messages loaded with [ message ] -> message | _ -> "<not one problem>"

let one_span (loaded : Jide_engine.Project.loaded) =
  match loaded.problems with
  | [ problem ] -> problem.Jide_engine.Project.span
  | _ -> Jide_lang.Span.make ~start_byte:(-1) ~end_byte:(-1)

let parse = Jide_engine.Project.parse
let x86_64 = Option.get (Jide_jasmin.Adapter.arch_of_string "x86-64")
let arm_m4 = Option.get (Jide_jasmin.Adapter.arch_of_string "arm-m4")
let linux = Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux")
let windows = Option.get (Jide_jasmin.Adapter.call_conv_of_string "windows")

let full =
  String.concat "\n"
    [
      "{";
      "  \"version\": 1,";
      "  \"arch\": \"arm-m4\",";
      "  \"call_conv\": \"windows\",";
      "  \"include\": { \"SECP\": \"src/jasmin\", \"ARCH\": \
       \"src/jasmin/x86_64\" },";
      "  \"entries\": [\"src/jasmin/secp256k1.jazz\"]";
      "}";
      "";
    ]

