(* The project file, [jasmin.project]: what the parser reads, and what it says
   about a line that is wrong.

   Every problem is a diagnostic on the project file itself, at the line that
   is wrong, so every test that expects one also pins its span: which bytes it
   covers and which line they are on. *)

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

(* The bytes of the first [fragment] in [text]: for the cases where the span
   indexes the file the reader holds, byte-order mark included. *)
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

(* The content of a one-based line of [text], without its terminator. *)
let line_bounds text number =
  let length = String.length text in
  let rec seek index current =
    if current = number then
      match String.index_from_opt text index '\n' with
      | None -> (index, length)
      | Some stop ->
          let stop =
            if stop > index && String.get text (stop - 1) = '\r' then stop - 1
            else stop
          in
          (index, stop)
    else
      match String.index_from_opt text index '\n' with
      | None -> (length, length)
      | Some stop -> seek (stop + 1) (current + 1)
  in
  seek 0 1

(* The bytes of [fragment], searched from the start of [line], so a fragment
   that appears twice in a file is still the one meant. *)
let span_on_line text ~line fragment =
  let start, stop = line_bounds text line in
  let fragment_length = String.length fragment in
  let rec from index =
    if index + fragment_length > stop then None
    else if String.equal (String.sub text index fragment_length) fragment then
      Some
        (Jide_lang.Span.make ~start_byte:index
           ~end_byte:(index + fragment_length))
    else from (index + 1)
  in
  from start

(* The span a problem must have: exactly [fragment], on [line]. *)
let check_span name text ~line fragment (actual : Jide_lang.Span.t) =
  match span_on_line text ~line fragment with
  | Some expected when actual = expected -> ()
  | Some expected ->
      incr failures;
      Printf.printf
        "FAIL %s: the problem covers bytes %d-%d, not the %S of line %d at \
         bytes %d-%d\n\
         %!"
        name actual.Jide_lang.Span.start_byte actual.Jide_lang.Span.end_byte
        fragment line expected.Jide_lang.Span.start_byte
        expected.Jide_lang.Span.end_byte
  | None ->
      incr failures;
      Printf.printf "FAIL %s: line %d holds no %S to point at\n%!" name line
        fragment

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

let test_a_full_file () =
  let loaded = parse ~path:"/work/project/jasmin-project.json" full in
  check "full: no problem" (loaded.problems = []);
  check "full: the architecture"
    ((loaded.project : Jide_engine.Project.t).arch = Some arm_m4);
  check "full: the calling convention"
    ((loaded.project : Jide_engine.Project.t).call_conv = Some windows);
  check "full: both include paths, resolved"
    ((loaded.project : Jide_engine.Project.t).idirs
    = [
        ("SECP", "/work/project/src/jasmin");
        ("ARCH", "/work/project/src/jasmin/x86_64");
      ]);
  check "full: the entry, resolved"
    ((loaded.project : Jide_engine.Project.t).entries
    = [ "/work/project/src/jasmin/secp256k1.jazz" ]);
  check "full: the file it came from"
    (loaded.path = Some "/work/project/jasmin-project.json");
  check "full: no byte-order mark" (loaded.bom = 0);
  check "full: the text" (loaded.text = full);
  check "full: no diagnostics for the reader"
    (Jide_engine.Project.diagnostics ~uri:"file:///p" loaded = [])

let test_a_minimal_file () =
  let loaded = parse "{}" in
  check "minimal: no problem" (loaded.problems = []);
  check "minimal: nothing set" (loaded.project = Jide_engine.Project.empty);
  let loaded = parse "{\"version\": 1}" in
  check "minimal: a version alone is no problem" (loaded.problems = []);
  check "minimal: and sets nothing" (loaded.project = Jide_engine.Project.empty)

let test_an_unknown_field () =
  let text = "{\"version\": 1, \"arch\": \"x86-64\", \"colours\": true}\n" in
  let loaded = parse text in
  check "unknown: one problem" (List.length (messages loaded) = 1);
  check "unknown: the message names the field"
    (contains (one_message loaded) "unknown field: colours");
  check "unknown: the range is the field's name"
    (one_span loaded = Option.get (span text "\"colours\""));
  check "unknown: the known fields still apply"
    ((loaded.project : Jide_engine.Project.t).arch = Some x86_64);
  match
    Jide_engine.Project.diagnostics ~uri:"file:///w/jasmin-project.json" loaded
  with
  | [ diagnostic ] ->
      check "unknown: the diagnostic's code"
        (diagnostic.Jide_lang.Diagnostic.code = Some "project");
      check "unknown: it is an error"
        (diagnostic.severity = Jide_lang.Diagnostic.Error);
      check "unknown: it sits on the field"
        (diagnostic.span = Option.get (span text "\"colours\""))
  | other ->
      check
        (Printf.sprintf "unknown: one diagnostic, got %d" (List.length other))
        false

let test_a_bad_architecture () =
  let text = "{\"arch\": \"mips\"}" in
  let loaded = parse text in
  check "bad arch: one problem" (List.length (messages loaded) = 1);
  check "bad arch: the message names it and the accepted values"
    (let message = one_message loaded in
     contains message "unknown architecture: mips"
     && contains message "x86-64" && contains message "riscv");
  check "bad arch: the range is the value"
    (one_span loaded = Option.get (span text "\"mips\""));
  check "bad arch: no architecture is set"
    ((loaded.project : Jide_engine.Project.t).arch = None)

let test_a_bad_calling_convention () =
  let text = "{\"call_conv\": \"plan9\"}" in
  let loaded = parse text in
  check "bad call conv: one problem" (List.length (messages loaded) = 1);
  check "bad call conv: the message names the accepted values"
    (let message = one_message loaded in
     contains message "unknown calling convention: plan9"
     && contains message "linux" && contains message "windows");
  check "bad call conv: no calling convention is set"
    ((loaded.project : Jide_engine.Project.t).call_conv = None)

let test_a_bad_version () =
  let text = "{\"version\": 2, \"arch\": \"x86-64\"}" in
  let loaded = parse text in
  check "bad version: one problem" (List.length (messages loaded) = 1);
  check "bad version: the message names the version"
    (contains (one_message loaded) "version 2");
  check "bad version: the range is the value"
    (one_span loaded = Option.get (span text "2"));
  check "bad version: no field is read"
    (loaded.project = Jide_engine.Project.empty);
  let loaded = parse "{\"version\": \"one\"}" in
  check "bad version: a string is refused" (List.length (messages loaded) = 1)

let test_a_bom () =
  let text = "\xEF\xBB\xBF{\"arch\": \"x86-64\"}" in
  let loaded = parse text in
  check "bom: no problem" (loaded.problems = []);
  check "bom: three bytes removed" (loaded.bom = 3);
  check "bom: the text has no mark" (loaded.text = "{\"arch\": \"x86-64\"}");
  check "bom: the architecture is read"
    ((loaded.project : Jide_engine.Project.t).arch = Some x86_64);
  (* The diagnostic's span indexes the file, not the text the parser saw. *)
  let text = "\xEF\xBB\xBF{\"colours\": true}" in
  let loaded = parse text in
  check "bom: an unknown field is three bytes further on"
    (one_span loaded = Option.get (span text "\"colours\""))

let test_not_json () =
  let loaded = parse "this is not json" in
  check "not json: one problem" (List.length (messages loaded) = 1);
  check "not json: the message says so"
    (contains (one_message loaded) "not valid JSON");
  check "not json: the defaults apply"
    (loaded.project = Jide_engine.Project.empty);
  let loaded = parse "[]" in
  check "not an object: one problem" (List.length (messages loaded) = 1);
  check "not an object: the message says so"
    (contains (one_message loaded) "one JSON object")

let test_a_missing_file () =
  let path = Filename.temp_file "jide-project" ".json" in
  Sys.remove path;
  let loaded = Jide_engine.Project.load path in
  check "missing: no problem" (loaded.problems = []);
  check "missing: no text" (loaded.text = "");
  check "missing: no file" (loaded.path = None);
  check "missing: the defaults apply"
    (loaded.project = Jide_engine.Project.empty)

(* A file that is there and cannot be read is not the same thing as a file
   that is not there: the first is reported, the second is the ordinary case
   the test above pins. *)
let test_an_unreadable_file () =
  let directory = Filename.temp_file "jide-project" "" in
  Sys.remove directory;
  Sys.mkdir directory 0o700;
  let path = Filename.concat directory Jide_engine.Project.file_name in
  let channel = open_out_bin path in
  output_string channel "{\"arch\": \"arm-m4\"}";
  close_out channel;
  (* Take every permission bit away. A process that may override the mode, or a
     filesystem that does not enforce it, still reads the file; the directory
     case below then carries the rule, and this one says it was skipped. *)
  Unix.chmod path 0o000;
  let readable =
    match In_channel.with_open_bin path In_channel.input_all with
    | _ -> true
    | exception Sys_error _ -> false
  in
  (if readable then
     Printf.printf
       "note: the permission case is skipped; a file with no permission bit is \
        still readable here\n\
        %!"
   else
     let loaded = Jide_engine.Project.load path in
     check "unreadable: one problem" (List.length (messages loaded) = 1);
     check "unreadable: the message says it cannot be read"
       (contains (one_message loaded) "cannot be read");
     check "unreadable: the file it came from" (loaded.path = Some path);
     check "unreadable: no text" (loaded.text = "");
     check "unreadable: the defaults apply"
       (loaded.project = Jide_engine.Project.empty);
     check "unreadable: the diagnostic reaches the reader"
       (match Jide_engine.Project.diagnostics ~uri:"file:///p" loaded with
       | [ diagnostic ] -> diagnostic.Jide_lang.Diagnostic.code = Some "project"
       | _ -> false));
  Unix.chmod path 0o600;
  (* A path that exists and cannot be read as a project file: a directory is
     the case no permission bit can make readable. *)
  let loaded = Jide_engine.Project.load directory in
  check "unreadable: a directory is one problem"
    (List.length (messages loaded) = 1);
  check "unreadable: a directory names itself" (loaded.path = Some directory);
  check "unreadable: a directory applies the defaults"
    (loaded.project = Jide_engine.Project.empty);
  Sys.remove path;
  Sys.rmdir directory

let test_the_engine_reports_an_unreadable_project () =
  let directory = Filename.temp_file "jide-project" "" in
  Sys.remove directory;
  Sys.mkdir directory 0o700;
  (* The workspace holds a [jasmin-project.json] that cannot be read as one. *)
  let path = Filename.concat directory Jide_engine.Project.file_name in
  Sys.mkdir path 0o700;
  let engine = Jide_engine.Engine.create () in
  Jide_engine.Engine.set_root engine (Some directory);
  check "engine unreadable: one diagnostic"
    (List.length (Jide_engine.Engine.project_diagnostics engine) = 1);
  check "engine unreadable: the defaults apply"
    (Jide_engine.Engine.settings engine = Jide_engine.Engine.default_settings);
  check "engine unreadable: the file is remembered"
    (match Jide_engine.Engine.project engine with
    | Some loaded -> loaded.Jide_engine.Project.path = Some path
    | None -> false);
  check "engine unreadable: a check still runs"
    (Jide_engine.Engine.open_document engine ~uri:"file:///tmp/c.jazz"
       ~version:1 ~text:"fn f(reg u32 a) -> reg u32 { return a; }";
     match Jide_engine.Engine.check engine ~uri:"file:///tmp/c.jazz" with
     | Some analysis -> analysis.Jide_engine.Engine.diagnostics = []
     | None -> false);
  Sys.rmdir path;
  Sys.rmdir directory

let test_a_file_on_disk () =
  let directory = Filename.temp_file "jide-project" "" in
  Sys.remove directory;
  Sys.mkdir directory 0o700;
  let path = Filename.concat directory Jide_engine.Project.file_name in
  let channel = open_out_bin path in
  output_string channel "{\"arch\": \"riscv\", \"include\": {\"A\": \"b\"}}";
  close_out channel;
  let loaded = Jide_engine.Project.load path in
  check "disk: no problem" (loaded.problems = []);
  check "disk: the file is remembered" (loaded.path = Some path);
  check "disk: the architecture"
    ((loaded.project : Jide_engine.Project.t).arch
    = Some (Option.get (Jide_jasmin.Adapter.arch_of_string "riscv")));
  check "disk: the include path is beside the file"
    ((loaded.project : Jide_engine.Project.t).idirs
    = [ ("A", Filename.concat directory "b") ]);
  Sys.remove path;
  Sys.rmdir directory

let test_the_engine_reads_the_project () =
  let directory = Filename.temp_file "jide-project" "" in
  Sys.remove directory;
  Sys.mkdir directory 0o700;
  let path = Filename.concat directory Jide_engine.Project.file_name in
  let channel = open_out_bin path in
  output_string channel
    "{\"arch\": \"arm-m4\", \"call_conv\": \"windows\", \"include\": {\"A\": \
     \"lib\"}}";
  close_out channel;
  let engine = Jide_engine.Engine.create () in
  Jide_engine.Engine.set_root engine (Some directory);
  let settings = Jide_engine.Engine.settings engine in
  check "engine: the architecture reaches the settings"
    (settings.Jide_engine.Engine.arch = arm_m4);
  check "engine: the calling convention reaches the settings"
    (settings.call_conv = windows);
  check "engine: the include path reaches the settings"
    (settings.idirs = [ ("A", Filename.concat directory "lib") ]);
  check "engine: no problem" (Jide_engine.Engine.project_diagnostics engine = []);
  check "engine: the file is remembered"
    (match Jide_engine.Engine.project engine with
    | Some loaded -> loaded.Jide_engine.Project.path = Some path
    | None -> false);
  (* Without a root the defaults are back. *)
  Jide_engine.Engine.set_root engine None;
  check "engine: no root, the defaults"
    (Jide_engine.Engine.settings engine = Jide_engine.Engine.default_settings);
  Sys.remove path;
  Sys.rmdir directory

let test_an_unknown_architecture_never_reaches_the_settings () =
  let directory = Filename.temp_file "jide-project" "" in
  Sys.remove directory;
  Sys.mkdir directory 0o700;
  let path = Filename.concat directory Jide_engine.Project.file_name in
  let channel = open_out_bin path in
  output_string channel "{\"arch\": \"mips\", \"call_conv\": \"plan9\"}";
  close_out channel;
  let engine = Jide_engine.Engine.create () in
  Jide_engine.Engine.set_root engine (Some directory);
  check "refused arch: the settings keep the default"
    (Jide_engine.Engine.settings engine = Jide_engine.Engine.default_settings);
  check "refused arch: two diagnostics"
    (List.length (Jide_engine.Engine.project_diagnostics engine) = 2);
  (* The check still runs, on the default architecture: an unknown name in the
     file must never reach the library's setters. *)
  Jide_engine.Engine.open_document engine ~uri:"file:///tmp/a.jazz" ~version:1
    ~text:"fn f(reg u32 a) -> reg u32 { return a; }";
  check "refused arch: a check still runs"
    (match Jide_engine.Engine.check engine ~uri:"file:///tmp/a.jazz" with
    | Some analysis -> analysis.Jide_engine.Engine.diagnostics = []
    | None -> false);
  Sys.remove path;
  Sys.rmdir directory

let test_the_include_paths_reach_the_checker () =
  let seen = ref [] in
  let check_captures ~arch:_ ~call_conv:_ ~file:_ ~text:_ ~idirs =
    seen := idirs;
    {
      Jide_jasmin.Adapter.diagnostics = [];
      warnings = [];
      facts = Jide_jasmin.Facts.empty;
    }
  in
  let directory = Filename.temp_file "jide-project" "" in
  Sys.remove directory;
  Sys.mkdir directory 0o700;
  let path = Filename.concat directory Jide_engine.Project.file_name in
  let channel = open_out_bin path in
  output_string channel "{\"include\": {\"LIB\": \"src\"}}";
  close_out channel;
  let engine = Jide_engine.Engine.create ~check:check_captures () in
  Jide_engine.Engine.set_root engine (Some directory);
  Jide_engine.Engine.open_document engine ~uri:"file:///tmp/b.jazz" ~version:1
    ~text:"fn f() -> reg u32 { return 0; }";
  ignore (Jide_engine.Engine.check engine ~uri:"file:///tmp/b.jazz");
  check "include paths: they reach the checker"
    (!seen = [ ("LIB", Filename.concat directory "src") ]);
  Sys.remove path;
  Sys.rmdir directory

let () =
  test_a_full_file ();
  test_a_minimal_file ();
  test_an_unknown_field ();
  test_a_bad_architecture ();
  test_a_bad_calling_convention ();
  test_a_bad_version ();
  test_a_bom ();
  test_not_json ();
  test_a_missing_file ();
  test_an_unreadable_file ();
  test_a_file_on_disk ();
  test_the_engine_reads_the_project ();
  test_the_engine_reports_an_unreadable_project ();
  test_an_unknown_architecture_never_reaches_the_settings ();
  test_the_include_paths_reach_the_checker ();
  if !failures = 0 then print_endline "all project tests passed"
  else (
    Printf.printf "%d project test(s) failed\n" !failures;
    exit 1)
