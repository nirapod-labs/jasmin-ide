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

let no_span = Jide_lang.Span.make ~start_byte:(-1) ~end_byte:(-1)

let one_span (loaded : Jide_engine.Project.loaded) =
  match loaded.problems with
  | [ problem ] -> problem.Jide_engine.Project.span
  | _ -> no_span

let check_messages name (loaded : Jide_engine.Project.loaded) expected =
  let actual = messages loaded in
  if actual <> expected then (
    incr failures;
    Printf.printf "FAIL %s: expected [%s], got [%s]\n%!" name
      (String.concat "; " expected)
      (String.concat "; " actual))

(* One problem, with exactly [message], covering [fragment] on [line]: a second
   problem, none, or a span elsewhere fails the test. *)
let check_problem name text (loaded : Jide_engine.Project.loaded) ~line
    ~fragment ~message =
  check_messages name loaded [ message ];
  check_span name text ~line fragment (one_span loaded)

let parse = Jide_engine.Project.parse
let load = Jide_engine.Project.load
let load_above = Jide_engine.Project.load_above
let find_above = Jide_engine.Project.find_above
let file_name = Jide_engine.Project.file_name
let path_of root name = Filename.concat root name
let arch name = Option.get (Jide_jasmin.Adapter.arch_of_string name)
let x86_64 = arch "x86-64"
let arm_m4 = arch "arm-m4"
let riscv = arch "riscv"
let linux = Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux")
let windows = Option.get (Jide_jasmin.Adapter.call_conv_of_string "windows")

(* The name the adapter gave an architecture, read back through the same table
   the project file is checked against, because the type is the library's and
   has no printer here. *)
let name_of_arch arch =
  match
    List.find_opt
      (fun name -> Jide_jasmin.Adapter.arch_of_string name = Some arch)
      Jide_jasmin.Adapter.arch_names
  with
  | Some name -> name
  | None -> "<an architecture the adapter does not know>"

let lines = String.concat "\n"

(* The files a test makes, as a relative path and its text; [None] makes a
   directory. Every root is removed when the tests end. *)
let roots = ref []

let write path text =
  let channel = open_out_bin path in
  output_string channel text;
  close_out channel

let rec remove_tree path =
  match Sys.is_directory path with
  | true ->
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path);
      Sys.rmdir path
  | false -> Sys.remove path
  | exception Sys_error _ -> ()

let make_directory path =
  if not (Sys.file_exists path) then Sys.mkdir path 0o700

let rec make_parents root path =
  let parent = Filename.dirname path in
  if (not (String.equal parent root)) && not (String.equal parent path) then (
    make_parents root parent;
    make_directory parent)

let make entries =
  let root = Filename.temp_file "jide-project" "" in
  Sys.remove root;
  Sys.mkdir root 0o700;
  roots := root :: !roots;
  List.iter
    (fun (name, content) ->
      let path = path_of root name in
      make_parents root path;
      match content with
      | Some text -> write path text
      | None -> make_directory path)
    entries;
  root

let cleanup () =
  List.iter remove_tree !roots;
  roots := []

(* The design's worked example, byte for byte, comments and blank lines
   included. Its tree holds [src/main.jazz], [src/shared] and [lib/x86_64],
   which is what makes the include paths and the entry checkable. *)
let worked_example =
  {|# crypto/jasmin.project

[jasmin]
# The target this project is written for. Values are the ones the linked
# compiler accepts; `jasminc -arch help` in a shell lists them.
arch = x86-64

call_conv = linux

# Where `from NAME require "…"` looks. Repeat the key to bind several names.
include = LIB = lib/x86_64
include = SHARED = src/shared

# The file whose environment a bare `.jinc` should be read in. Optional: a
# `.jazz` file opened on its own needs no entry.
entry = src/main.jazz
|}

let worked_example_tree =
  [
    (file_name, Some worked_example);
    ("src/main.jazz", Some "");
    ("src/primitives/aead.jazz", Some "");
    ("src/shared/common.jinc", Some "");
    ("lib/x86_64/intrinsics.jinc", Some "");
    ("lib/arm/intrinsics.jinc", Some "");
  ]

let test_the_worked_example () =
  let root = make worked_example_tree in
  let path = path_of root file_name in
  let loaded = load path in
  check "worked: the name the walk looks for"
    (String.equal file_name "jasmin.project");
  check "worked: no problem" (loaded.problems = []);
  check "worked: the file it came from" (loaded.path = Some path);
  check "worked: no byte-order mark" (loaded.bom = 0);
  check "worked: the text as written" (loaded.text = worked_example);
  let (project : Jide_engine.Project.t) = loaded.project in
  check "worked: the architecture" (project.arch = Some x86_64);
  check "worked: the calling convention" (project.call_conv = Some linux);
  check "worked: both include names, in file order, resolved"
    (project.idirs
    = [
        ("LIB", path_of root "lib/x86_64"); ("SHARED", path_of root "src/shared");
      ]);
  check "worked: the entry, resolved"
    (project.entries = [ path_of root "src/main.jazz" ]);
  check "worked: no diagnostic for the reader"
    (Jide_engine.Project.diagnostics ~uri:"file:///p" loaded = []);
  (* A file one directory down finds the same project, and one that sits two
     directories down finds it by walking up. *)
  let below = load_above (path_of root "src/primitives") in
  check "worked: found from a nested directory" (below.path = Some path);
  check "worked: the nested document gets the same record"
    (below.project = project)

let test_a_minimal_file () =
  let loaded = parse "" in
  check "empty: no problem" (loaded.problems = []);
  check "empty: nothing set" (loaded.project = Jide_engine.Project.empty);
  check "empty: no text" (loaded.text = "");
  check "empty: no byte-order mark" (loaded.bom = 0);
  let loaded = parse (lines [ "[jasmin]" ]) in
  check "section alone: no problem" (loaded.problems = []);
  check "section alone: nothing set" (loaded.project = Jide_engine.Project.empty)

let test_the_two_values () =
  let text = lines [ "[jasmin]"; "arch = arm-m4"; "call_conv = windows" ] in
  let loaded = parse text in
  check "values: no problem" (loaded.problems = []);
  let (project : Jide_engine.Project.t) = loaded.project in
  check "values: the architecture" (project.arch = Some arm_m4);
  check "values: the calling convention" (project.call_conv = Some windows)

(* The design's failure table, one test per row. *)

let test_an_unknown_architecture () =
  let text =
    lines
      [
        "[jasmin]";
        "# a commented-out key is not read";
        "arch = cortex-m4";
        "call_conv = linux";
      ]
  in
  let loaded = parse text in
  check_problem "bad arch" text loaded ~line:3 ~fragment:"cortex-m4"
    ~message:
      (Printf.sprintf "unknown architecture: cortex-m4 (known: %s)"
         (String.concat ", " Jide_jasmin.Adapter.arch_names));
  check "bad arch: every name the adapter knows is listed"
    (List.for_all
       (contains (one_message loaded))
       Jide_jasmin.Adapter.arch_names);
  let (project : Jide_engine.Project.t) = loaded.project in
  check "bad arch: no architecture is set" (project.arch = None);
  check "bad arch: the rest of the file is still read"
    (project.call_conv = Some linux)

let test_a_bad_calling_convention () =
  let text = lines [ "[jasmin]"; "call_conv = plan9" ] in
  let loaded = parse text in
  check_problem "bad call conv" text loaded ~line:2 ~fragment:"plan9"
    ~message:"unknown calling convention: plan9 (known: linux, windows)";
  check "bad call conv: nothing is set"
    ((loaded.project : Jide_engine.Project.t).call_conv = None)

let test_an_include_without_an_inner_equal () =
  let text = lines [ "[jasmin]"; "include = LIB" ] in
  let loaded = parse text in
  check_problem "include without an inner =" text loaded ~line:2 ~fragment:"LIB"
    ~message:"expected key = value";
  check "include without an inner =: nothing is bound"
    ((loaded.project : Jide_engine.Project.t).idirs = [])

let test_an_unknown_key () =
  let text = lines [ "[jasmin]"; "arch = x86-64"; "nonsense = 1" ] in
  let loaded = parse text in
  check_problem "unknown key" text loaded ~line:3 ~fragment:"nonsense"
    ~message:"unknown key: nonsense";
  check "unknown key: the known key still applies"
    ((loaded.project : Jide_engine.Project.t).arch = Some x86_64);
  (* The JSON schema's version field is gone with the JSON: it is a key this
     parser does not know, and a typo is why unknown keys are refused. *)
  let text = lines [ "[jasmin]"; "version = 1" ] in
  let loaded = parse text in
  check_problem "the version key is unknown" text loaded ~line:2
    ~fragment:"version" ~message:"unknown key: version"

let test_a_missing_entry () =
  let text = lines [ "[jasmin]"; "entry = src/missing.jazz" ] in
  let root = make [ (file_name, Some text) ] in
  let loaded = load (path_of root file_name) in
  check_problem "missing entry" text loaded ~line:2 ~fragment:"src/missing.jazz"
    ~message:"the entry file does not exist";
  check "missing entry: nothing is recorded"
    ((loaded.project : Jide_engine.Project.t).entries = [])

let test_an_include_that_is_not_a_directory () =
  let text = lines [ "[jasmin]"; "include = LIB = notes.txt" ] in
  let root = make [ (file_name, Some text); ("notes.txt", Some "a note") ] in
  let loaded = load (path_of root file_name) in
  check_problem "include that is a file" text loaded ~line:2
    ~fragment:"LIB = notes.txt" ~message:"include LIB is not a directory";
  check "include that is a file: nothing is bound"
    ((loaded.project : Jide_engine.Project.t).idirs = [])

let test_a_missing_file () =
  let root = make [] in
  let loaded = load (path_of root file_name) in
  check "missing: no problem" (loaded.problems = []);
  check "missing: no file" (loaded.path = None);
  check "missing: no text" (loaded.text = "");
  check "missing: the defaults apply"
    (loaded.project = Jide_engine.Project.empty);
  check "missing: no diagnostic"
    (Jide_engine.Project.diagnostics ~uri:"file:///p" loaded = [])

(* A file that is there and cannot be read is not the same thing as a file that
   is not there: the first is reported, the second is the ordinary case the
   test above pins. A directory is the case no permission bit can make
   readable. *)
let test_an_unreadable_file () =
  let root = make [ (file_name, None) ] in
  let path = path_of root file_name in
  let loaded = load path in
  check "unreadable: one problem" (List.length (messages loaded) = 1);
  check "unreadable: the message says it cannot be read"
    (contains (one_message loaded) "cannot be read");
  check "unreadable: the file it came from" (loaded.path = Some path);
  check "unreadable: no text" (loaded.text = "");
  check "unreadable: the defaults apply"
    (loaded.project = Jide_engine.Project.empty);
  check "unreadable: it reaches the reader as one error"
    (match Jide_engine.Project.diagnostics ~uri:"file:///p" loaded with
    | [ diagnostic ] ->
        diagnostic.Jide_lang.Diagnostic.code = Some "project"
        && diagnostic.Jide_lang.Diagnostic.severity = Jide_lang.Diagnostic.Error
    | _ -> false);
  (* A file whose permission bits are gone. A process that may override the
     mode, or a filesystem that does not enforce it, still reads it; the
     directory case above then carries the rule, and this one says it was
     skipped. *)
  let other =
    make [ (file_name, Some (lines [ "[jasmin]"; "arch = arm-m4" ])) ]
  in
  let file = path_of other file_name in
  Unix.chmod file 0o000;
  let readable =
    match In_channel.with_open_bin file In_channel.input_all with
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
