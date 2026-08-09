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
     let loaded = load file in
     check "unreadable by permission: one problem"
       (List.length (messages loaded) = 1);
     check "unreadable by permission: the file is remembered"
       (loaded.path = Some file);
     check "unreadable by permission: the defaults apply"
       (loaded.project = Jide_engine.Project.empty));
  Unix.chmod file 0o600

let test_repeated_include_accumulates () =
  let text =
    lines
      [
        "[jasmin]";
        "include = A = lib";
        "";
        "include = B = lib/b";
        "include = C = lib";
      ]
  in
  let root = make [ (file_name, Some text); ("lib", None); ("lib/b", None) ] in
  let loaded = load (path_of root file_name) in
  check "repeated include: no problem" (loaded.problems = []);
  check "repeated include: every binding, in file order, resolved"
    ((loaded.project : Jide_engine.Project.t).idirs
    = [
        ("A", path_of root "lib");
        ("B", path_of root "lib/b");
        ("C", path_of root "lib");
      ])

let test_a_repeated_entry_is_refused () =
  let text = lines [ "[jasmin]"; "entry = a.jazz"; "entry = b.jazz" ] in
  let root =
    make [ (file_name, Some text); ("a.jazz", Some ""); ("b.jazz", Some "") ]
  in
  let loaded = load (path_of root file_name) in
  check_problem "repeated entry" text loaded ~line:3 ~fragment:"entry"
    ~message:"entry is given twice (line 2, then line 3)";
  check "repeated entry: the first one stands"
    ((loaded.project : Jide_engine.Project.t).entries
    = [ path_of root "a.jazz" ])

let test_a_repeated_architecture_is_refused () =
  let text = lines [ "[jasmin]"; "arch = x86-64"; "arch = riscv" ] in
  let loaded = parse text in
  check_problem "repeated arch" text loaded ~line:3 ~fragment:"arch"
    ~message:"arch is given twice (line 2, then line 3)";
  check "repeated arch: the first one stands"
    ((loaded.project : Jide_engine.Project.t).arch = Some x86_64)

let test_comments_and_blank_lines () =
  let text =
    lines
      [
        "[jasmin]";
        "";
        "# arch = arm-m4";
        "; call_conv = windows";
        "";
        "  \t";
        "arch = x86-64";
        "";
      ]
  in
  let loaded = parse text in
  check "comments: no problem" (loaded.problems = []);
  let (project : Jide_engine.Project.t) = loaded.project in
  check "comments: the comment is not a key" (project.arch = Some x86_64);
  check "comments: a commented-out value is not read" (project.call_conv = None)

(* A comment is a whole line, so a [#] inside a value is part of the value: a
   path holding one stays usable, and there is no trailing comment to eat it. *)
let test_a_hash_inside_a_value () =
  let text = lines [ "[jasmin]"; "entry = a#b.jazz" ] in
  let loaded = parse text in
  check "hash in a value: no problem" (loaded.problems = []);
  check "hash in a value: the whole value"
    ((loaded.project : Jide_engine.Project.t).entries = [ "a#b.jazz" ])

let test_a_bom () =
  let text = "\xEF\xBB\xBF[jasmin]\narch = x86-64\n" in
  let loaded = parse text in
  check "bom: no problem" (loaded.problems = []);
  check "bom: three bytes removed" (loaded.bom = 3);
  check "bom: the text has no mark" (loaded.text = "[jasmin]\narch = x86-64\n");
  check "bom: the architecture is read"
    ((loaded.project : Jide_engine.Project.t).arch = Some x86_64);
  (* The diagnostic's span indexes the file, not the text the parser saw. *)
  let text = "\xEF\xBB\xBF[jasmin]\nnonsense = 1\n" in
  let loaded = parse text in
  check_messages "bom: the unknown key" loaded [ "unknown key: nonsense" ];
  check "bom: the span is three bytes further on"
    (one_span loaded = Option.get (span text "nonsense"))

let test_a_line_that_is_not_a_key () =
  let text = "this is not a project file\n" in
  let loaded = parse text in
  check_problem "not a key" text loaded ~line:1
    ~fragment:"this is not a project file" ~message:"expected key = value";
  check "not a key: the defaults apply"
    (loaded.project = Jide_engine.Project.empty)

let test_an_unknown_section () =
  let text = lines [ "[editor]"; "arch = x86-64" ] in
  let loaded = parse text in
  check_problem "unknown section" text loaded ~line:1 ~fragment:"editor"
    ~message:"unknown section: editor (only [jasmin] is read)";
  check "unknown section: the key inside it is not read"
    ((loaded.project : Jide_engine.Project.t).arch = None)

(* The section is optional: the file carries one project's settings, and a key
   written above the header belongs to it. *)
let test_the_section_is_optional () =
  let loaded = parse "arch = x86-64\n" in
  check "no section: no problem" (loaded.problems = []);
  check "no section: the key is read"
    ((loaded.project : Jide_engine.Project.t).arch = Some x86_64)

let test_crlf () =
  let text = "[jasmin]\r\narch = x86-64\r\n\r\ncall_conv = windows\r\n" in
  let loaded = parse text in
  check "crlf: no problem" (loaded.problems = []);
  let (project : Jide_engine.Project.t) = loaded.project in
  check "crlf: the architecture" (project.arch = Some x86_64);
  check "crlf: the calling convention" (project.call_conv = Some windows)

(* Paths are relative to the directory holding the file, never to the
   directory of the file being analysed: the project file here is two
   directories above the document. *)
let test_a_path_beside_a_nested_directory () =
  let root =
    make
      [
        (file_name, Some (lines [ "[jasmin]"; "include = SHARED = lib/shared" ]));
        ("lib/shared", None);
        ("src/deep", None);
      ]
  in
  let loaded = load_above (path_of root "src/deep") in
  check "nested: no problem" (loaded.problems = []);
  check "nested: the file above it" (loaded.path = Some (path_of root file_name));
  check "nested: the include path is the project file's own directory"
    ((loaded.project : Jide_engine.Project.t).idirs
    = [ ("SHARED", path_of root "lib/shared") ])

let test_the_walk_finds_a_file_three_directories_up () =
  let root =
    make
      [
        (file_name, Some (lines [ "[jasmin]"; "arch = riscv" ])); ("a/b/c", None);
      ]
  in
  let deep = path_of root "a/b/c" in
  check "walk: the file three directories up"
    (find_above deep = Some (path_of root file_name));
  let loaded = load_above deep in
  check "walk: its architecture, three directories up"
    ((loaded.project : Jide_engine.Project.t).arch = Some riscv)

let test_the_walk_stops_at_the_nearest () =
  let root =
    make
      [
        (file_name, Some (lines [ "[jasmin]"; "arch = arm-m4" ]));
        ("a/jasmin.project", Some (lines [ "[jasmin]"; "arch = riscv" ]));
        ("a/b/c", None);
      ]
  in
  let deep = path_of root "a/b/c" in
  check "nearest: the file in the directory above"
    (find_above deep = Some (path_of root "a/jasmin.project"));
  check "nearest: it stops at the nearest of the two"
    (((load_above deep).project : Jide_engine.Project.t).arch = Some riscv);
  check "nearest: the outer one is still there for its own tree"
    (((load_above root).project : Jide_engine.Project.t).arch = Some arm_m4)

(* A directory with no project file above it is the ordinary case. *)
let test_no_project_file_above () =
  let root = make [ ("lonely/deep", None) ] in
  let deep = path_of root "lonely/deep" in
  (match find_above deep with
  | None -> ()
  | Some path ->
      incr failures;
      Printf.printf "FAIL no file above: the walk found %s\n%!" path);
  let loaded = load_above deep in
  check "no file above: no problem" (loaded.problems = []);
  check "no file above: no file" (loaded.path = None);
  check "no file above: no text" (loaded.text = "");
  check "no file above: the defaults apply"
    (loaded.project = Jide_engine.Project.empty);
  check "no file above: no diagnostic"
    (Jide_engine.Project.diagnostics ~uri:"file:///p" loaded = [])

(* The engine, which is what a document is analysed under. *)

let test_the_engine_reads_the_project () =
  let root =
    make
      [
        ( file_name,
          Some
            (lines
               [
                 "[jasmin]";
                 "arch = arm-m4";
                 "call_conv = windows";
                 "include = A = lib";
               ]) );
        ("lib", None);
      ]
  in
  let path = path_of root file_name in
  let engine = Jide_engine.Engine.create () in
  Jide_engine.Engine.set_root engine (Some root);
  let settings = Jide_engine.Engine.settings engine in
  check "engine: the architecture reaches the settings"
    (settings.Jide_engine.Engine.arch = arm_m4);
  check "engine: the calling convention reaches the settings"
    (settings.call_conv = windows);
  check "engine: the include path reaches the settings"
    (settings.idirs = [ ("A", path_of root "lib") ]);
  check "engine: no problem" (Jide_engine.Engine.project_diagnostics engine = []);
  check "engine: the file is remembered"
    (match Jide_engine.Engine.project engine with
    | Some loaded -> loaded.Jide_engine.Project.path = Some path
    | None -> false);
  (* Without a root the defaults are back. *)
  Jide_engine.Engine.set_root engine None;
  check "engine: no root, the defaults"
    (Jide_engine.Engine.settings engine = Jide_engine.Engine.default_settings)

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
