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

let write path text =
  let channel = open_out_bin path in
  output_string channel text;
  close_out channel

let read_disk path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> Some text
  | exception Sys_error _ -> None

(* A project written inline: the files are made, the test runs, the files go
   away again. *)
let with_directory files test =
  let directory = Filename.temp_file "jide-context" "" in
  Sys.remove directory;
  Sys.mkdir directory 0o700;
  let path name = Filename.concat directory name in
  List.iter (fun (name, text) -> write (path name) text) files;
  let result = test directory path in
  List.iter (fun (name, _) -> Sys.remove (path name)) files;
  Sys.rmdir directory;
  result

let uri_of path = Jide_lang.Uri.to_string (Jide_lang.Uri.of_path path)
let context = Jide_engine.Context.of_file

(* Three files: an entry that requires one included file, which requires
   another. *)
let entry_text = "require \"lib.jinc\"\nfn main() -> reg u32 { return 0; }\n"
let lib_text = "require \"inner.jinc\"\ntype word = u32\n"
let inner_text = "type byte = u8\n"

let three_files =
  [
    ("entry.jazz", entry_text);
    ("lib.jinc", lib_text);
    ("inner.jinc", inner_text);
  ]

let test_the_three_file_project () =
  with_directory three_files (fun _directory path ->
      let entries = [ path "entry.jazz" ] in
      let of_file file = context ~entries ~idirs:[] ~read:read_disk file in
      let entry = of_file (path "entry.jazz") in
      check "entry: checked alone" (Jide_engine.Context.is_alone entry);
      check "entry: no entry named" (Jide_engine.Context.entry entry = None);
      check "entry: an empty chain" (Jide_engine.Context.chain entry = []);
      check "entry: the path"
        (Jide_engine.Context.file entry = path "entry.jazz");
      let lib = of_file (path "lib.jinc") in
      check "lib: the entry that requires it"
        (Jide_engine.Context.entry lib = Some (path "entry.jazz"));
      check "lib: the chain from the entry"
        (Jide_engine.Context.chain lib = [ path "entry.jazz"; path "lib.jinc" ]);
      let inner = of_file (path "inner.jinc") in
      check "inner: the same entry"
        (Jide_engine.Context.entry inner = Some (path "entry.jazz"));
      check "inner: the chain through the middle file"
        (Jide_engine.Context.chain inner
        = [ path "entry.jazz"; path "lib.jinc"; path "inner.jinc" ]);
      check "inner: the description names the middle file"
        (contains
           (Jide_engine.Context.describe inner)
           ("required through " ^ path "lib.jinc"));
      check "lib: the description names only the entry"
        (Jide_engine.Context.describe lib
        = "checked in the context of " ^ path "entry.jazz");
      check "entry: the description says alone"
        (Jide_engine.Context.describe entry = "checked alone"))

let test_a_file_no_entry_requires () =
  with_directory
    (three_files @ [ ("lonely.jinc", "type lonely = u16\n") ])
    (fun _directory path ->
      let entries = [ path "entry.jazz" ] in
      let lonely =
        context ~entries ~idirs:[] ~read:read_disk (path "lonely.jinc")
      in
      check "lonely: checked alone" (Jide_engine.Context.is_alone lonely);
      check "lonely: an empty chain" (Jide_engine.Context.chain lonely = []))

let test_the_first_entry_wins () =
  with_directory
    [
      ( "first.jazz",
        "require \"shared.jinc\"\nfn first() -> reg u32 { return 0; }\n" );
      ( "second.jazz",
        "require \"shared.jinc\"\nfn second() -> reg u32 { return 1; }\n" );
      ("shared.jinc", "type shared = u32\n");
    ]
    (fun _directory path ->
      let entries = [ path "first.jazz"; path "second.jazz" ] in
      let shared =
        context ~entries ~idirs:[] ~read:read_disk (path "shared.jinc")
      in
      check "first entry: the one listed first"
        (Jide_engine.Context.entry shared = Some (path "first.jazz"));
      let entries = [ path "second.jazz"; path "first.jazz" ] in
      let shared =
        context ~entries ~idirs:[] ~read:read_disk (path "shared.jinc")
      in
      check "first entry: the order of the list decides"
        (Jide_engine.Context.entry shared = Some (path "second.jazz")))

let test_an_entry_list_decides_what_is_an_entry () =
  with_directory
    (three_files
    @ [
        ( "other.jazz",
          "require \"lib.jinc\"\nfn other() -> reg u32 { return 1; }\n" );
      ])
    (fun _directory path ->
      (* A .jazz file is an entry only while the project lists none, so with
         the entry listed the other one is not an entry and nothing requires
         it. *)
      let entries = [ path "entry.jazz" ] in
      let other =
        context ~entries ~idirs:[] ~read:read_disk (path "other.jazz")
      in
      check "listed entries: another .jazz is not one"
        (Jide_engine.Context.is_alone other);
      (* With no entry listed every .jazz file is one. *)
      let entries = [] in
      let other =
        context ~entries ~idirs:[] ~read:read_disk (path "other.jazz")
      in
      check "no listed entries: a .jazz file is an entry"
        (Jide_engine.Context.is_alone other);
      let lib = context ~entries ~idirs:[] ~read:read_disk (path "lib.jinc") in
      check "no listed entries: an included file is alone"
        (Jide_engine.Context.is_alone lib))

let test_an_include_path_name () =
  with_directory
    [
      ( "entry.jazz",
        "require \"LIB:deep.jinc\"\nfn main() -> reg u32 { return 0; }\n" );
    ]
    (fun directory path ->
      Sys.mkdir (Filename.concat directory "src") 0o700;
      write (path "src/deep.jinc") "type deep = u32\n";
      let entries = [ path "entry.jazz" ] in
      let idirs = [ ("LIB", Filename.concat directory "src") ] in
      let deep =
        context ~entries ~idirs ~read:read_disk (path "src/deep.jinc")
      in
      check "include path: the named directory finds it"
        (Jide_engine.Context.entry deep = Some (path "entry.jazz"));
      (* Without the name the requirement names a file beside the entry, so it
         is not the one under src. *)
      let deep =
        context ~entries ~idirs:[] ~read:read_disk (path "src/deep.jinc")
      in
      check "include path: no name, no context"
        (Jide_engine.Context.is_alone deep);
      Sys.remove (path "src/deep.jinc");
      Sys.rmdir (path "src"))

let test_comments_and_several_names () =
  check "require: one name"
    (Jide_engine.Context.requires_of_text "require \"a.jinc\"\n" = [ "a.jinc" ]);
  check "require: several names on one line"
    (Jide_engine.Context.requires_of_text "require \"a.jinc\" \"b.jinc\"\n"
    = [ "a.jinc"; "b.jinc" ]);
  check "require: a line comment hides one"
    (Jide_engine.Context.requires_of_text "// require \"a.jinc\"\n" = []);
  check "require: a block comment hides one"
    (Jide_engine.Context.requires_of_text "/* require \"a.jinc\" */\n" = []);
  check "require: a string is not a require"
    (Jide_engine.Context.requires_of_text
       "inline fn f() -> u32 { return \"require \\\"a.jinc\\\"\"; }"
    = []);
  check "require: an include name is kept as written"
    (Jide_engine.Context.requires_of_text "require \"LIB:a.jinc\"\n"
    = [ "LIB:a.jinc" ]);
  check "require: an escaped quote ends nothing"
    (Jide_engine.Context.requires_of_text "require \"a\\\"b.jinc\"\n"
    = [ "a\\\"b.jinc" ])

let test_a_cycle_ends () =
  with_directory
    [
      ("entry.jazz", "require \"a.jinc\"\nfn main() -> reg u32 { return 0; }\n");
      ("a.jinc", "require \"b.jinc\"\ntype a = u32\n");
      ("b.jinc", "require \"a.jinc\"\ntype b = u32\n");
    ]
    (fun _directory path ->
      let entries = [ path "entry.jazz" ] in
      let b = context ~entries ~idirs:[] ~read:read_disk (path "b.jinc") in
      check "cycle: the entry is still found"
        (Jide_engine.Context.entry b = Some (path "entry.jazz"));
      check "cycle: the chain ends at the file"
        (Jide_engine.Context.chain b
        = [ path "entry.jazz"; path "a.jinc"; path "b.jinc" ]))

let test_the_paths_are_normalised () =
  check "normalise: a relative path"
    (Jide_engine.Context.normalise "./a/./b/../c.jinc" = "a/c.jinc");
  check "normalise: an absolute path"
    (Jide_engine.Context.normalise "/a//b/../c.jinc" = "/a/c.jinc");
  check "normalise: above the root"
    (Jide_engine.Context.normalise "/../a.jinc" = "/a.jinc");
  check "normalise: two spellings are equal"
    (Jide_engine.Context.equal "/a/./b/../c.jinc" "/a/c.jinc");
  check "normalise: different files are not"
    (not (Jide_engine.Context.equal "/a/c.jinc" "/a/d.jinc"))

let test_a_relative_require () =
  with_directory [] (fun directory path ->
      Sys.mkdir (Filename.concat directory "src") 0o700;
      Sys.mkdir (Filename.concat directory "common") 0o700;
      write (path "src/entry.jazz")
        "require \"../common/lib.jinc\"\nfn main() -> reg u32 { return 0; }\n";
      write (path "common/lib.jinc") "type word = u32\n";
      let entries = [ path "src/entry.jazz" ] in
      let lib =
        context ~entries ~idirs:[] ~read:read_disk (path "common/lib.jinc")
      in
      check "relative require: the file beside the entry"
        (Jide_engine.Context.entry lib = Some (path "src/entry.jazz"));
      Sys.remove (path "src/entry.jazz");
      Sys.rmdir (path "src");
      Sys.remove (path "common/lib.jinc");
      Sys.rmdir (path "common"))

(* The engine's half: an open document gets its context from the project's
   entry list, and the client's own choice replaces that list. *)
let engine_with files test =
  with_directory files (fun directory path ->
      let engine = Jide_engine.Engine.create () in
      Jide_engine.Engine.set_root engine (Some directory);
      let open_and_check name text =
        let file = path name in
        Jide_engine.Engine.open_document engine ~uri:(uri_of file) ~version:1
          ~text;
        ignore (Jide_engine.Engine.check engine ~uri:(uri_of file));
        Jide_engine.Engine.context engine ~uri:(uri_of file)
      in
      test engine directory path open_and_check)

(* The project file the engine walks up to. The name comes from the interface,
   so a test cannot go on passing against a file the parser stopped reading. *)
let project_with_entry =
  (Jide_engine.Project.file_name, "[jasmin]\nentry = entry.jazz\n")

let test_the_engine_reads_the_project_entries () =
  engine_with (three_files @ [ project_with_entry ])
    (fun engine _directory path open_and_check ->
      check "engine: the project file is found"
        (match Jide_engine.Engine.project engine with
        | Some loaded ->
            loaded.Jide_engine.Project.path
            = Some (path Jide_engine.Project.file_name)
        | None -> false);
      let lib = open_and_check "lib.jinc" lib_text in
      check "engine: the project's entry"
        (match lib with
        | Some context ->
            Jide_engine.Context.entry context = Some (path "entry.jazz")
        | None -> false);
      let inner = open_and_check "inner.jinc" inner_text in
      check "engine: the context reaches the analysis"
        (match inner with
        | Some context -> not (Jide_engine.Context.is_alone context)
        | None -> false);
      check "engine: an unanalysed document has no context"
        (Jide_engine.Engine.context engine ~uri:(uri_of (path "absent.jinc"))
        = None))

let test_the_client_override_wins () =
  engine_with
    (three_files
    @ [
        ( "other.jazz",
          "require \"lib.jinc\"\nfn other() -> reg u32 { return 1; }\n" );
        project_with_entry;
      ])
    (fun engine _directory path open_and_check ->
      Jide_engine.Engine.set_context_entry engine (Some (path "other.jazz"));
      check "override: it is remembered"
        (Jide_engine.Engine.context_entry engine = Some (path "other.jazz"));
      let lib = open_and_check "lib.jinc" lib_text in
      check "override: the chosen entry wins"
        (match lib with
        | Some context ->
            Jide_engine.Context.entry context = Some (path "other.jazz")
        | None -> false);
      let entry = open_and_check "entry.jazz" entry_text in
      check "override: another .jazz file is not an entry now"
        (match entry with
        | Some context -> Jide_engine.Context.is_alone context
        | None -> false);
      Jide_engine.Engine.set_context_entry engine None;
      check "override: handed back to the project"
        (Jide_engine.Engine.context_entry engine = None);
      let lib = open_and_check "lib.jinc" lib_text in
      check "override: the project's list is back"
        (match lib with
        | Some context ->
            Jide_engine.Context.entry context = Some (path "entry.jazz")
        | None -> false);
      (* Changing the root drops the cached entry list with the project. *)
      Jide_engine.Engine.set_root engine None;
      check "no root: the analysis is dropped with the entries"
        (Jide_engine.Engine.analysis engine ~uri:(uri_of (path "lib.jinc"))
        = None))

let test_the_default_entry_set () =
  engine_with three_files (fun _engine directory path open_and_check ->
      Sys.mkdir (Filename.concat directory "deep") 0o700;
      Sys.mkdir (Filename.concat directory "deep/nested") 0o700;
      write
        (path "deep/nested/other.jazz")
        "fn other() -> reg u32 { return 1; }\n";
      let lib = open_and_check "lib.jinc" lib_text in
      check "default entries: the entry is found"
        (match lib with
        | Some context ->
            Jide_engine.Context.entry context = Some (path "entry.jazz")
        | None -> false);
      Sys.remove (path "deep/nested/other.jazz");
      Sys.rmdir (path "deep/nested");
      Sys.rmdir (path "deep"))

let () =
  test_the_three_file_project ();
  test_a_file_no_entry_requires ();
  test_the_first_entry_wins ();
  test_an_entry_list_decides_what_is_an_entry ();
  test_an_include_path_name ();
  test_comments_and_several_names ();
  test_a_cycle_ends ();
  test_the_paths_are_normalised ();
  test_a_relative_require ();
  test_the_engine_reads_the_project_entries ();
  test_the_client_override_wins ();
  test_the_default_entry_set ();
  if !failures = 0 then print_endline "all context tests passed"
  else (
    Printf.printf "%d context test(s) failed\n" !failures;
    exit 1)
