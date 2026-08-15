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

