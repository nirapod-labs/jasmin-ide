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

let temp_dir () =
  let path = Filename.temp_file "jide-mutation" "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  path

let rec remove_tree path =
  if not (Sys.file_exists path) then ()
  else if Sys.is_directory path then (
    Array.iter
      (fun entry -> remove_tree (Filename.concat path entry))
      (Sys.readdir path);
    Unix.rmdir path)
  else Sys.remove path

let write path text =
  let channel = open_out_bin path in
  output_string channel text;
  close_out channel

let program =
  String.concat "\n"
    [
      "param int N = 3;";
      "type word = u64;";
      "export";
      "fn f(reg u64 x) -> reg u64 {";
      "  reg u64 y;";
      "  global u64 g = 42;";
      "  g = 42;";
      "  y = x;";
      "  y += g;";
      "  return y;";
      "}";
      "fn h(reg u64 x) -> reg u64 {";
      "  return x;";
      "}";
      "";
    ]

let tokens text =
  match Jide_mutation.lex text with Some tokens -> tokens | None -> []

let names items =
  List.map
    (fun (item : Jide_mutation.item) -> item.kind ^ " " ^ item.name)
    items

