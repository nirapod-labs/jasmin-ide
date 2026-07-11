let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let starts_with ~prefix text =
  String.length text >= String.length prefix
  && String.sub text 0 (String.length prefix) = prefix

let contains text needle =
  let n = String.length text and m = String.length needle in
  let rec loop index =
    if m > n - index then false
    else if String.sub text index m = needle then true
    else loop (index + 1)
  in
  loop 0

let last = function
  | [] -> ""
  | first :: rest -> List.fold_left (fun _ line -> line) first rest

let write_temp_file ~suffix text =
  let path = Filename.temp_file "jide-check" suffix in
  let channel = open_out_bin path in
  output_string channel text;
  close_out channel;
  path

let remove_file path = if Sys.file_exists path then Sys.remove path

let write_file path text =
  let channel = open_out_bin path in
  output_string channel text;
  close_out channel

(** A fresh directory, with the temporary file of that name removed first. *)
let make_directory () =
  let path = Filename.temp_file "jide-check" "" in
  Sys.remove path;
  Sys.mkdir path 0o700;
  path

let x86_64 = Option.get (Jide_jasmin.Adapter.arch_of_string "x86-64")
let linux = Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux")

let run arguments =
  let out = ref [] and err = ref [] in
  let code =
    Jide_check.Cli.run
      ~out:(fun line -> out := line :: !out)
      ~err:(fun line -> err := line :: !err)
      arguments
  in
  (code, List.rev !out, List.rev !err)

