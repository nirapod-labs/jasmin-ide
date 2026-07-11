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

let error_diagnostic message =
  Jide_lang.Diagnostic.make ~code:"parse" ~severity:Jide_lang.Diagnostic.Error
    (Jide_lang.Span.make ~start_byte:0 ~end_byte:1)
    message

let warning ?(file = "") ?(line = 1) ?(start_column = 0) ?(end_column = 0)
    message =
  { Jide_jasmin.Warnings.file; line; start_column; end_column; message }

let test_defaults () =
  match Jide_check.Request.parse [ "a.jazz"; "b.jazz" ] with
  | Jide_check.Request.Run request ->
      check "defaults: architecture" (request.arch = x86_64);
      check "defaults: calling convention" (request.call_conv = linux);
      check "defaults: no include path" (request.idirs = []);
      check "defaults: the files in order"
        (request.files = [ "a.jazz"; "b.jazz" ])
  | _ -> check "defaults: a complete command line" false

let test_options () =
  (match
     Jide_check.Request.parse
       [ "--arch"; "arm-m4"; "--call-conv"; "windows"; "f.jazz" ]
   with
  | Jide_check.Request.Run request ->
      check "options: arch"
        (request.arch = Option.get (Jide_jasmin.Adapter.arch_of_string "arm-m4"));
      check "options: call conv"
        (request.call_conv
        = Option.get (Jide_jasmin.Adapter.call_conv_of_string "windows"))
  | _ -> check "options: separated values" false);
  match Jide_check.Request.parse [ "--arch=riscv"; "f.jazz" ] with
  | Jide_check.Request.Run request ->
      check "options: attached arch"
        (request.arch = Option.get (Jide_jasmin.Adapter.arch_of_string "riscv"))
  | _ -> check "options: attached arch parses" false

let test_includes () =
  (match
     Jide_check.Request.parse [ "-I"; "SECP=src"; "-IARCH=src/x86"; "f.jazz" ]
   with
  | Jide_check.Request.Run request ->
      check "includes: both pairs"
        (request.idirs = [ ("SECP", "src"); ("ARCH", "src/x86") ])
  | _ -> check "includes: separated pairs" false);
  match Jide_check.Request.parse [ "-ISECP=src"; "f.jazz" ] with
  | Jide_check.Request.Run request ->
      check "includes: attached pair" (request.idirs = [ ("SECP", "src") ])
  | _ -> check "includes: attached pair parses" false

let test_include_path () =
  (* A library that only the named include path makes reachable: the same
     source has to fail without -I and pass with it. *)
  let dir = make_directory () in
  let lib = Filename.concat dir "lib" in
  Sys.mkdir lib 0o700;
  let library = Filename.concat lib "inc.jazz" in
  write_file library
    (String.concat "\n"
       [
         "fn inc(reg u32 x) -> reg u32 {";
         "  reg u32 y;";
         "  y = x + 1;";
         "  return y;";
         "}";
         "";
       ]);
  let source = Filename.concat dir "main.jazz" in
  write_file source
    (String.concat "\n"
       [
         "from LIB require \"inc.jazz\"";
         "";
         "fn main(reg u32 a) -> reg u32 {";
         "  reg u32 b;";
         "  b = inc(a);";
         "  return b;";
         "}";
         "";
       ]);
  let code, out, _err = run [ "-I"; "LIB=" ^ lib; source ] in
  check "include path: exit code 0 with -I" (code = 0);
  check "include path: ok with -I" (last out = source ^ ": ok");
  let code, out, _err = run [ source ] in
  check "include path: exit 1 without -I" (code = 1);
  (match out with
  | first :: _ ->
      check "include path: the include name is unknown without -I"
        (starts_with ~prefix:(source ^ ":1:6: typing: unknown name LIB") first)
  | [] -> check "include path: some output" false);
  remove_file source;
  remove_file library;
  Sys.rmdir lib;
  Sys.rmdir dir

let test_usage_errors () =
  let invalid name arguments =
    match Jide_check.Request.parse arguments with
    | Jide_check.Request.Invalid _ -> ()
    | _ -> check ("invalid command line: " ^ name) false
  in
  invalid "unknown architecture" [ "--arch"; "mips"; "f.jazz" ];
  invalid "unknown call conv" [ "--call-conv"; "plan9"; "f.jazz" ];
  invalid "unknown option" [ "--nope"; "f.jazz" ];
  invalid "no file" [ "--arch"; "x86-64" ];
  invalid "include without name" [ "-I"; "src"; "f.jazz" ];
  invalid "empty include name" [ "-I"; "=src"; "f.jazz" ];
  invalid "two equals signs" [ "-I"; "A=src=more"; "f.jazz" ];
  invalid "missing arch value" [ "--arch" ];
  (match Jide_check.Request.parse [ "--arch"; "mips"; "f.jazz" ] with
  | Jide_check.Request.Invalid message ->
      check "usage error names the architecture"
        (starts_with ~prefix:"unknown architecture: mips" message)
  | _ -> ());
  match Jide_check.Request.parse [ "--"; "-odd.jazz" ] with
  | Jide_check.Request.Run request ->
      check "after -- a dash starts a file" (request.files = [ "-odd.jazz" ])
  | _ -> check "after -- parses" false

