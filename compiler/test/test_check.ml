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

let test_help () =
  check "help: -h" (Jide_check.Request.parse [ "-h" ] = Jide_check.Request.Help);
  check "help: --help with a file"
    (Jide_check.Request.parse [ "f.jazz"; "--help" ] = Jide_check.Request.Help);
  check "help: the text names the mode"
    (starts_with ~prefix:"usage: jasmin-lsp check" Jide_check.Request.usage)

let test_line_column () =
  let text = "abc\ndef\n" in
  check "position: first byte"
    (Jide_check.Report.line_column ~text
       (Jide_lang.Span.make ~start_byte:0 ~end_byte:1)
    = (1, 1));
  check "position: second line"
    (Jide_check.Report.line_column ~text
       (Jide_lang.Span.make ~start_byte:4 ~end_byte:5)
    = (2, 1));
  check "position: third byte of the second line"
    (Jide_check.Report.line_column ~text
       (Jide_lang.Span.make ~start_byte:6 ~end_byte:7)
    = (2, 3));
  check "position: past the end is the last line"
    (Jide_check.Report.line_column ~text
       (Jide_lang.Span.make ~start_byte:100 ~end_byte:200)
    = (3, 1));
  check "position: a negative offset is the first byte"
    (Jide_check.Report.line_column ~text
       (Jide_lang.Span.make ~start_byte:(-4) ~end_byte:1)
    = (1, 1));
  let utf8 = "é\nx" in
  check "position: columns count bytes"
    (Jide_check.Report.line_column ~text:utf8
       (Jide_lang.Span.make ~start_byte:3 ~end_byte:4)
    = (2, 1))

let test_escape () =
  check "escape: one line"
    (Jide_check.Report.escape "a\nb\tc\\d\r" = "a\\nb\\tc\\\\d\\r");
  check "escape: unchanged" (Jide_check.Report.escape "plain" = "plain")

let test_kind () =
  let diagnostic =
    Jide_lang.Diagnostic.make ~code:"typing"
      ~severity:Jide_lang.Diagnostic.Error
      (Jide_lang.Span.make ~start_byte:0 ~end_byte:0)
      "message"
  in
  check "kind: the code" (Jide_check.Report.kind diagnostic = "typing");
  let bare =
    Jide_lang.Diagnostic.make ~severity:Jide_lang.Diagnostic.Warning
      (Jide_lang.Span.make ~start_byte:0 ~end_byte:0)
      "message"
  in
  check "kind: a warning without a code"
    (Jide_check.Report.kind bare = "warning");
  let internal =
    Jide_lang.Diagnostic.make ~severity:Jide_lang.Diagnostic.Error
      (Jide_lang.Span.make ~start_byte:0 ~end_byte:0)
      "message"
  in
  check "kind: an error without a code"
    (Jide_check.Report.kind internal = "internal");
  check "kind: the internal helper"
    (Jide_check.Report.kind (Jide_check.Report.internal "boom") = "internal")

let test_summary () =
  let error = error_diagnostic "an error" in
  let warning =
    Jide_lang.Diagnostic.make ~code:"warning"
      ~severity:Jide_lang.Diagnostic.Warning
      (Jide_lang.Span.make ~start_byte:0 ~end_byte:1)
      "a warning"
  in
  check "summary: no diagnostic"
    (Jide_check.Report.summary ~file:"f" [] = "f: ok");
  check "summary: a warning only"
    (Jide_check.Report.summary ~file:"f" [ warning ] = "f: ok");
  check "summary: one error"
    (Jide_check.Report.summary ~file:"f" [ error ] = "f: 1 error");
  check "summary: two errors"
    (Jide_check.Report.summary ~file:"f" [ error; error ] = "f: 2 errors")

let test_line () =
  let text = "abc\ndef\n" in
  let diagnostic =
    Jide_lang.Diagnostic.make ~code:"typing"
      ~severity:Jide_lang.Diagnostic.Error
      (Jide_lang.Span.make ~start_byte:4 ~end_byte:6)
      "some\nmessage"
  in
  check "line: the whole line"
    (Jide_check.Report.line ~file:"f.jazz" ~text diagnostic
    = "f.jazz:2:1: typing: some\\nmessage")

let test_warning_span () =
  let text = "abc\n  def\n" in
  let diagnostics =
    Jide_check.Report.of_check ~file:"f.jazz" ~text
      {
        Jide_jasmin.Adapter.diagnostics = [];
        warnings =
          [ warning ~file:"f.jazz" ~line:2 ~start_column:4 ~end_column:5 "x" ];
        facts = Jide_jasmin.Facts.empty;
        required = [];
      }
  in
  (match diagnostics with
  | [ diagnostic ] ->
      check "warning: severity"
        (diagnostic.severity = Jide_lang.Diagnostic.Warning);
      check "warning: code" (diagnostic.code = Some "warning");
      check "warning: span from the line start and the column"
        (diagnostic.span = Jide_lang.Span.make ~start_byte:8 ~end_byte:9)
  | other ->
      check
        ("warning: one diagnostic, got " ^ string_of_int (List.length other))
        false);
  let no_location =
    Jide_check.Report.of_check ~file:"f.jazz" ~text
      {
        Jide_jasmin.Adapter.diagnostics = [];
        warnings = [ warning ~file:"f.jazz" ~line:0 "y" ];
        facts = Jide_jasmin.Facts.empty;
        required = [];
      }
  in
  (match no_location with
  | [ diagnostic ] ->
      check "warning: no location sits at the first byte"
        (diagnostic.span = Jide_lang.Span.make ~start_byte:0 ~end_byte:0)
  | _ -> check "warning: no location is one diagnostic" false);
  let clamped =
    Jide_check.Report.of_check ~file:"f.jazz" ~text
      {
        Jide_jasmin.Adapter.diagnostics = [];
        warnings =
          [
            warning ~file:"f.jazz" ~line:1 ~start_column:100 ~end_column:200 "z";
          ];
        facts = Jide_jasmin.Facts.empty;
        required = [];
      }
  in
  match clamped with
  | [ diagnostic ] ->
      check "warning: a column past the line is clamped"
        (diagnostic.span = Jide_lang.Span.make ~start_byte:3 ~end_byte:3)
  | _ -> check "warning: clamped is one diagnostic" false

let test_diagnostic_order () =
  let text = "abc\n" in
  let result =
    {
      Jide_jasmin.Adapter.diagnostics =
        [
          Jide_lang.Diagnostic.make ~code:"typing"
            ~severity:Jide_lang.Diagnostic.Error
            (Jide_lang.Span.make ~start_byte:2 ~end_byte:3)
            "second";
          Jide_lang.Diagnostic.make ~code:"typing"
            ~severity:Jide_lang.Diagnostic.Error
            (Jide_lang.Span.make ~start_byte:0 ~end_byte:1)
            "first";
        ];
      warnings =
        [
          warning ~file:"f.jazz" ~line:1 ~start_column:0 ~end_column:1 "warning";
          warning ~file:"other.jazz" ~line:1 "elsewhere";
        ];
      facts = Jide_jasmin.Facts.empty;
      required = [];
    }
  in
  let diagnostics = Jide_check.Report.of_check ~file:"f.jazz" ~text result in
  check "order: errors first in source order, then warnings of this file"
    (List.map
       (fun (diagnostic : Jide_lang.Diagnostic.t) -> diagnostic.message)
       diagnostics
    = [ "first"; "second"; "warning" ])

let test_check_valid () =
  let valid =
    "fn f(reg u32 a) -> reg u32 {\n  reg u32 b;\n  b = a;\n  return b;\n}\n"
  in
  let path = write_temp_file ~suffix:".jazz" valid in
  let code, out, err = run [ path ] in
  check "valid: exit code 0" (code = 0);
  check "valid: one summary line" (out = [ path ^ ": ok" ]);
  check "valid: nothing on stderr" (err = []);
  remove_file path

let test_check_type_error () =
  let broken =
    "fn f(reg u32 a) -> reg u32 {\n  reg bool b;\n  b = a;\n  return a;\n}\n"
  in
  let path = write_temp_file ~suffix:".jazz" broken in
  let code, out, _err = run [ path ] in
  check "type error: exit 1" (code = 1);
  (match out with
  | first :: summary ->
      check "type error: the line starts with the file and a position"
        (starts_with ~prefix:(path ^ ":3:7: typing: ") first);
      check "type error: the summary counts it"
        (summary = [ path ^ ": 1 error" ])
  | [] -> check "type error: some output" false);
  remove_file path

let test_check_syntax_error () =
  let broken = "fn f(reg u32 a -> reg u32 {\n  return a;\n}\n" in
  let path = write_temp_file ~suffix:".jazz" broken in
  let code, out, _err = run [ path ] in
  check "syntax error: exit 1" (code = 1);
  (match out with
  | first :: _ ->
      check "syntax error: kind parse"
        (starts_with ~prefix:(path ^ ":1:") first && contains first ": parse: ")
  | [] -> check "syntax error: some output" false);
  remove_file path

let test_check_warning () =
  let warned =
    "fn f(reg u32 a) -> reg u32 {\n  reg u32 unused;\n  return a;\n}\n"
  in
  let path = write_temp_file ~suffix:".jazz" warned in
  let code, out, _err = run [ path ] in
  check "warning: exit code 0" (code = 0);
  check "warning: the last line is ok" (last out = path ^ ": ok");
  check "warning: a warning line is printed"
    (List.exists
       (fun line -> line = path ^ ":2:11: warning: unused variable unused")
       out);
  remove_file path

let test_check_missing_file () =
  let path = Filename.temp_file "jide-check" ".jazz" in
  remove_file path;
  let code, out, _err = run [ path ] in
  check "missing file: exit 1" (code = 1);
  match out with
  | first :: summary ->
      check "missing file: an internal error at the first byte"
        (starts_with
           ~prefix:(path ^ ":1:1: internal: cannot read the file: ")
           first);
      check "missing file: the summary counts it"
        (summary = [ path ^ ": 1 error" ])
  | [] -> check "missing file: some output" false

let test_run_usage () =
  let code, out, err = run [ "--nope" ] in
  check "usage: exit 2" (code = 2);
  check "usage: nothing on standard output" (out = []);
  check "usage: the reason on standard error"
    (List.exists
       (fun line -> starts_with ~prefix:"jasmin-lsp check: unknown option" line)
       err);
  let code, out, _err = run [ "--help" ] in
  check "help: exit code 0" (code = 0);
  check "help: the usage on standard output"
    (match out with
    | [ text ] -> starts_with ~prefix:"usage: jasmin-lsp check" text
    | _ -> false)

let test_two_files () =
  let valid = "fn f(reg u32 a) -> reg u32 {\n  return a;\n}\n" in
  let broken =
    "fn g(reg u32 a) -> reg u32 {\n  reg bool b;\n  b = a;\n  return a;\n}\n"
  in
  let good = write_temp_file ~suffix:".jazz" valid in
  let bad = write_temp_file ~suffix:".jazz" broken in
  let code, out, _err = run [ good; bad ] in
  check "two files: exit 1 when one has an error" (code = 1);
  check "two files: the clean one is reported first, then the other"
    (match out with
    | first :: _ -> first = good ^ ": ok" && last out = bad ^ ": 1 error"
    | [] -> false);
  remove_file good;
  remove_file bad

let () =
  test_defaults ();
  test_options ();
  test_includes ();
  test_include_path ();
  test_usage_errors ();
  test_help ();
  test_line_column ();
  test_escape ();
  test_kind ();
  test_summary ();
  test_line ();
  test_warning_span ();
  test_diagnostic_order ();
  test_check_valid ();
  test_check_type_error ();
  test_check_syntax_error ();
  test_check_warning ();
  test_check_missing_file ();
  test_run_usage ();
  test_two_files ();
  if !failures = 0 then print_endline "all check tests passed"
  else (
    Printf.printf "%d check test(s) failed\n" !failures;
    exit 1)
