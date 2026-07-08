let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let contains haystack needle =
  let n = String.length haystack and m = String.length needle in
  let rec loop index =
    if m = 0 then true
    else if index + m > n then false
    else String.sub haystack index m = needle || loop (index + 1)
  in
  loop 0

let write_temp_file ~suffix text =
  let path = Filename.temp_file "jide-adapter" suffix in
  let channel = open_out_bin path in
  output_string channel text;
  close_out channel;
  path

let remove_file path = if Sys.file_exists path then Sys.remove path
let x86_64 = Option.get (Jide_jasmin.Adapter.arch_of_string "x86-64")
let linux = Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux")

let check_text name text =
  Jide_jasmin.Adapter.check x86_64 linux ~file:"/tmp/probe.jazz" ~text ~idirs:[]

module X86 =
  (val Jasmin.CoreArchFactory.get_arch_module Jasmin.Utils.X86_64
         Jasmin.Glob_options.Linux
      : Jasmin.Arch_full.Arch)

module Loop = Jide_jasmin.Check.Make (X86)

let valid =
  {|
fn f(reg u32 a) -> reg u32 {
  reg u32 b;
  b = a;
  return b;
}
|}

let with_syntax_error =
  {|
fn f(reg u32 a -> reg u32 {
  reg u32 b;
  return b;
}
|}

let with_type_error =
  {|
fn f(reg u32 a) -> reg u32 {
  reg bool b;
  b = a;
  return a;
}
|}

let with_unused_variable =
  {|
fn f(reg u32 a) -> reg u32 {
  reg u32 unused;
  return a;
}
|}

let with_deprecated_cast =
  {|
fn f(reg u32 a) -> reg u32 {
  reg int i;
  i = (int)a;
  return a;
}
|}

let first_item_fails =
  {|
fn first(reg u32 a) -> reg u32 {
  reg bool b;
  b = a;
  return a;
}

fn second(reg u32 a) -> reg u32 {
  reg u32 x;
  x = a;
  return x;
}
|}

let library_with_unused_variable =
  {|
fn library(reg u32 a) -> reg u32 {
  reg u32 unused;
  return a;
}
|}

let library_with_type_error =
  {|
fn library(reg u32 a) -> reg u32 {
  reg bool b;
  b = a;
  return a;
}
|}

let requiring path =
  "require \"" ^ path
  ^ "\"\n\nfn entry(reg u32 a) -> reg u32 {\n  return a;\n}\n"

let location file line start_column end_column =
  {
    Jasmin.Location.loc_fname = file;
    loc_start = (line, start_column);
    loc_end = (line, end_column);
    loc_bchar = 0;
    loc_echar = 1;
  }

let print_warning ?(stack = []) base =
  let location = Jasmin.Location.i_loc base stack in
  Format.eprintf "@[<v>%a@ warning: unused variable x.7@]@."
    Jasmin.Location.pp_iloc location

let test_valid () =
  let result = check_text "valid" valid in
  check "valid: no diagnostics" (result.Jide_jasmin.Adapter.diagnostics = []);
  check "valid: no warnings" (result.warnings = [])

let test_syntax_error () =
  let result = check_text "syntax error" with_syntax_error in
  check "syntax error: one diagnostic" (List.length result.diagnostics = 1);
  match result.diagnostics with
  | [ diagnostic ] ->
      check "syntax error: code"
        (diagnostic.Jide_lang.Diagnostic.code = Some "parse");
      check "syntax error: non-empty message"
        (String.length diagnostic.message > 0);
      check "syntax error: span inside the text"
        (diagnostic.span.Jide_lang.Span.end_byte
        <= String.length with_syntax_error)
  | _ -> check "syntax error: shape" false

let test_type_error () =
  let result = check_text "type error" with_type_error in
  check "type error: at least one diagnostic"
    (List.length result.diagnostics >= 1);
  match result.diagnostics with
  | diagnostic :: _ ->
      check "type error: code"
        (diagnostic.Jide_lang.Diagnostic.code = Some "typing");
      check "type error: points at the assignment"
        (diagnostic.span.Jide_lang.Span.start_byte > 0)
  | [] -> check "type error: shape" false

let test_continue_after_failed_item () =
  let file = "/tmp/probe.jazz" in
  let parsed = Jide_jasmin.Parse.parse ~name:file first_item_fails in
  check "continue: the fixture parses" (not (Jide_jasmin.Parse.failed parsed));
  let result, environment =
    Loop.type_items ~file ~items:parsed.items ~idirs:[]
  in
  check "continue: only the failed item produces a diagnostic"
    (List.length result.Jide_jasmin.Check.diagnostics = 1);
  (match result.diagnostics with
  | [ diagnostic ] ->
      check "continue: the diagnostic is a type error"
        (diagnostic.Jide_lang.Diagnostic.code = Some "typing")
  | _ -> ());
  match environment with
  | Some environment ->
      check "continue: the second item is declared in the environment"
        (Jasmin.Pretyping.Env.Funs.find "second" environment <> None);
      check "continue: the failed item is not declared in the environment"
        (Jasmin.Pretyping.Env.Funs.find "first" environment = None)
  | None -> check "continue: the loop leaves an environment behind" false

let test_capture () =
  let base = location "f.jazz" 3 4 5 in
  let (), text =
    Jide_jasmin.Warnings.with_capture (fun () -> print_warning base)
  in
  check "capture: text is captured" (String.length text > 0);
  match Jide_jasmin.Warnings.parse text with
  | [ warning ] ->
      check "capture: file" (warning.Jide_jasmin.Warnings.file = "f.jazz");
      check "capture: line" (warning.line = 3);
      check "capture: start column" (warning.start_column = 4);
      check "capture: end column" (warning.end_column = 5);
      check "capture: uid suffix stripped"
        (warning.message = "unused variable x")
  | other ->
      check
        ("capture: one warning, got " ^ string_of_int (List.length other))
        false

let test_single_line_warning () =
  let text = "\"f.jazz\", line 3 (4-5): warning: unused variable x.7\n" in
  match Jide_jasmin.Warnings.parse text with
  | [ warning ] ->
      check "single line: file" (warning.Jide_jasmin.Warnings.file = "f.jazz");
      check "single line: line" (warning.line = 3);
      check "single line: start column" (warning.start_column = 4);
      check "single line: end column" (warning.end_column = 5);
      check "single line: uid suffix stripped"
        (warning.message = "unused variable x")
  | other ->
      check
        ("single line: one warning, got " ^ string_of_int (List.length other))
        false

let test_quoted_location () =
  let two_line =
    "\"a.jazz\", line 2 (3-4)\n"
    ^ "warning: the variable x is already declared at \"/tmp/other.jazz\", \
       line 77 (9-10)\n"
  in
  (match Jide_jasmin.Warnings.parse two_line with
  | [ warning ] ->
      check "quoted location: the file does not move"
        (warning.Jide_jasmin.Warnings.file = "a.jazz");
      check "quoted location: the line does not move" (warning.line = 2);
      check "quoted location: the columns do not move"
        (warning.start_column = 3 && warning.end_column = 4);
      check "quoted location: the message is whole"
        (warning.message
       = "the variable x is already declared at \"/tmp/other.jazz\", line 77 \
          (9-10)")
  | other ->
      check
        ("quoted location: one warning, got "
        ^ string_of_int (List.length other))
        false);
  let message_only = "warning: see \"/tmp/o.jazz\", line 77 (3-4)\n" in
  match Jide_jasmin.Warnings.parse ~file:"cur.jazz" message_only with
  | [ warning ] ->
      check "quoted location: a message supplies no location"
        (warning.Jide_jasmin.Warnings.file = "cur.jazz" && warning.line = 0);
      check "quoted location: the quoted text stays in the message"
        (warning.message = "see \"/tmp/o.jazz\", line 77 (3-4)")
  | other ->
      check
        ("quoted location: the message-only warning, got "
        ^ string_of_int (List.length other))
        false

let test_stacked_location () =
  let check_first name text =
    match Jide_jasmin.Warnings.parse text with
    | [ warning ] ->
        check
          (name ^ ": the first file is kept")
          (warning.Jide_jasmin.Warnings.file = "a.jazz");
        check (name ^ ": the first line is kept") (warning.line = 3);
        check
          (name ^ ": the first columns are kept")
          (warning.start_column = 4 && warning.end_column = 5);
        check (name ^ ": the message") (warning.message = "unused variable x")
    | other ->
        check
          (name ^ ": one warning, got " ^ string_of_int (List.length other))
          false
  in
  check_first "stacked"
    "\"a.jazz\", line 3 (4-5)\n\
    \ from \"b.jazz\", line 2 (1-3)\n\
     warning: unused variable x.7\n";
  check_first "stacked without file"
    "\"a.jazz\", line 3 (4-5)\n from line 9 (2)\nwarning: unused variable x.7\n";
  let base = location "a.jazz" 3 4 5 in
  let upper = location "b.jazz" 2 1 3 in
  let (), printed =
    Jide_jasmin.Warnings.with_capture (fun () ->
        print_warning ~stack:[ upper ] base)
  in
  check_first "stacked as printed" printed

let test_warning () =
  let result = check_text "unused variable" with_unused_variable in
  check "warning: file names the probe"
    (List.exists
       (fun (warning : Jide_jasmin.Warnings.t) ->
         String.length warning.file > 0 && warning.line > 0)
       result.warnings);
  check "warning: message mentions the variable"
    (List.exists
       (fun (warning : Jide_jasmin.Warnings.t) ->
         contains (String.lowercase_ascii warning.message) "unused")
       result.warnings)

let test_parse_warning () =
  let result = check_text "deprecated cast" with_deprecated_cast in
  check "parse warning: the program has no diagnostics"
    (result.Jide_jasmin.Adapter.diagnostics = []);
  check "parse warning: the warning the parser prints is captured"
    (List.exists
       (fun (warning : Jide_jasmin.Warnings.t) ->
         contains (String.lowercase_ascii warning.message) "deprecated"
         && warning.Jide_jasmin.Warnings.file = "/tmp/probe.jazz")
       result.warnings)

let test_required_file () =
  let library = write_temp_file ~suffix:".jinc" library_with_unused_variable in
  let result = check_text "required file" (requiring library) in
  check "required file: the document has no diagnostics"
    (result.Jide_jasmin.Adapter.diagnostics = []);
  check "required file: the warning names the file it comes from"
    (List.exists
       (fun (warning : Jide_jasmin.Warnings.t) ->
         warning.Jide_jasmin.Warnings.file = library)
       result.warnings);
  check "required file: no warning is put on the document"
    (List.for_all
       (fun (warning : Jide_jasmin.Warnings.t) ->
         warning.Jide_jasmin.Warnings.file <> "/tmp/probe.jazz")
       result.warnings);
  remove_file library

let test_required_file_error () =
  let library = write_temp_file ~suffix:".jinc" library_with_type_error in
  let result = check_text "required error" (requiring library) in
  check "required error: the diagnostic is not put on the document"
    (result.Jide_jasmin.Adapter.diagnostics = []);
  let direct =
    Jide_jasmin.Adapter.check x86_64 linux ~file:library
      ~text:library_with_type_error ~idirs:[]
  in
  check "required error: the error itself is real"
    (List.length direct.diagnostics = 1);
  remove_file library

let test_arch_names () =
  check "arch names"
    (Jide_jasmin.Adapter.arch_names = [ "x86-64"; "arm-m4"; "armv8a"; "riscv" ]);
  check "unknown arch" (Jide_jasmin.Adapter.arch_of_string "mips" = None);
  check "unknown call conv"
    (Jide_jasmin.Adapter.call_conv_of_string "plan9" = None)

let test_version () =
  check "version mentions Jasmin"
    (String.length Jide_jasmin.Adapter.jasmin_version > 0)

let test_recovery_not_yet () =
  (* The fast path only: a file with a syntax error yields one parse
     diagnostic and no typing diagnostics, which the recovery driver will
     later replace with a partial tree. *)
  let result = check_text "fast path" with_syntax_error in
  check "fast path: typing errors suppressed"
    (List.for_all
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         diagnostic.code = Some "parse")
       result.diagnostics)

let () =
  test_valid ();
  test_syntax_error ();
  test_type_error ();
  test_continue_after_failed_item ();
  test_capture ();
  test_single_line_warning ();
  test_quoted_location ();
  test_stacked_location ();
  test_warning ();
  test_parse_warning ();
  test_required_file ();
  test_required_file_error ();
  test_arch_names ();
  test_version ();
  test_recovery_not_yet ();
  if !failures = 0 then print_endline "all adapter tests passed"
  else (
    Printf.printf "%d adapter test(s) failed\n" !failures;
    exit 1)
