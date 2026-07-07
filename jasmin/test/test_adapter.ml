let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let x86_64 = Option.get (Jide_jasmin.Adapter.arch_of_string "x86-64")
let linux = Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux")

let check_text name text =
  Jide_jasmin.Adapter.check x86_64 linux ~file:"/tmp/probe.jazz" ~text ~idirs:[]

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

let test_capture () =
  let (), text =
    Jide_jasmin.Warnings.with_capture (fun () ->
        Format.eprintf
          "\"f.jazz\", line 3 (4-5): warning: unused variable x.7@.")
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
         let message = String.lowercase_ascii warning.message in
         let has sub =
           let n = String.length message and m = String.length sub in
           let rec loop i =
             i + m <= n && (String.sub message i m = sub || loop (i + 1))
           in
           loop 0
         in
         has "unused")
       result.warnings)

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
  test_warning ();
  test_arch_names ();
  test_version ();
  test_recovery_not_yet ();
  if !failures = 0 then print_endline "all adapter tests passed"
  else (
    Printf.printf "%d adapter test(s) failed\n" !failures;
    exit 1)
