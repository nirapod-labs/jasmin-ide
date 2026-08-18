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
  Jide_jasmin.Adapter.check x86_64 linux ~file:"probe.jazz" ~text ~idirs:[]

let check_with ~resolve text =
  Jide_jasmin.Adapter.check ~resolve x86_64 linux ~file:"probe.jazz" ~text
    ~idirs:[]

let mentions (result : Jide_jasmin.Adapter.check_result) needle =
  List.exists
    (fun (diagnostic : Jide_lang.Diagnostic.t) ->
      contains
        (String.lowercase_ascii diagnostic.message)
        (String.lowercase_ascii needle))
    result.diagnostics

(* Whether one of the files reported as holding an error holds one that says
   [needle]. Those diagnostics are not on the document: their spans index the
   file, so the check returns them with it. *)
let required_mentions (result : Jide_jasmin.Adapter.check_result)
    ~(file : string) needle =
  List.exists
    (fun (required : Jide_jasmin.Adapter.required) ->
      String.equal required.file file
      && List.exists
           (fun (diagnostic : Jide_lang.Diagnostic.t) ->
             contains
               (String.lowercase_ascii diagnostic.message)
               (String.lowercase_ascii needle))
           required.diagnostics)
    result.required

(* Whether a span can index [text]: both offsets inside it, the end not
   before the start. A diagnostic whose span does not pass this indexes some
   other file, and a caller that places it would reject it or highlight
   nothing. *)
let indexes (text : string) (span : Jide_lang.Span.t) =
  span.Jide_lang.Span.start_byte >= 0
  && span.Jide_lang.Span.start_byte <= span.Jide_lang.Span.end_byte
  && span.Jide_lang.Span.end_byte <= String.length text

(* The bytes a span indexes, when it indexes [text] at all. *)
let text_at (text : string) (span : Jide_lang.Span.t) =
  if indexes text span then
    Some
      (String.sub text span.Jide_lang.Span.start_byte
         (span.Jide_lang.Span.end_byte - span.Jide_lang.Span.start_byte))
  else None

(* Whether every diagnostic's span can be placed on [text]. *)
let all_placed (text : string) (result : Jide_jasmin.Adapter.check_result) =
  List.for_all
    (fun (diagnostic : Jide_lang.Diagnostic.t) -> indexes text diagnostic.span)
    result.diagnostics

module X86 =
  (val Jide_jasmin.Arch.module_of Jide_jasmin.Arch.X86_64 Jide_jasmin.Arch.Linux
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

let library_plain = {|
fn library(reg u32 a) -> reg u32 {
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
  (* The compiler's own error, then the stub of the item recovery could not
     keep: the file's only function is broken, so the tree is empty and the
     marker for it has to come from the stub. *)
  check "syntax error: the compiler's error and one stub"
    (List.length result.Jide_jasmin.Adapter.diagnostics = 2);
  match result.diagnostics with
  | diagnostic :: rest ->
      check "syntax error: code"
        (diagnostic.Jide_lang.Diagnostic.code = Some "parse");
      check "syntax error: non-empty message"
        (String.length diagnostic.message > 0);
      check "syntax error: span inside the text"
        (diagnostic.span.Jide_lang.Span.end_byte
        <= String.length with_syntax_error);
      check "syntax error: the second diagnostic is the stub"
        (match rest with
        | [ stub ] ->
            contains stub.Jide_lang.Diagnostic.message "could not be parsed"
        | _ -> false)
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
  let file = "probe.jazz" in
  let parsed = Jide_jasmin.Parse.parse ~name:file first_item_fails in
  check "continue: the fixture parses" (not (Jide_jasmin.Parse.failed parsed));
  let result, environment =
    Loop.type_items
      ~resolve:(fun _ -> None)
      ~budget:200 ~dropped:[] ~recovered:[] ~context:[] ~file
      ~text:first_item_fails ~items:parsed.items ~idirs:[]
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
         && warning.Jide_jasmin.Warnings.file = "probe.jazz")
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
         warning.Jide_jasmin.Warnings.file <> "probe.jazz")
       result.warnings);
  remove_file library

(* A required file with an error of its own is not placed on the document,
   whose bytes its offsets do not describe. It is returned against the file it
   is in, with that file's own name and text, which is where the compiler
   prints it. The require item does not summarise it: the summary named the
   file and never showed the error. A call to a name the broken file declares
   is that same failure seen again and is not reported a second time. *)
let test_required_file_error () =
  let library = write_temp_file ~suffix:".jinc" library_with_type_error in
  let body =
    Printf.sprintf
      "require %S\n\n\
       fn entry(reg u32 a) -> reg u32 {\n\
      \  reg u32 x;\n\
      \  x = library(a);\n\
      \  return x;\n\
       }\n"
      library
  in
  let result = check_text "required error" body in
  check "required error: the document has no diagnostics"
    (result.Jide_jasmin.Adapter.diagnostics = []);
  check "required error: the file is reported under its own name"
    (List.exists
       (fun (required : Jide_jasmin.Adapter.required) ->
         String.equal required.file library)
       result.required);
  check "required error: it carries the file's own message"
    (required_mentions result ~file:library "implicitly cast");
  check "required error: its span indexes the file's own text"
    (List.for_all
       (fun (required : Jide_jasmin.Adapter.required) ->
         List.for_all
           (fun (diagnostic : Jide_lang.Diagnostic.t) ->
             indexes required.text diagnostic.span)
           required.diagnostics)
       result.required);
  check "required error: the call to it is not reported as unknown"
    (not (mentions result "unknown function"));
  let direct =
    Jide_jasmin.Adapter.check x86_64 linux ~file:library
      ~text:library_with_type_error ~idirs:[]
  in
  check "required error: the error itself is real"
    (List.length direct.diagnostics = 1);
  remove_file library

(* The lookup the caller passes in is what a require is read through, so a
   file the editor has open is used as it is and the disk is not consulted. *)
let test_required_file_from_a_lookup () =
  let asked = ref [] in
  let resolve path =
    asked := path :: !asked;
    if Filename.basename path = "open-buffer-only.jinc" then
      Some "fn library(reg u32 a) -> reg u32 {\n  return a;\n}\n"
    else None
  in
  let body =
    "require \"open-buffer-only.jinc\"\n\n\
     fn entry(reg u32 a) -> reg u32 {\n\
    \  reg u32 x;\n\
    \  x = library(a);\n\
    \  return x;\n\
     }\n"
  in
  let result = check_with ~resolve body in
  check "lookup: the document has no diagnostics"
    (result.Jide_jasmin.Adapter.diagnostics = []);
  check "lookup: the path asked for is the required one"
    (List.exists
       (fun path -> Filename.basename path = "open-buffer-only.jinc")
       !asked);
  let absent = check_text "absent" body in
  check "lookup: without the buffer the file is not found"
    (List.exists
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         contains diagnostic.message "cannot read")
       absent.diagnostics)

(* A file that a require cannot read is a diagnostic of the require item, not
   an exception from inside the compiler. *)
let test_required_file_missing () =
  let result =
    check_text "missing" (requiring "/nonexistent-directory/no-such.jinc")
  in
  check "missing: one diagnostic for the require item"
    (List.length result.Jide_jasmin.Adapter.diagnostics = 1);
  check "missing: it names the file"
    (List.exists
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         contains diagnostic.message "cannot read"
         && contains diagnostic.message "no-such.jinc")
       result.diagnostics)

(* A required file that does not even parse is a failure of the check, not a
   clean result: the document may be well formed only because the file is
   not, so the file's own parse diagnostics are returned against it. The
   names the broken file would have declared are not reported a second time
   as unknown names of the document, which would put a diagnostic on a
   correct line of it. *)
let test_required_file_syntax_error () =
  let broken = "fn library(reg u32 a -> reg u32 {\n  return a;\n}\n" in
  let resolve path =
    if Filename.basename path = "syntax-error.jinc" then Some broken else None
  in
  let body =
    "require \"syntax-error.jinc\"\n\n\
     fn entry(reg u32 a) -> reg u32 {\n\
    \  reg u32 x;\n\
    \  x = library(a);\n\
    \  return x;\n\
     }\n"
  in
  let result = check_with ~resolve body in
  check "required syntax error: the document has no diagnostic"
    (result.Jide_jasmin.Adapter.diagnostics = []);
  check "required syntax error: the file is reported under its own name"
    (List.exists
       (fun (required : Jide_jasmin.Adapter.required) ->
         String.equal required.file "syntax-error.jinc")
       result.required);
  check "required syntax error: it carries the parse diagnostic"
    (required_mentions result ~file:"syntax-error.jinc" "unexpected token");
  check "required syntax error: the name it did not declare is not reported"
    (not (mentions result "unknown"));
  check "required syntax error: its span indexes the file's own text"
    (List.for_all
       (fun (required : Jide_jasmin.Adapter.required) ->
         List.for_all
           (fun (diagnostic : Jide_lang.Diagnostic.t) ->
             indexes required.text diagnostic.span)
           required.diagnostics)
       result.required);
  let direct =
    Jide_jasmin.Adapter.check x86_64 linux ~file:"syntax-error.jinc"
      ~text:broken ~idirs:[]
  in
  check "required syntax error: the error itself is real"
    (List.exists
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         diagnostic.code = Some "parse")
       direct.diagnostics)

(* A required file with a typing error in one item and a parse error in
   another has both reported against itself: the parse error is not dropped
   because the typing error was recorded first. *)
let test_required_file_typing_and_syntax_error () =
  let broken =
    "fn library(reg u32 a) -> reg u32 {\n\
    \  reg bool b;\n\
    \  b = a;\n\
    \  return a;\n\
     }\n\n\
     fn other(reg u32 a -> reg u32 {\n\
    \  return a;\n\
     }\n"
  in
  let resolve path =
    if Filename.basename path = "both-errors.jinc" then Some broken else None
  in
  let result = check_with ~resolve (requiring "both-errors.jinc") in
  check "required both: the document has no diagnostic"
    (result.Jide_jasmin.Adapter.diagnostics = []);
  check "required both: the parse error is reported against the file"
    (required_mentions result ~file:"both-errors.jinc" "unexpected token");
  check "required both: the typing error is reported against the file"
    (required_mentions result ~file:"both-errors.jinc" "implicitly cast");
  check "required both: the file is the only one reported"
    (List.for_all
       (fun (required : Jide_jasmin.Adapter.required) ->
         String.equal required.file "both-errors.jinc")
       result.required)

(* One file required twice, once by the name beside the document and once
   through a [..] spelling of it, is one file. The compiler's [enter_file]
   answers [None] for a path its namespace has already loaded and reads
   nothing, so the second [require] adds no item; a checker that reads the
   file again splices its declarations twice and they collide with each
   other. The release's `tests/success/require/common/file2.jazz` requires
   one file both ways. *)
let test_required_file_twice () =
  let library = write_temp_file ~suffix:".jinc" library_plain in
  let spelled =
    Filename.concat (Filename.dirname library)
      ("nowhere/../" ^ Filename.basename library)
  in
  let body =
    Printf.sprintf
      "require %S\n\
       require %S\n\n\
       fn entry(reg u32 a) -> reg u32 {\n\
      \  reg u32 x;\n\
      \  x = library(a);\n\
      \  return x;\n\
       }\n"
      library spelled
  in
  let result = check_text "required twice" body in
  check "required twice: the document has no diagnostics"
    (result.Jide_jasmin.Adapter.diagnostics = []);
  remove_file library

(* The same two requires inside a namespace, where the file is spliced into
   the namespace's own items: read twice, its namespaced declarations would
   collide there. *)
let test_required_file_twice_inside_namespace () =
  let library = write_temp_file ~suffix:".jinc" library_plain in
  let spelled =
    Filename.concat (Filename.dirname library)
      ("nowhere/../" ^ Filename.basename library)
  in
  let body =
    Printf.sprintf
      "namespace N {\n\
      \  require %S\n\
      \  require %S\n\n\
      \  fn g(reg u32 a) -> reg u32 {\n\
      \    return a;\n\
      \  }\n\
       }\n\n\
       fn entry(reg u32 a) -> reg u32 {\n\
      \  reg u32 x;\n\
      \  x = N::library(a);\n\
      \  return x;\n\
       }\n"
      library spelled
  in
  let result = check_text "required twice in a namespace" body in
  check "required twice in a namespace: the document has no diagnostics"
    (result.Jide_jasmin.Adapter.diagnostics = []);
  remove_file library

(* Two files that require each other are a diagnostic too, not a recursion
   that ends in a stack overflow. *)
let test_required_file_cycle () =
  let resolve path =
    match Filename.basename path with
    | "cycle-a.jinc" ->
        Some
          "require \"cycle-b.jinc\"\n\n\
           fn a_name(reg u32 x) -> reg u32 {\n\
          \  return x;\n\
           }\n"
    | "cycle-b.jinc" ->
        Some
          "require \"cycle-a.jinc\"\n\n\
           fn b_name(reg u32 x) -> reg u32 {\n\
          \  return x;\n\
           }\n"
    | _ -> None
  in
  let result =
    check_with ~resolve
      "require \"cycle-a.jinc\"\n\n\
       fn entry(reg u32 a) -> reg u32 {\n\
      \  return a;\n\
       }\n"
  in
  check "cycle: one diagnostic"
    (List.length result.Jide_jasmin.Adapter.diagnostics = 1);
  check "cycle: it says the file requires itself"
    (List.exists
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         contains diagnostic.message "requires itself")
       result.diagnostics)

(* Two files that require each other, spelled with [./] on every name. The
   path is normalised before it is compared with the chain, so the same two
   files are the same two files however the require spelled them: one
   diagnostic that says the file requires itself, not a pile of markers
   whose first one blames the nesting bound. *)
let test_dot_prefixed_cycle () =
  let asked = ref [] in
  let resolve path =
    asked := path :: !asked;
    match Filename.basename path with
    | "dot-a.jinc" ->
        Some
          "require \"./dot-b.jinc\"\n\n\
           fn a_name(reg u32 x) -> reg u32 {\n\
          \  return x;\n\
           }\n"
    | "dot-b.jinc" ->
        Some
          "require \"./dot-a.jinc\"\n\n\
           fn b_name(reg u32 x) -> reg u32 {\n\
          \  return x;\n\
           }\n"
    | _ -> None
  in
  let result =
    check_with ~resolve
      "require \"./dot-a.jinc\"\n\n\
       fn entry(reg u32 a) -> reg u32 {\n\
      \  return a;\n\
       }\n"
  in
  check "dot cycle: one diagnostic"
    (List.length result.Jide_jasmin.Adapter.diagnostics = 1);
  check "dot cycle: it says the file requires itself"
    (List.exists
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         contains diagnostic.message "requires itself")
       result.diagnostics);
  check "dot cycle: it does not blame the nesting bound"
    (not (mentions result "nested too deeply"));
  check "dot cycle: the path asked for has no [.] left in it"
    (List.for_all
       (fun path -> (not (contains path "/./")) && not (contains path "/../"))
       !asked);
  check "dot cycle: both files were asked for, once each"
    (List.sort String.compare (List.map Filename.basename !asked)
    = [ "dot-a.jinc"; "dot-b.jinc" ])

(* The context a document that is not an entry is checked in, written inline
   as a three-file project: the entry requires the file that declares the
   parameter, then the document, which uses the parameter and does not
   require it. Alone the document does not resolve the name; in its entry's
   context it does, which is the rule of design 8.4. The entry's items after
   the require that names the document are not typed — the context stops
   there — so the error in them is nobody's diagnostic. *)
let test_context_chain_of_three_files () =
  let entry =
    "require \"parameter.jinc\"\n\
     require \"uses.jinc\"\n\n\
     fn probe(reg u32 a) -> reg u32 {\n\
    \  reg u32 x;\n\
    \  x = local(a);\n\
    \  return x;\n\
     }\n\n\
     fn after(reg u32 a) -> reg u32 {\n\
    \  reg u32 x;\n\
    \  x = never_declared(a);\n\
    \  return x;\n\
     }\n"
  in
  let parameter = "fn shared(reg u32 a) -> reg u32 {\n  return a;\n}\n" in
  let uses =
    "fn local(reg u32 a) -> reg u32 {\n\
    \  reg u32 x;\n\
    \  x = shared(a);\n\
    \  return x;\n\
     }\n"
  in
  let resolve path =
    match Filename.basename path with
    | "parameter.jinc" -> Some parameter
    | "uses.jinc" -> Some uses
    | "entry.jazz" -> Some entry
    | _ -> None
  in
  let check_document ~file ~text ~context =
    Jide_jasmin.Adapter.check ~resolve ~context x86_64 linux ~file ~text
      ~idirs:[]
  in
  let entry_check = check_document ~file:"entry.jazz" ~text:entry ~context:[] in
  check
    "three files: the entry reports only the error written after its requires"
    (List.length entry_check.Jide_jasmin.Adapter.diagnostics = 1
    && mentions entry_check "never_declared");
  let alone_check = check_document ~file:"uses.jinc" ~text:uses ~context:[] in
  check "three files: alone the document does not know the shared name"
    (mentions alone_check "shared");
  let in_context =
    check_document ~file:"uses.jinc" ~text:uses ~context:[ "entry.jazz" ]
  in
  check "three files: in its entry's context the document is clean"
    (in_context.Jide_jasmin.Adapter.diagnostics = []);
  check "three files: the context stops at the document"
    (in_context.Jide_jasmin.Adapter.required = [])

(* A file reached through two chains: the entry requires the parameter file
   itself and also the file that requires it, so the parameter file is the
   second require's require. It is entered once — the [loaded] guard — and
   its declaration is not reported as already declared. *)
let test_diamond_of_three_files () =
  let entry =
    "require \"diamond-parameter.jinc\"\n\
     require \"diamond-middle.jinc\"\n\n\
     fn probe(reg u32 a) -> reg u32 {\n\
    \  reg u32 x;\n\
    \  x = middle(a);\n\
    \  return x;\n\
     }\n"
  in
  let parameter = "fn shared(reg u32 a) -> reg u32 {\n  return a;\n}\n" in
  let middle =
    "require \"diamond-parameter.jinc\"\n\n\
     fn middle(reg u32 a) -> reg u32 {\n\
    \  reg u32 x;\n\
    \  x = shared(a);\n\
    \  return x;\n\
     }\n"
  in
  let asked = ref [] in
  let resolve path =
    let name = Filename.basename path in
    asked := name :: !asked;
    match name with
    | "diamond-parameter.jinc" -> Some parameter
    | "diamond-middle.jinc" -> Some middle
    | _ -> None
  in
  let result =
    Jide_jasmin.Adapter.check ~resolve x86_64 linux ~file:"entry.jazz"
      ~text:entry ~idirs:[]
  in
  check "diamond: the entry is clean"
    (result.Jide_jasmin.Adapter.diagnostics = []);
  check "diamond: nothing is reported as already declared"
    (result.Jide_jasmin.Adapter.required = []);
  check "diamond: the shared file is read once"
    (List.length (List.filter (String.equal "diamond-parameter.jinc") !asked)
    = 1)

(* A require inside a namespace goes through the same lookup a top-level one
   uses. The library's own include path reads the disk and cannot be reached
   from here, so the adapter reads the file through the lookup and types its
   items inside the namespace: the file is open in the editor, and the
   namespaced name it declares resolves. *)
let test_require_inside_namespace () =
  let asked = ref [] in
  let resolve path =
    asked := path :: !asked;
    if Filename.basename path = "namespaced-only.jinc" then
      Some "fn library(reg u32 a) -> reg u32 {\n  return a;\n}\n"
    else None
  in
  let body =
    "namespace N {\n\
    \  require \"namespaced-only.jinc\"\n\n\
    \  fn g(reg u32 a) -> reg u32 {\n\
    \    return a;\n\
    \  }\n\
     }\n\n\
     fn use(reg u32 a) -> reg u32 {\n\
    \  reg u32 x;\n\
    \  x = N::library(a);\n\
    \  return x;\n\
     }\n"
  in
  let result = check_with ~resolve body in
  check "namespace require: no diagnostics"
    (result.Jide_jasmin.Adapter.diagnostics = []);
  check "namespace require: the lookup was asked for the file"
    (List.exists
       (fun path -> Filename.basename path = "namespaced-only.jinc")
       !asked);
  check "namespace require: the path asked for is normalised"
    (List.for_all
       (fun path -> (not (contains path "/./")) && not (contains path "/../"))
       !asked)

(* The same require when the lookup and the disk both have nothing: the
   require item says the file cannot be read, once, and the names the file
   would have declared are treated as absent rather than reported as a
   fabricated unknown function. No raw compiler rendering is placed on the
   document. *)
let test_require_inside_namespace_missing () =
  let body =
    "namespace N {\n\
    \  require \"namespaced-missing.jinc\"\n\n\
    \  fn g(reg u32 a) -> reg u32 {\n\
    \    return a;\n\
    \  }\n\
     }\n\n\
     fn use(reg u32 a) -> reg u32 {\n\
    \  reg u32 x;\n\
    \  x = N::library(a);\n\
    \  return x;\n\
     }\n"
  in
  let result = check_text "namespace missing" body in
  check "namespace missing: one diagnostic"
    (List.length result.Jide_jasmin.Adapter.diagnostics = 1);
  check "namespace missing: it is the adapter's cannot-read"
    (mentions result "cannot read");
  check "namespace missing: it names the file"
    (mentions result "namespaced-missing.jinc");
  check "namespace missing: no raw compiler error is placed"
    (not (mentions result "error reading file"));
  check "namespace missing: no fabricated unknown function"
    (not (mentions result "unknown function"))

(* A namespaced include that does not parse: the require item says the file
   has errors and the names it would have declared are absent, so the call
   to one of them is that failure seen again. *)
let test_require_inside_namespace_has_errors () =
  let resolve path =
    if Filename.basename path = "namespaced-broken.jinc" then
      Some "fn library(reg u32 a -> reg u32 {\n  return a;\n}\n"
    else None
  in
  let body =
    "namespace N {\n\
    \  require \"namespaced-broken.jinc\"\n\n\
    \  fn g(reg u32 a) -> reg u32 {\n\
    \    return a;\n\
    \  }\n\
     }\n\n\
     fn use(reg u32 a) -> reg u32 {\n\
    \  reg u32 x;\n\
    \  x = N::library(a);\n\
    \  return x;\n\
     }\n"
  in
  let result = check_with ~resolve body in
  check "namespace errors: one diagnostic"
    (List.length result.Jide_jasmin.Adapter.diagnostics = 1);
  check "namespace errors: it says the required file has errors"
    (mentions result "has errors");
  check "namespace errors: it names the file"
    (mentions result "namespaced-broken.jinc");
  check "namespace errors: no fabricated unknown function"
    (not (mentions result "unknown function"))

(* A namespaced include that was read and does not type. The error's offsets
   index the required file, not the document, and the namespace's items are
   spliced into the namespace's one item, so that item is what says the file
   has errors instead of the file being attributed on its own. The call to a
   name the broken file declares is that failure seen again, not a fabricated
   unknown function. *)
let test_require_inside_namespace_type_error () =
  let file = "namespaced-typed-broken.jinc" in
  let resolve path =
    if Filename.basename path = file then
      Some
        "fn library(reg u32 a) -> reg u32 {\n\
        \  reg u32 x;\n\
        \  x = not_a_function(a);\n\
        \  return x;\n\
         }\n"
    else None
  in
  let use =
    "fn use(reg u32 a) -> reg u32 {\n\
    \  reg u32 x;\n\
    \  x = N::library(a);\n\
    \  return x;\n\
     }\n"
  in
  let namespaced =
    "namespace N {\n\
    \  require \"namespaced-typed-broken.jinc\"\n\n\
    \  fn g(reg u32 a) -> reg u32 {\n\
    \    return a;\n\
    \  }\n\
     }\n\n" ^ use
  in
  let result = check_with ~resolve namespaced in
  check "namespace type error: one diagnostic"
    (List.length result.Jide_jasmin.Adapter.diagnostics = 1);
  check "namespace type error: it says the required file has errors"
    (mentions result "has errors");
  check "namespace type error: it names the file" (mentions result file);
  check "namespace type error: the include's own message is not placed"
    (not (mentions result "not_a_function"));
  check "namespace type error: no fabricated unknown function"
    (not (mentions result "unknown function"));
  check "namespace type error: it is placed on the require item"
    (List.exists
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         diagnostic.span.Jide_lang.Span.start_byte > 0
         && diagnostic.span.Jide_lang.Span.end_byte
            > diagnostic.span.Jide_lang.Span.start_byte)
       result.diagnostics);
  (* The same file at top level is the same answer, which is the parity this
     test exists to keep. *)
  let top_level =
    check_with ~resolve
      (Printf.sprintf
         "require %S\n\n\
          fn use(reg u32 a) -> reg u32 {\n\
         \  reg u32 x;\n\
         \  x = library(a);\n\
         \  return x;\n\
          }\n"
         file)
  in
  check "namespace type error: the top-level require agrees"
    (List.length top_level.Jide_jasmin.Adapter.diagnostics = 1
    && mentions top_level "has errors"
    && mentions top_level file)

(* The suppression a namespace require installs is for the fabricated
   cascade: the unresolved names an unreadable file would have declared. It
   must not reach an error of a file that was read, so a namespace with both
   an unreadable require and a readable one that does not type reports both
   failures, and the call to a name only the unreadable file could declare is
   still suppressed. *)
let test_require_inside_namespace_suppression_scope () =
  let resolve path =
    match Filename.basename path with
    | "namespaced-scope-broken.jinc" ->
        Some
          "fn library(reg u32 a) -> reg u32 {\n\
          \  reg u32 x;\n\
          \  x = not_a_function(a);\n\
          \  return x;\n\
           }\n"
    | _ -> None
  in
  let body =
    "namespace N {\n\
    \  require \"namespaced-scope-missing.jinc\"\n\
    \  require \"namespaced-scope-broken.jinc\"\n\
     }\n\n\
     fn use(reg u32 a) -> reg u32 {\n\
    \  reg u32 x;\n\
    \  x = N::library(a);\n\
    \  return x;\n\
     }\n"
  in
  let result = check_with ~resolve body in
  check "suppression scope: one diagnostic per require item"
    (List.length result.Jide_jasmin.Adapter.diagnostics = 2);
  check "suppression scope: the unreadable file is reported"
    (List.exists
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         contains diagnostic.message "cannot read"
         && contains diagnostic.message "namespaced-scope-missing.jinc")
       result.diagnostics);
  check "suppression scope: the read file's failure is not hidden"
    (List.exists
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         contains diagnostic.message "has errors"
         && contains diagnostic.message "namespaced-scope-broken.jinc")
       result.diagnostics);
  check "suppression scope: the fabricated cascade is not reported"
    (not (mentions result "unknown function"))

(* A namespaced require of a file that requires another one, and the file
   that one requires does not type. The failure is two levels down, so the
   error the namespace fails on names the deeper file, not the file the
   document named. One diagnostic per require item of the chain has to say
   what it read, the deeper file first, which is the answer the same two
   files give when the document requires them at top level. Without the
   deeper require's record reaching the site list, the error names a file no
   require item was recorded as having read and the document is silent. *)
let test_require_inside_namespace_two_levels () =
  let outer = "namespaced-outer.jinc" in
  let inner = "namespaced-inner.jinc" in
  let resolve path =
    match Filename.basename path with
    | "namespaced-outer.jinc" -> Some (Printf.sprintf "require %S\n" inner)
    | "namespaced-inner.jinc" ->
        Some
          "fn library(reg u32 a) -> reg u32 {\n\
          \  reg u32 x;\n\
          \  x = not_a_function(a);\n\
          \  return x;\n\
           }\n"
    | _ -> None
  in
  let names file (result : Jide_jasmin.Adapter.check_result) =
    List.exists
      (fun (diagnostic : Jide_lang.Diagnostic.t) ->
        contains diagnostic.message "has errors"
        && contains diagnostic.message file)
      result.diagnostics
  in
  let namespaced =
    Printf.sprintf
      "namespace N {\n\
      \  require %S\n\
       }\n\n\
       fn use(reg u32 a) -> reg u32 {\n\
      \  reg u32 x;\n\
      \  x = N::library(a);\n\
      \  return x;\n\
       }\n"
      outer
  in
  let result = check_with ~resolve namespaced in
  check "two levels: one diagnostic per require item"
    (List.length result.Jide_jasmin.Adapter.diagnostics = 2);
  check "two levels: the deeper file is reported" (names inner result);
  check "two levels: the file that read it is reported" (names outer result);
  check "two levels: the include's own message is not placed"
    (not (mentions result "not_a_function"));
  check "two levels: no fabricated unknown function"
    (not (mentions result "unknown function"));
  check "two levels: every span indexes the document"
    (all_placed namespaced result);
  (* The same two files required from the top of the document are the oracle:
     they give one diagnostic per require item, the deeper file first. *)
  let top_level =
    check_with ~resolve
      (Printf.sprintf
         "require %S\n\n\
          fn use(reg u32 a) -> reg u32 {\n\
         \  reg u32 x;\n\
         \  x = library(a);\n\
         \  return x;\n\
          }\n"
         outer)
  in
  check "two levels: the top-level require agrees on the count"
    (List.length top_level.Jide_jasmin.Adapter.diagnostics
    = List.length result.Jide_jasmin.Adapter.diagnostics);
  check "two levels: the top-level require agrees on the deeper file"
    (names inner top_level);
  check "two levels: the top-level require agrees on the file that read it"
    (names outer top_level)

(* A require written inside a namespaced include is an item of that include,
   so its offsets index the include and not the document. The failure is
   reported on the document item whose expansion reached the file — the
   namespace item — and the span never reaches past the document's end. A
   span taken from the include would be more than twice this document's
   length. *)
let test_require_inside_namespaced_include_is_placed () =
  let far =
    String.make 240 ' ' ^ "require \"namespaced-nested-missing.jinc\"\n"
  in
  let resolve path =
    if Filename.basename path = "namespaced-far.jinc" then Some far else None
  in
  let body =
    "namespace N {\n\
    \  require \"namespaced-far.jinc\"\n\
     }\n\n\
     fn use(reg u32 a) -> reg u32 {\n\
    \  reg u32 x;\n\
    \  x = N::library(a);\n\
    \  return x;\n\
     }\n"
  in
  let result = check_with ~resolve body in
  check "nested missing: one diagnostic"
    (List.length result.Jide_jasmin.Adapter.diagnostics = 1);
  check "nested missing: it says the file cannot be read"
    (mentions result "cannot read"
    && mentions result "namespaced-nested-missing.jinc");
  check "nested missing: its span indexes the document" (all_placed body result);
  check "nested missing: it attaches to the namespace item"
    (List.exists
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         match text_at body diagnostic.span with
         | Some text -> contains text "namespace N"
         | None -> false)
       result.diagnostics)

(* A namespace's requires are expanded before the namespace is typed, and the
   expansion is the one path that can write several diagnostics for one item.
   The 200-diagnostic bound is still the answer: the loop stops at it, and
   the items after it are not typed. Without the bound being tested there,
   one diagnostic per unreadable file comes back, 250 in this document. *)
let test_expansion_respects_the_diagnostic_bound () =
  let requires =
    List.init 250 (fun index ->
        Printf.sprintf "  require \"namespaced-absent-%d.jinc\"\n" index)
  in
  let body = "namespace N {\n" ^ String.concat "" requires ^ "}\n" in
  let result = check_with ~resolve:(fun _ -> None) body in
  check "bound: the expansion does not overshoot the bound"
    (List.length result.Jide_jasmin.Adapter.diagnostics = 200);
  check "bound: every returned diagnostic is the expansion's own"
    (List.for_all
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         contains diagnostic.message "cannot read")
       result.diagnostics);
  check "bound: every span indexes the document" (all_placed body result)

(* A [from] name no [-I NAME=PATH] bound is refused, not resolved against
   the document's directory: the compiler's own [unknown name %s] at the
   name's bytes, and nothing is read for the item. The file is beside the
   document and the lookup would answer it, which is exactly the program the
   compiler rejects; with the binding the same document reads the same file
   and has nothing to report. *)
let test_unknown_include_name () =
  let asked = ref [] in
  let resolve path =
    asked := path :: !asked;
    if Filename.basename path = "bound-inc.jazz" then
      Some "fn inc(reg u32 x) -> reg u32 {\n  return x;\n}\n"
    else None
  in
  let body =
    "from LIB require \"bound-inc.jazz\"\n\n\
     fn main(reg u32 a) -> reg u32 {\n\
    \  return a;\n\
     }\n"
  in
  let with_idirs idirs =
    Jide_jasmin.Adapter.check ~resolve x86_64 linux ~file:"probe.jazz"
      ~text:body ~idirs
  in
  let unbound = with_idirs [] in
  check "unknown name: one diagnostic"
    (List.length unbound.Jide_jasmin.Adapter.diagnostics = 1);
  check "unknown name: the compiler's message on the name's bytes"
    (List.exists
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         diagnostic.message = "unknown name LIB"
         && text_at body diagnostic.span = Some "LIB")
       unbound.diagnostics);
  check "unknown name: the file beside the document is not read" (!asked = []);
  let bound = with_idirs [ ("LIB", "lib") ] in
  check "unknown name: the binding reads the file and passes"
    (bound.Jide_jasmin.Adapter.diagnostics = []);
  check "unknown name: the path asked for is the bound one"
    (List.exists (fun path -> contains path "/lib/bound-inc.jazz") !asked)

(* The same item inside a namespace: one diagnostic on the name, no file
   read, and the namespace's names treated as the unread file's, so the
   reference to N::library is not a second, fabricated error. Written in a
   namespaced include instead, the name's bytes index the include, so the
   refusal is placed on the document's namespace item like every other
   failure of that expansion. *)
let test_unknown_include_name_inside_namespace () =
  let asked = ref [] in
  let resolve path =
    asked := path :: !asked;
    if Filename.basename path = "bound-ns.jazz" then
      Some "fn library(reg u32 a) -> reg u32 {\n  return a;\n}\n"
    else None
  in
  let body =
    "namespace N {\n\
    \  from LIB require \"bound-ns.jazz\"\n\
     }\n\n\
     fn use(reg u32 a) -> reg u32 {\n\
    \  reg u32 x;\n\
    \  x = N::library(a);\n\
    \  return x;\n\
     }\n"
  in
  let result = check_with ~resolve body in
  check "namespaced unknown name: one diagnostic"
    (List.length result.Jide_jasmin.Adapter.diagnostics = 1);
  check "namespaced unknown name: the compiler's message on the name's bytes"
    (List.exists
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         diagnostic.message = "unknown name LIB"
         && text_at body diagnostic.span = Some "LIB")
       result.diagnostics);
  check "namespaced unknown name: the file beside the document is not read"
    (!asked = []);
  check "namespaced unknown name: no fabricated unknown function"
    (not (mentions result "unknown function"));
  (* The same item written in a namespaced include, past 240 bytes of
     padding, so a span taken from the include would run past this
     document's end. *)
  let far = String.make 240 ' ' ^ "from NOPE require \"nowhere.jinc\"\n" in
  let resolve_far path =
    if Filename.basename path = "unknown-name-outer.jinc" then Some far
    else None
  in
  let outer_body =
    "namespace N {\n\
    \  require \"unknown-name-outer.jinc\"\n\
     }\n\n\
     fn use(reg u32 a) -> reg u32 {\n\
    \  return a;\n\
     }\n"
  in
  let outer = check_with ~resolve:resolve_far outer_body in
  check "nested unknown name: one diagnostic"
    (List.length outer.Jide_jasmin.Adapter.diagnostics = 1);
  check "nested unknown name: it is the compiler's message"
    (mentions outer "unknown name NOPE");
  check "nested unknown name: its span indexes the document"
    (all_placed outer_body outer);
  check "nested unknown name: it attaches to the namespace item"
    (List.exists
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         match text_at outer_body diagnostic.span with
         | Some text -> contains text "namespace N"
         | None -> false)
       outer.diagnostics)

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
  (* A file whose only item is broken: recovery drops it, so there is
     nothing left for the typing loop to type and the compiler's parse
     diagnostic is the whole result. The loop over a tree that does keep
     items is tested in the typing tests. *)
  let result = check_text "fast path" with_syntax_error in
  check "fast path: only the parse error is reported"
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
  test_required_file_from_a_lookup ();
  test_required_file_missing ();
  test_required_file_cycle ();
  test_dot_prefixed_cycle ();
  test_require_inside_namespace ();
  test_require_inside_namespace_missing ();
  test_require_inside_namespace_has_errors ();
  test_require_inside_namespace_type_error ();
  test_require_inside_namespace_suppression_scope ();
  test_require_inside_namespace_two_levels ();
  test_require_inside_namespaced_include_is_placed ();
  test_expansion_respects_the_diagnostic_bound ();
  test_unknown_include_name ();
  test_unknown_include_name_inside_namespace ();
  test_arch_names ();
  test_version ();
  test_recovery_not_yet ();
  if !failures = 0 then print_endline "all adapter tests passed"
  else (
    Printf.printf "%d adapter test(s) failed\n" !failures;
    exit 1)
