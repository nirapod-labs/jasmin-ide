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

let create () = Jide_engine.Engine.create ()
let uri_of name = "file:///tmp/" ^ name ^ ".jazz"

let valid =
  "fn f(reg u32 a) -> reg u32 {\n  reg u32 b;\n  b = a;\n  return b;\n}\n"

let invalid =
  "fn f(reg u32 a) -> reg u32 {\n  reg bool b;\n  b = a;\n  return a;\n}\n"

let open_text engine ~uri ~version text =
  Jide_engine.Engine.open_document engine ~uri ~version ~text

let diagnostics_of engine ~uri =
  match Jide_engine.Engine.check engine ~uri with
  | Some analysis -> analysis.Jide_engine.Engine.diagnostics
  | None -> []

let messages diagnostics =
  List.map
    (fun (diagnostic : Jide_lang.Diagnostic.t) -> diagnostic.message)
    diagnostics

let message_of engine ~uri =
  match diagnostics_of engine ~uri with
  | [ diagnostic ] -> diagnostic.Jide_lang.Diagnostic.message
  | _ -> "<not one diagnostic>"

let span_of_single engine ~uri =
  match diagnostics_of engine ~uri with
  | [ diagnostic ] -> diagnostic.Jide_lang.Diagnostic.span
  | _ -> Jide_lang.Span.make ~start_byte:(-1) ~end_byte:(-1)

(* The store's file and path cases need names for the engine and the document,
   a temporary directory, and a labelled check that prints both sides of a
   comparison. Each case prints its name, so the output of a run doubles as
   the list of what was checked. *)
module Engine = Jide_engine.Engine
module Document = Jide_engine.Document

let tests = ref 0
let current = ref ""

let fail format =
  Printf.ksprintf
    (fun message ->
      incr failures;
      Printf.printf "FAIL %s: %s\n" !current message)
    format

let check_bool label condition =
  if not condition then fail "%s: the condition does not hold" label

let check_int label expected actual =
  if expected <> actual then
    fail "%s: expected %d, got %d" label expected actual

let check_string label expected actual =
  if not (String.equal expected actual) then
    fail "%s: expected %S, got %S" label expected actual

let test name body =
  current := name;
  incr tests;
  let before = !failures in
  (try body () with exn -> fail "raised %s" (Printexc.to_string exn));
  if !failures = before then Printf.printf "ok   %s\n" name

let uri_of_path path = "file://" ^ path

(* [lstat], not [stat]: a symbolic link is removed as a link, so the tree
   cannot reach its target, and a link whose target is already gone is still
   removed. *)
let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path);
      Sys.rmdir path
  | _ -> Sys.remove path

let with_temp_dir body =
  let path = Filename.temp_file "jide-engine-test" "" in
  Sys.remove path;
  Sys.mkdir path 0o700;
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> body path)

let write_file path text =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel text)

let check_text label expected = function
  | None -> fail "%s: expected %S, got None" label expected
  | Some actual -> check_string label expected actual

let check_version label expected = function
  | None -> fail "%s: expected %d, got None" label expected
  | Some actual -> check_int label expected actual

let check_key label = function
  | None -> fail "%s: expected a key, got None" label
  | Some _ -> ()

let check_path label expected = function
  | Ok actual -> check_string label expected actual
  | Error message -> fail "%s: expected %S, got Error %S" label expected message

let check_refused label = function
  | Error _ -> ()
  | Ok path -> fail "%s: expected a refusal, got %S" label path

(* The store's own contract, without the controller in the way: only a
   document the client opened may be changed, and a closed document must
   never come back from a change. *)

let test_change_without_open_is_ignored () =
  let engine = create () in
  let uri = uri_of "never-opened" in
  Jide_engine.Engine.change_document engine ~uri ~version:1
    ~text:"fn f() -> reg u32 { return 0; }";
  check "change without open: no document created"
    (Jide_engine.Engine.document engine ~uri = None);
  check "change without open: the store stays empty"
    (Jide_engine.Engine.documents engine = [])

let test_change_after_close_is_ignored () =
  let engine = create () in
  let uri = uri_of "closed" in
  Jide_engine.Engine.open_document engine ~uri ~version:1
    ~text:"fn f() -> reg u32 { return 0; }";
  Jide_engine.Engine.close_document engine ~uri;
  Jide_engine.Engine.change_document engine ~uri ~version:2
    ~text:"fn g() -> reg u32 { return 1; }";
  check "change after close: no document created"
    (Jide_engine.Engine.document engine ~uri = None);
  check "change after close: the store stays empty"
    (Jide_engine.Engine.documents engine = [])

let test_open_then_change_updates_the_text () =
  let engine = create () in
  let uri = uri_of "open" in
  Jide_engine.Engine.open_document engine ~uri ~version:1
    ~text:"fn f() -> reg u32 { return 0; }";
  Jide_engine.Engine.change_document engine ~uri ~version:3
    ~text:"fn g() -> reg u32 { return 1; }";
  match Jide_engine.Engine.document engine ~uri with
  | Some document ->
      check "open then change: text"
        (Jide_engine.Document.text document = "fn g() -> reg u32 { return 1; }");
      check "open then change: version"
        (Jide_engine.Document.version document = 3)
  | None -> check "open then change: document kept" false

let test_close_removes_the_document () =
  let engine = create () in
  let uri = uri_of "removed" in
  Jide_engine.Engine.open_document engine ~uri ~version:1 ~text:"fn f()";
  Jide_engine.Engine.close_document engine ~uri;
  check "close: document removed"
    (Jide_engine.Engine.document engine ~uri = None);
  check "close: the store is empty" (Jide_engine.Engine.documents engine = [])

(* The analysis: what it is keyed by, what it reports and how it survives a
   failure. *)

let test_analysis_key_follows_the_text () =
  let engine = create () in
  let uri = uri_of "key" in
  open_text engine ~uri ~version:1 valid;
  match Jide_engine.Engine.analysis_key engine ~uri with
  | None -> check "key: a document has a key" false
  | Some ((version, digest) as key) ->
      check "key: the first component is the document's version" (version = 1);
      check "key: the digest is 32 hexadecimal characters"
        (String.length digest = 32 && not (contains digest " "));
      check "key: an unchanged text keeps the key"
        (Jide_engine.Engine.analysis_key engine ~uri = Some key);
      Jide_engine.Engine.change_document engine ~uri ~version:2 ~text:valid;
      let same = Jide_engine.Engine.analysis_key engine ~uri in
      check "key: the same bytes under a new version give a new key"
        (same <> Some key);
      Jide_engine.Engine.change_document engine ~uri ~version:2 ~text:invalid;
      check "key: other bytes at the same version give a new key"
        (Jide_engine.Engine.analysis_key engine ~uri <> same);
      (* The key is a function of the version and the bytes and nothing else,
         so putting the earlier bytes back puts the earlier key back. *)
      Jide_engine.Engine.change_document engine ~uri ~version:2 ~text:valid;
      check "key: restored text restores the key"
        (Jide_engine.Engine.analysis_key engine ~uri = same)

(* The key is the text's, and the text of a closed document is the file's, so
   a uri has no key only when nothing can be read for it. The path below is
   inside a directory the case owns and has removed again, so no file can be
   there whatever the host happens to hold in /tmp. *)
let test_analysis_key_without_a_document () =
  with_temp_dir (fun directory ->
      let engine = create () in
      let uri = uri_of_path (Filename.concat directory "absent.jazz") in
      check "key: no document and no file, no key"
        (Jide_engine.Engine.analysis_key engine ~uri = None))

let test_check_reuses_the_analysis () =
  let engine = create () in
  let uri = uri_of "reused" in
  open_text engine ~uri ~version:1 valid;
  let first = Jide_engine.Engine.check engine ~uri in
  check "reuse: one analysis ran" (Jide_engine.Engine.analysis_count engine = 1);
  let second = Jide_engine.Engine.check engine ~uri in
  check "reuse: the same analysis is returned"
    (match (first, second) with Some a, Some b -> a == b | _ -> false);
  check "reuse: no second analysis ran"
    (Jide_engine.Engine.analysis_count engine = 1);
  Jide_engine.Engine.change_document engine ~uri ~version:2 ~text:invalid;
  let third = Jide_engine.Engine.check engine ~uri in
  check "reuse: a change analyses again"
    (Jide_engine.Engine.analysis_count engine = 2);
  check "reuse: the new analysis has the new version"
    (match third with
    | Some analysis -> analysis.Jide_engine.Engine.version = 2
    | None -> false);
  check "reuse: the analysis before a check is the stored one"
    (Jide_engine.Engine.analysis engine ~uri = third)

let test_check_without_a_document () =
  let engine = create () in
  check "check: no document, no analysis"
    (Jide_engine.Engine.check engine ~uri:(uri_of "absent") = None);
  check "check: nothing ran" (Jide_engine.Engine.analysis_count engine = 0)

let test_clean_document () =
  let engine = create () in
  let uri = uri_of "clean" in
  open_text engine ~uri ~version:7 valid;
  match Jide_engine.Engine.check engine ~uri with
  | Some analysis ->
      check "clean: no diagnostic" (analysis.Jide_engine.Engine.diagnostics = []);
      check "clean: the version is the document's" (analysis.version = 7);
      check "clean: the name is the document's" (analysis.uri = uri);
      check "clean: no byte-order mark was removed" (analysis.bom = 0)
  | None -> check "clean: an analysis" false

let test_type_error_span () =
  let engine = create () in
  let uri = uri_of "type-error" in
  open_text engine ~uri ~version:1 invalid;
  let diagnostics = diagnostics_of engine ~uri in
  match diagnostics with
  | [ diagnostic ] ->
      check "type error: the compiler's code"
        (diagnostic.Jide_lang.Diagnostic.code = Some "typing");
      check "type error: severity"
        (diagnostic.severity = Jide_lang.Diagnostic.Error);
      check "type error: the span is inside the text"
        (diagnostic.span.Jide_lang.Span.start_byte >= 0
        && diagnostic.span.end_byte <= String.length invalid);
      check "type error: the message"
        (contains diagnostic.message "implicitly cast u32 into bool")
  | other ->
      check
        (Printf.sprintf "type error: one diagnostic, got %d" (List.length other))
        false

let test_bom_shifts_the_spans () =
  let engine = create () in
  let uri = uri_of "bom" in
  open_text engine ~uri ~version:1 ("\xEF\xBB\xBF" ^ invalid);
  match Jide_engine.Engine.check engine ~uri with
  | None -> check "bom: an analysis" false
  | Some analysis -> (
      check "bom: three bytes removed" (analysis.bom = 3);
      check "bom: the analysed text has no mark" (analysis.text = invalid);
      match analysis.diagnostics with
      | [ diagnostic ] ->
          check "bom: the span starts after the mark"
            (diagnostic.Jide_lang.Diagnostic.span.Jide_lang.Span.start_byte > 3);
          check "bom: the span is inside the document"
            (diagnostic.span.end_byte
            <= String.length ("\xEF\xBB\xBF" ^ invalid))
      | other ->
          check
            (Printf.sprintf "bom: one diagnostic, got %d" (List.length other))
            false)

let test_bom_without_an_error () =
  let engine = create () in
  let uri = uri_of "bom-clean" in
  open_text engine ~uri ~version:1 ("\xEF\xBB\xBF" ^ valid);
  match Jide_engine.Engine.check engine ~uri with
  | Some analysis ->
      check "bom clean: no diagnostic" (analysis.diagnostics = []);
      check "bom clean: three bytes removed" (analysis.bom = 3);
      check "bom clean: the analysed text" (analysis.text = valid)
  | None -> check "bom clean: an analysis" false

let test_byte_order_mark_alone () =
  let engine = create () in
  let uri = uri_of "bom-only" in
  open_text engine ~uri ~version:1 "\xEF\xBB\xBF";
  match Jide_engine.Engine.check engine ~uri with
  | Some analysis ->
      check "bom only: three bytes removed" (analysis.bom = 3);
      check "bom only: an empty text" (analysis.text = "")
  | None -> check "bom only: an analysis" false

let test_span_after_a_bom_matches_the_plain_text () =
  let plain = create () and with_bom = create () in
  let plain_uri = uri_of "plain" and bom_uri = uri_of "with-bom" in
  open_text plain ~uri:plain_uri ~version:1 invalid;
  open_text with_bom ~uri:bom_uri ~version:1 ("\xEF\xBB\xBF" ^ invalid);
  let plain_span = span_of_single plain ~uri:plain_uri in
  let bom_span = span_of_single with_bom ~uri:bom_uri in
  check "bom: the same error is three bytes further on"
    (bom_span = Jide_lang.Span.shift plain_span 3)

let test_warning_becomes_a_diagnostic () =
  let engine = create () in
  let uri = uri_of "warned" in
  open_text engine ~uri ~version:1
    "fn f(reg u32 a) -> reg u32 {\n  reg u32 unused;\n  return a;\n}\n";
  let diagnostics = diagnostics_of engine ~uri in
  match diagnostics with
  | [ diagnostic ] ->
      check "warning: severity"
        (diagnostic.Jide_lang.Diagnostic.severity = Jide_lang.Diagnostic.Warning);
      check "warning: the code is warning" (diagnostic.code = Some "warning");
      check "warning: the message"
        (contains diagnostic.message "unused variable");
      check "warning: the span is on the declared name"
        (diagnostic.span = Jide_lang.Span.make ~start_byte:39 ~end_byte:45)
  | other ->
      check
        (Printf.sprintf "warning: one diagnostic, got %d" (List.length other))
        false

let test_warning_after_a_bom () =
  let engine = create () in
  let uri = uri_of "bom-warned" in
  let text =
    "fn f(reg u32 a) -> reg u32 {\n  reg u32 unused;\n  return a;\n}\n"
  in
  open_text engine ~uri ~version:1 ("\xEF\xBB\xBF" ^ text);
  match diagnostics_of engine ~uri with
  | [ diagnostic ] ->
      (* The same bytes as the case above, three bytes further into the
         document. *)
      check "bom warning: shifted by three bytes"
        (diagnostic.Jide_lang.Diagnostic.span
        = Jide_lang.Span.make ~start_byte:42 ~end_byte:48)
  | other ->
      check
        (Printf.sprintf "bom warning: one diagnostic, got %d"
           (List.length other))
        false

let test_errors_come_before_warnings () =
  let engine = create () in
  let uri = uri_of "mixed" in
  (* The warning is in one function and the error in the next: typing stops at
     the first error inside a function, so both have to be in the file for the
     order to be observable. *)
  open_text engine ~uri ~version:1
    "fn g(reg u32 a) -> reg u32 {\n\
    \  reg u32 unused;\n\
    \  return a;\n\
     }\n\n\
     fn f(reg u32 a) -> reg u32 {\n\
    \  reg bool b;\n\
    \  b = a;\n\
    \  return a;\n\
     }\n";
  match diagnostics_of engine ~uri with
  | error :: warning :: rest ->
      check "order: an error first"
        (error.Jide_lang.Diagnostic.severity = Jide_lang.Diagnostic.Error);
      check "order: the error comes later in the file"
        (error.Jide_lang.Diagnostic.span.Jide_lang.Span.start_byte
       > warning.Jide_lang.Diagnostic.span.Jide_lang.Span.start_byte);
      check "order: then the warning"
        (warning.severity = Jide_lang.Diagnostic.Warning);
      check "order: nothing else" (rest = [])
  | other ->
      check
        (Printf.sprintf "order: an error then a warning, got %d"
           (List.length other))
        false

let test_over_the_size_limit () =
  let engine = create () in
  let uri = uri_of "huge" in
  let limit = Jide_engine.Engine.max_bytes in
  check "size: the limit is 8 MiB" (limit = 8 * 1024 * 1024);
  (* One byte over the limit is refused without a parse, so the filler does
     not have to be a program. *)
  open_text engine ~uri ~version:1 (String.make (limit + 1) '\n');
  (match Jide_engine.Engine.check engine ~uri with
  | Some analysis -> (
      match analysis.Jide_engine.Engine.diagnostics with
      | [ diagnostic ] ->
          check "size: one diagnostic"
            (diagnostic.Jide_lang.Diagnostic.code = Some "size");
          check "size: it is an error"
            (diagnostic.severity = Jide_lang.Diagnostic.Error);
          check "size: it says the limit"
            (contains diagnostic.message
               (string_of_int Jide_engine.Engine.max_bytes))
      | other ->
          check
            (Printf.sprintf "size: one diagnostic, got %d" (List.length other))
            false)
  | None -> check "size: an analysis" false);
  check "size: nothing was checked"
    (Jide_engine.Engine.analysis_count engine = 1)

let test_at_the_size_limit_is_analysed () =
  let engine = create () in
  let uri = uri_of "at-limit" in
  let limit = Jide_engine.Engine.max_bytes in
  let text = valid ^ String.make (limit - String.length valid) ' ' in
  open_text engine ~uri ~version:1 text;
  match Jide_engine.Engine.check engine ~uri with
  | Some analysis ->
      check "size: a document exactly at the limit is analysed"
        (match analysis.diagnostics with
        | [] -> true
        | [ diagnostic ] -> diagnostic.Jide_lang.Diagnostic.code <> Some "size"
        | _ -> false)
  | None -> check "size: an analysis at the limit" false

let test_the_adapter_admits_what_the_engine_admits () =
  check "size: the adapter parses a document the engine admits"
    (Jide_jasmin.Adapter.max_bytes >= Jide_engine.Engine.max_bytes)

let test_exception_becomes_an_internal_diagnostic () =
  let raised = ref 0 in
  let check_raises ~context:_ ~arch:_ ~call_conv:_ ~file:_ ~text:_ ~idirs:_ =
    incr raised;
    raise (Failure "the checker broke")
  in
  let engine = Jide_engine.Engine.create ~check:check_raises () in
  let uri = uri_of "raising" in
  open_text engine ~uri ~version:1 valid;
  (match Jide_engine.Engine.check engine ~uri with
  | Some analysis -> (
      match analysis.Jide_engine.Engine.diagnostics with
      | [ diagnostic ] ->
          check "internal: one diagnostic"
            (diagnostic.Jide_lang.Diagnostic.code = Some "internal");
          check "internal: it is an error"
            (diagnostic.severity = Jide_lang.Diagnostic.Error);
          check "internal: the exception is named"
            (contains diagnostic.message "the checker broke");
          check "internal: it sits at the first byte"
            (diagnostic.span = Jide_lang.Span.make ~start_byte:0 ~end_byte:0)
      | other ->
          check
            (Printf.sprintf "internal: one diagnostic, got %d"
               (List.length other))
            false)
  | None -> check "internal: an analysis" false);
  check "internal: the checker ran once" (!raised = 1);
  (* The next call is served: the failure is in the analysis, not the store. *)
  Jide_engine.Engine.change_document engine ~uri ~version:2 ~text:invalid;
  check "internal: the next check still runs"
    (match Jide_engine.Engine.check engine ~uri with
    | Some analysis -> List.length analysis.diagnostics = 1
    | None -> false);
  check "internal: the checker ran twice" (!raised = 2)

let test_a_failing_check_does_not_break_the_store () =
  let check_raises ~context:_ ~arch:_ ~call_conv:_ ~file:_ ~text:_ ~idirs:_ =
    raise Stack_overflow
  in
  let engine = Jide_engine.Engine.create ~check:check_raises () in
  let uri = uri_of "overflow" in
  open_text engine ~uri ~version:1 valid;
  ignore (Jide_engine.Engine.check engine ~uri);
  check "store: the document is still open"
    (Jide_engine.Engine.document engine ~uri <> None);
  check "store: the key still exists"
    (Jide_engine.Engine.analysis_key engine ~uri <> None)

let test_two_spellings_of_one_uri () =
  let engine = create () in
  let uri = "file:///tmp/one%20two.jazz" in
  open_text engine ~uri ~version:1 valid;
  let other = "file:///tmp/one two.jazz" in
  check "uri: another spelling is the same document"
    (Jide_engine.Engine.document engine ~uri:other <> None);
  check "uri: and has the same analysis"
    (match
       ( Jide_engine.Engine.check engine ~uri,
         Jide_engine.Engine.check engine ~uri:other )
     with
    | Some a, Some b -> a == b
    | _ -> false)

let test_settings () =
  let engine = create () in
  check "settings: the default architecture"
    (Jide_engine.Engine.settings engine = Jide_engine.Engine.default_settings);
  let settings = Jide_engine.Engine.default_settings in
  Jide_engine.Engine.set_settings engine
    { settings with idirs = [ ("A", "b") ] };
  check "settings: stored"
    ((Jide_engine.Engine.settings engine).idirs = [ ("A", "b") ])

let test_the_settings_reach_the_checker () =
  let seen = ref [] in
  let check_captures ~context:_ ~arch:_ ~call_conv:_ ~file:_ ~text:_ ~idirs =
    seen := idirs;
    {
      Jide_jasmin.Adapter.diagnostics = [];
      warnings = [];
      facts = Jide_jasmin.Facts.empty;
      required = [];
    }
  in
  let engine = Jide_engine.Engine.create ~check:check_captures () in
  let settings = Jide_engine.Engine.default_settings in
  Jide_engine.Engine.set_settings engine
    { settings with idirs = [ ("X", "y") ] };
  open_text engine ~uri:(uri_of "idirs") ~version:1 valid;
  ignore (Jide_engine.Engine.check engine ~uri:(uri_of "idirs"));
  check "settings: the include paths reach the checker" (!seen = [ ("X", "y") ])

(* A settings change is a change to what an analysis says, so the analysis
   made under the old settings must not be served. The checker below writes
   the include paths it was given into its diagnostic, so a stale analysis is
   visible in the message. *)
let test_a_settings_change_forgets_the_analysis () =
  let check_reports ~context:_ ~arch:_ ~call_conv:_ ~file:_ ~text:_ ~idirs =
    let message =
      match idirs with
      | [] -> "no include path"
      | idirs ->
          String.concat ","
            (List.map (fun (name, path) -> name ^ "=" ^ path) idirs)
    in
    {
      Jide_jasmin.Adapter.diagnostics =
        [
          Jide_lang.Diagnostic.make ~code:"settings"
            ~severity:Jide_lang.Diagnostic.Error
            (Jide_lang.Span.make ~start_byte:0 ~end_byte:0)
            message;
        ];
      warnings = [];
      facts = Jide_jasmin.Facts.empty;
      required = [];
    }
  in
  let engine = Jide_engine.Engine.create ~check:check_reports () in
  let uri = uri_of "settings-change" in
  open_text engine ~uri ~version:1 valid;
  let settings = Jide_engine.Engine.settings engine in
  Jide_engine.Engine.set_settings engine
    { settings with idirs = [ ("A", "a") ] };
  check "settings change: the first analysis is made under them"
    (message_of engine ~uri = "A=a");
  check "settings change: one analysis ran"
    (Jide_engine.Engine.analysis_count engine = 1);
  Jide_engine.Engine.set_settings engine
    { settings with idirs = [ ("B", "b") ] };
  check "settings change: the analysis is made again"
    (message_of engine ~uri = "B=b");
  check "settings change: two analyses ran"
    (Jide_engine.Engine.analysis_count engine = 2);
  (* The same value again is not a change, so the stored analysis is reused. *)
  Jide_engine.Engine.set_settings engine
    { settings with idirs = [ ("B", "b") ] };
  check "settings change: the same settings reuse the analysis"
    (Jide_engine.Engine.analysis_count engine = 2)

(* The context stage is the last stage of the analysis and the one with real
   IO in it. The interface promises that an exception anywhere in the
   pipeline is one [internal] diagnostic, so an exception out of this stage
   must be caught rather than leaving [check]. *)
let test_an_exception_in_the_context_stage_is_internal () =
  let context_raises ~entries:_ ~idirs:_ ~read:_ _ =
    raise (Failure "the context stage broke")
  in
  let check_is_fine ~context:_ ~arch:_ ~call_conv:_ ~file:_ ~text:_ ~idirs:_ =
    {
      Jide_jasmin.Adapter.diagnostics = [];
      warnings = [];
      facts = Jide_jasmin.Facts.empty;
      required = [];
    }
  in
  let engine =
    Jide_engine.Engine.create ~check:check_is_fine ~context:context_raises ()
  in
  let uri = uri_of "context-raises" in
  open_text engine ~uri ~version:1 valid;
  (match Jide_engine.Engine.check engine ~uri with
  | Some analysis -> (
      check "context raises: the text is the analysed one"
        (analysis.Jide_engine.Engine.text = valid);
      match analysis.Jide_engine.Engine.diagnostics with
      | [ diagnostic ] ->
          check "context raises: one internal diagnostic"
            (diagnostic.Jide_lang.Diagnostic.code = Some "internal");
          check "context raises: it is an error"
            (diagnostic.severity = Jide_lang.Diagnostic.Error);
          check "context raises: the exception is named"
            (contains diagnostic.message "the context stage broke");
          check "context raises: the span is at the first byte"
            (diagnostic.span = Jide_lang.Span.make ~start_byte:0 ~end_byte:0)
      | other ->
          check
            (Printf.sprintf "context raises: one diagnostic, got %d"
               (List.length other))
            false;
          check "context raises: the file is checked alone"
            (Jide_engine.Context.entry analysis.Jide_engine.Engine.context
            = None))
  | None -> check "context raises: an analysis" false);
  (* The failure is in the analysis, not in the store: the analysis is stored
     and the next call is served. *)
  check "context raises: the analysis is stored"
    (Jide_engine.Engine.analysis engine ~uri <> None);
  check "context raises: one analysis ran"
    (Jide_engine.Engine.analysis_count engine = 1);
  check "context raises: the next check still answers"
    (match Jide_engine.Engine.check engine ~uri with
    | Some analysis -> List.length analysis.Jide_engine.Engine.diagnostics = 1
    | None -> false)

(* The snapshot: what the last check built, read back without a second check.
   These four tests are the contract the completion and signature-help lanes
   are written against, so each of them names one half of it: nothing is
   analysed to answer, the answer is the last analysis's, it survives a broken
   file, and a caller can see when it is stale. *)

(* A checker that counts how many times it was called, so "no check ran" is a
   number and not an inference. Its result is empty, so the assertions below
   are about the index and the resolver, which are built from the text and do
   not read the checker's output. *)
let counting_check counter ~context:_ ~arch:_ ~call_conv:_ ~file:_ ~text:_
    ~idirs:_ =
  incr counter;
  {
    Jide_jasmin.Adapter.diagnostics = [];
    warnings = [];
    facts = Jide_jasmin.Facts.empty;
    required = [];
  }

let names_of_declarations (index : Jide_engine.Index.t) =
  List.map
    (fun (declaration : Jide_engine.Index.declaration) -> declaration.name)
    index.Jide_engine.Index.declarations

(* The text a declaration's name span covers, read out of the index's own
   text: a declaration whose name is not where its span says would be a silent
   corruption a name-only check would miss. *)
let declaration_texts (index : Jide_engine.Index.t) =
  List.map
    (fun (declaration : Jide_engine.Index.declaration) ->
      ( declaration.name,
        Jide_engine.Index.text_of_span index.Jide_engine.Index.text
          declaration.name_span ))
    index.Jide_engine.Index.declarations

(* Where a use of [name] is, taken from the index's own occurrences rather
   than from byte arithmetic written by hand: the [n]th occurrence of the name
   that is a use and not the declaration. Every byte below that is a position
   of the text is chosen this way, so the tests cannot drift when a word in a
   program changes length. *)
let use_byte (index : Jide_engine.Index.t) ~(name : string) ~(n : int) =
  let uses =
    List.filter
      (fun (occurrence : Jide_engine.Index.occurrence) ->
        String.equal occurrence.Jide_engine.Index.name name
        &&
        match occurrence.Jide_engine.Index.role with
        | Jide_engine.Index.Declared _ -> false
        | _ -> true)
      index.Jide_engine.Index.occurrences
  in
  match List.nth_opt uses (n - 1) with
  | Some occurrence ->
      Some occurrence.Jide_engine.Index.span.Jide_lang.Span.start_byte
  | None -> None

(* (a) A uri that was never analysed has no snapshot, and asking for one does
   not analyse it. The proof is two counters: the engine's own
   [analysis_count], and a checker that counts the calls it received. *)
let test_snapshot_without_an_analysis () =
  let calls = ref 0 in
  let engine = Jide_engine.Engine.create ~check:(counting_check calls) () in
  let uri = uri_of "never-analysed" in
  open_text engine ~uri ~version:1 valid;
  check "snapshot: no analysis yet, no snapshot"
    (Jide_engine.Engine.snapshot engine ~uri = None);
  check "snapshot: asking did not run a check"
    (Jide_engine.Engine.analysis_count engine = 0);
  check "snapshot: and did not call the checker" (!calls = 0);
  (* A uri no document was opened for is the same answer, and the same
     nothing. *)
  check "snapshot: an unknown uri has no snapshot"
    (Jide_engine.Engine.snapshot engine ~uri:(uri_of "not-open") = None);
  check "snapshot: the document is still open"
    (Jide_engine.Engine.document engine ~uri <> None);
  check "snapshot: still nothing analysed"
    (Jide_engine.Engine.analysis_count engine = 0 && !calls = 0)

(* (b) After a check, the snapshot is the index and the resolver that check
   built: the declarations name the text they came from, the resolver answers a
   known name, and a completion answers from the two without a check. *)
let test_snapshot_after_a_check () =
  let calls = ref 0 in
  let engine = Jide_engine.Engine.create ~check:(counting_check calls) () in
  let uri = uri_of "snapshot" in
  let text =
    "fn double(reg u32 a) -> reg u32 {\n\
    \  reg u32 b;\n\
    \  b = a;\n\
    \  return b;\n\
     }\n\
     fn use_it(reg u32 c) -> reg u32 {\n\
    \  c = double(c);\n\
    \  return c;\n\
     }\n"
  in
  open_text engine ~uri ~version:1 text;
  ignore (Jide_engine.Engine.check engine ~uri);
  check "snapshot: one analysis ran"
    (Jide_engine.Engine.analysis_count engine = 1);
  match Jide_engine.Engine.snapshot engine ~uri with
  | None -> check "snapshot: an analysis has one" false
  | Some snapshot ->
      let index = snapshot.Jide_engine.Engine.index in
      let resolve = snapshot.Jide_engine.Engine.resolve in
      check "snapshot: the index is the document's own text"
        (String.equal index.Jide_engine.Index.text text);
      check "snapshot: the index names the document"
        (match Jide_lang.Uri.path (Jide_lang.Uri.of_string uri) with
        | Some path -> String.equal index.Jide_engine.Index.file path
        | None -> false);
      check "snapshot: the declarations are the text's own"
        (List.mem ("double", "double") (declaration_texts index));
      check "snapshot: a parameter and a local are declared"
        (List.mem "a" (names_of_declarations index)
        && List.mem "b" (names_of_declarations index));
      check "snapshot: the function is in the outline"
        (List.exists
           (fun (item : Jide_engine.Index.item) ->
             item.Jide_engine.Index.item_name = Some "double")
           index.Jide_engine.Index.outline);
      (* The resolver answers a known name at a use. [return b] is the second
         use of [b]; the resolver must name the declaration of [b], not the
         use, so the identity is the declaration's own name byte. *)
      check "snapshot: the resolver answers the name at the return"
        (match use_byte index ~name:"b" ~n:2 with
        | None -> false
        | Some byte -> (
            match
              Jide_engine.Resolve.answer_at resolve
                ~file:index.Jide_engine.Index.file ~byte
            with
            | Some (_, Jide_engine.Resolve.Resolved declaration) ->
                String.equal declaration.Jide_engine.Index.name "b"
                && declaration.Jide_engine.Index.kind
                   = Jide_engine.Index.Variable
                && declaration.Jide_engine.Index.name_span
                     .Jide_lang.Span.start_byte <> byte
            | _ -> false));
      (* The completion lane's own call, over the snapshot alone: the function
         declared in the file is offered as a name in scope, and no check ran
         to answer it. The offset is the start of the body of the second
         function, where both functions are declared already. *)
      let at =
        match use_byte index ~name:"c" ~n:1 with Some byte -> byte | None -> 0
      in
      let completion =
        Jide_engine.Complete.completion ~arch:Jide_jasmin.Arch.X86_64 ~index
          ~resolve ~byte:at ()
      in
      check "snapshot: completion answers from it"
        (match completion.Jide_engine.Complete.items with
        | [] -> false
        | items ->
            List.exists
              (fun (item : Jide_engine.Complete.item) ->
                String.equal item.Jide_engine.Complete.label "double")
              items);
      check "snapshot: answering ran no check"
        (Jide_engine.Engine.analysis_count engine = 1 && !calls = 1)

(* (c) The contract the caller relies on: the snapshot is the LAST analysis's,
   even after the text changed, because reading it must never recheck. The
   staleness it costs is visible: the stored key and the current key differ. *)
let test_snapshot_is_the_last_analysis () =
  let calls = ref 0 in
  let engine = Jide_engine.Engine.create ~check:(counting_check calls) () in
  let uri = uri_of "stale-snapshot" in
  let before = "fn f(reg u32 a) -> reg u32 {\n  return a;\n}\n" in
  let after = "fn g(reg u32 a) -> reg u32 {\n  reg u32 b;\n  return b;\n}\n" in
  open_text engine ~uri ~version:1 before;
  ignore (Jide_engine.Engine.check engine ~uri);
  let stored_key =
    match Jide_engine.Engine.analysis engine ~uri with
    | Some analysis -> analysis.Jide_engine.Engine.key
    | None -> (-1, "")
  in
  let first = Jide_engine.Engine.snapshot engine ~uri in
  Jide_engine.Engine.change_document engine ~uri ~version:2 ~text:after;
  (* The change alone analyses nothing: the engine checks only when asked. *)
  check "stale: a change runs no check"
    (Jide_engine.Engine.analysis_count engine = 1 && !calls = 1);
  (match (first, Jide_engine.Engine.snapshot engine ~uri) with
  | Some old_snapshot, Some new_snapshot ->
      check "stale: the snapshot is the same value, physically"
        (old_snapshot == new_snapshot);
      check "stale: it still holds the old text"
        (String.equal
           old_snapshot.Jide_engine.Engine.index.Jide_engine.Index.text before);
      check "stale: it still declares the old name"
        (List.mem "f"
           (names_of_declarations old_snapshot.Jide_engine.Engine.index));
      check "stale: it does not hold the new name yet"
        (not
           (List.mem "g"
              (names_of_declarations old_snapshot.Jide_engine.Engine.index)))
  | _ -> check "stale: a snapshot on both sides" false);
  check "stale: reading it ran no check"
    (Jide_engine.Engine.analysis_count engine = 1 && !calls = 1);
  (* The staleness is what the caller compares: the document's key is now a
     different value from the key of the analysis the snapshot came from. *)
  check "stale: the current key is not the stored one"
    (Jide_engine.Engine.analysis_key engine ~uri <> Some stored_key);
  (* And a check makes them agree again, with the new text's snapshot. *)
  ignore (Jide_engine.Engine.check engine ~uri);
  (match Jide_engine.Engine.snapshot engine ~uri with
  | Some snapshot ->
      check "stale: after the check the snapshot is the new text's"
        (String.equal snapshot.Jide_engine.Engine.index.Jide_engine.Index.text
           after);
      check "stale: and it declares the new name"
        (List.mem "g" (names_of_declarations snapshot.Jide_engine.Engine.index))
  | None -> check "stale: a snapshot after the check" false);
  check "stale: the saved analysis is the current one"
    (Jide_engine.Engine.analysis_key engine ~uri
    = Option.map
        (fun (analysis : Jide_engine.Engine.analysis) -> analysis.key)
        (Jide_engine.Engine.analysis engine ~uri))

(* (d) A file that does not parse still yields a snapshot, and its index still
   holds the items the recovering parser kept: that is what the outline and
   hover read while the file is broken (design section 1 goal 2, and the
   invariant that a broken file keeps them working). The broken statement is
   dropped, so the function is still there. *)
let test_snapshot_of_a_broken_file () =
  let calls = ref 0 in
  let engine = Jide_engine.Engine.create ~check:(counting_check calls) () in
  let uri = uri_of "broken-snapshot" in
  let text =
    "fn keep(reg u32 a) -> reg u32 {\n\
    \  reg u32 b;\n\
    \  b = ;\n\
    \  return a;\n\
     }\n\
     fn also_kept(reg u32 c) -> reg u32 {\n\
    \  return c;\n\
     }\n"
  in
  open_text engine ~uri ~version:1 text;
  ignore (Jide_engine.Engine.check engine ~uri);
  match Jide_engine.Engine.snapshot engine ~uri with
  | None -> check "broken: a snapshot is still there" false
  | Some snapshot ->
      let index = snapshot.Jide_engine.Engine.index in
      let names = names_of_declarations index in
      check "broken: the snapshot has the document's text"
        (String.equal index.Jide_engine.Index.text text);
      check "broken: the surviving functions are declared"
        (List.mem "keep" names && List.mem "also_kept" names);
      check "broken: the outline keeps the surviving items"
        (List.length index.Jide_engine.Index.outline >= 2);
      check "broken: the resolver answers a name in the kept function"
        (match use_byte index ~name:"c" ~n:1 with
        | None -> false
        | Some byte -> (
            match
              Jide_engine.Resolve.scope_at snapshot.Jide_engine.Engine.resolve
                ~file:index.Jide_engine.Index.file ~byte
            with
            | Some scope ->
                List.exists
                  (fun (binding : Jide_engine.Resolve.binding) ->
                    String.equal binding.Jide_engine.Resolve.name "c")
                  scope.Jide_engine.Resolve.vars
            | None -> false));
      check "broken: reading it ran no extra check"
        (Jide_engine.Engine.analysis_count engine = 1 && !calls = 1)

(* Reading a URI: the open buffer first, the file second. *)

let test_open_and_read_back () =
  let engine = Engine.create () in
  let uri = uri_of "opened" in
  let text = "fn f() -> reg u32 { return 0; }\n" in
  Engine.open_document engine ~uri ~version:7 ~text;
  check_text "open: the buffer's text" text (Engine.text engine ~uri);
  check_version "open: the client's version" 7 (Engine.version engine ~uri)

let test_change_and_read_the_new_text () =
  let engine = Engine.create () in
  let uri = uri_of "changed" in
  Engine.open_document engine ~uri ~version:1 ~text:"fn f()";
  Engine.change_document engine ~uri ~version:4 ~text:"fn g()";
  check_text "change: the new text" "fn g()" (Engine.text engine ~uri);
  check_version "change: the new version" 4 (Engine.version engine ~uri)

let test_close_then_read_from_disk () =
  with_temp_dir (fun directory ->
      let path = Filename.concat directory "disk.jazz" in
      write_file path "fn from_disk()\n";
      let uri = uri_of_path path in
      let engine = Engine.create () in
      Engine.open_document engine ~uri ~version:5 ~text:"fn from_buffer()\n";
      check_text "before close: the buffer wins" "fn from_buffer()\n"
        (Engine.text engine ~uri);
      check_version "before close: the client's version" 5
        (Engine.version engine ~uri);
      Engine.close_document engine ~uri;
      check_text "after close: the file's text" "fn from_disk()\n"
        (Engine.text engine ~uri);
      check_version "after close: version 0" 0 (Engine.version engine ~uri))

(* One document, however the client spells its URI: a percent-encoded path
   and the decoded one, and a scheme in another case, are one key. The file
   on disk holds other bytes, so a fall-through would be visible. *)
let test_uri_spellings_are_one_document () =
  with_temp_dir (fun directory ->
      let path = Filename.concat directory "a b.jazz" in
      write_file path "fn from_disk()\n";
      let decoded = "file://" ^ path in
      let encoded = String.concat "%20" (String.split_on_char ' ' decoded) in
      let alternate_scheme =
        "FILE" ^ String.sub decoded 4 (String.length decoded - 4)
      in
      let localhost = "file://localhost" ^ path in
      let engine = Engine.create () in
      Engine.open_document engine ~uri:encoded ~version:3
        ~text:"fn from_buffer()\n";
      check_int "spellings: one document" 1
        (List.length (Engine.documents engine));
      check_text "open encoded, read decoded: the buffer's text"
        "fn from_buffer()\n"
        (Engine.text engine ~uri:decoded);
      check_version "open encoded, read decoded: the client's version" 3
        (Engine.version engine ~uri:decoded);
      check_text "open encoded, read through localhost: the buffer's text"
        "fn from_buffer()\n"
        (Engine.text engine ~uri:localhost);
      check_bool "open encoded, read upper-case scheme: one analysis key"
        (Engine.analysis_key engine ~uri:decoded
        = Engine.analysis_key engine ~uri:alternate_scheme);
      (match Engine.document engine ~uri:localhost with
      | None -> fail "document lookup: expected the document, got None"
      | Some document ->
          check_string "document lookup: the URI it was opened with" encoded
            (Document.uri document));
      Engine.change_document engine ~uri:decoded ~version:4
        ~text:"fn changed()\n";
      check_text "change decoded, read encoded: the new text" "fn changed()\n"
        (Engine.text engine ~uri:encoded);
      check_version "change decoded, read encoded: the new version" 4
        (Engine.version engine ~uri:encoded);
      Engine.close_document engine ~uri:alternate_scheme;
      check_text "close in another spelling: the file's text" "fn from_disk()\n"
        (Engine.text engine ~uri:decoded);
      check_bool "close in another spelling: no document"
        (Engine.document engine ~uri:encoded = None);
      check_int "close in another spelling: the store is empty" 0
        (List.length (Engine.documents engine)))

let test_missing_file_is_a_message () =
  with_temp_dir (fun directory ->
      let path = Filename.concat directory "missing.jazz" in
      let uri = uri_of_path path in
      (match Document.of_file ~uri path with
      | Ok _ -> check_bool "missing: of_file fails" false
      | Error message ->
          check_bool "missing: the message is not empty"
            (String.length message > 0));
      let engine = Engine.create () in
      check_bool "missing: text is None" (Engine.text engine ~uri = None);
      check_bool "missing: version is None" (Engine.version engine ~uri = None);
      check_bool "missing: the analysis key is None"
        (Engine.analysis_key engine ~uri = None);
      check_bool "a non-file uri has no text"
        (Engine.text engine ~uri:"untitled:Untitled-1" = None))

let test_file_keeps_its_bytes () =
  with_temp_dir (fun directory ->
      let path = Filename.concat directory "bytes.jazz" in
      let bom = "\xEF\xBB\xBF" in
      let text = bom ^ "fn f()\n" in
      write_file path text;
      let uri = uri_of_path path in
      match Document.of_file ~uri path with
      | Error message -> fail "file: expected the bytes, got Error %S" message
      | Ok document -> (
          check_string "file: the text" text (Document.text document);
          check_int "file: version 0" 0 (Document.version document);
          check_string "file: the uri" uri (Document.uri document);
          match Document.path document with
          | Some actual -> check_string "file: the path" path actual
          | None -> check_bool "file: the path is recorded" false))

(* A read is bounded: a regular file over the limit and anything that is not
   a regular file are refused before a byte is read. The file exactly at the
   limit is still read, so the bound is off by one in the safe direction
   only. *)
let test_reads_are_bounded () =
  with_temp_dir (fun directory ->
      let big = Filename.concat directory "big.jazz" in
      write_file big "fn f()\n";
      Unix.truncate big (Document.max_file_bytes + 1);
      (match Document.read_file big with
      | Ok _ -> fail "a file over the limit: expected a refusal"
      | Error message ->
          check_bool "a file over the limit: the message is not empty"
            (String.length message > 0));
      let uri = uri_of_path big in
      let engine = Engine.create () in
      check_bool "a file over the limit: the text is None"
        (Engine.text engine ~uri = None);
      check_bool "a file over the limit: the version is None"
        (Engine.version engine ~uri = None);
      check_bool "a file over the limit: the analysis key is None"
        (Engine.analysis_key engine ~uri = None);
      let at_limit = Filename.concat directory "at-limit.jazz" in
      write_file at_limit "";
      Unix.truncate at_limit Document.max_file_bytes;
      (match Document.read_file at_limit with
      | Error message ->
          fail "a file at the limit: expected the bytes, got Error %S" message
      | Ok text ->
          check_int "a file at the limit: every byte is read"
            Document.max_file_bytes (String.length text));
      (match Document.read_file "/dev/null" with
      | Ok _ -> fail "a device: expected a refusal"
      | Error message ->
          check_bool "a device: the message is not empty"
            (String.length message > 0));
      match Document.read_file directory with
      | Ok _ -> fail "a directory: expected a refusal"
      | Error message ->
          check_bool "a directory: the message is not empty"
            (String.length message > 0))

let test_analysis_key_follows_a_file () =
  with_temp_dir (fun directory ->
      let path = Filename.concat directory "key.jazz" in
      write_file path "fn a()\n";
      let uri = uri_of_path path in
      let engine = Engine.create () in
      let key = Engine.analysis_key engine ~uri in
      check_key "file key: present" key;
      check_bool "file key: the same bytes keep the key"
        (Engine.analysis_key engine ~uri = key);
      (match key with
      | Some (version, _) -> check_int "file key: version 0" 0 version
      | None -> ());
      write_file path "fn b()\n";
      let changed = Engine.analysis_key engine ~uri in
      check_bool "file key: changed bytes change the key" (changed <> key);
      write_file path "fn a()\n";
      check_bool "file key: the same bytes give the same key again"
        (Engine.analysis_key engine ~uri = key))

(* Paths inside the workspace root. *)

let test_resolve_relative_path () =
  let engine = Engine.create () in
  Engine.set_root engine (Some "/tmp/jide-engine-root");
  check_path "relative" "/tmp/jide-engine-root/src/main.jazz"
    (Engine.resolve engine "src/main.jazz");
  check_path "dot and dot dot" "/tmp/jide-engine-root/lib/main.jazz"
    (Engine.resolve engine "src/../lib/./main.jazz");
  check_path "absolute inside" "/tmp/jide-engine-root/main.jazz"
    (Engine.resolve engine "/tmp/jide-engine-root/main.jazz");
  Engine.set_root engine (Some "/tmp/jide-engine-root/");
  check_path "root with a trailing slash" "/tmp/jide-engine-root/src/main.jazz"
    (Engine.resolve engine "src/main.jazz")

let test_resolve_refuses_escape () =
  let engine = Engine.create () in
  Engine.set_root engine (Some "/tmp/jide-engine-root");
  check_refused "a parent component" (Engine.resolve engine "../outside.jazz");
  check_refused "a deeper escape"
    (Engine.resolve engine "src/../../outside.jazz");
  check_refused "an absolute path outside" (Engine.resolve engine "/etc/passwd");
  check_refused "a sibling sharing the prefix"
    (Engine.resolve engine "/tmp/jide-engine-root-other/main.jazz")

let test_resolve_without_root () =
  let engine = Engine.create () in
  check_refused "relative without a root" (Engine.resolve engine "main.jazz");
  check_refused "absolute without a root"
    (Engine.resolve engine "/tmp/main.jazz")

(* A link that leaves the root is refused by the check on the real path of
   the part that exists; a link that stays inside still resolves. *)
let test_resolve_refuses_a_symlink_escape () =
  with_temp_dir (fun directory ->
      let inside = Filename.concat directory "inside.jazz" in
      write_file inside "fn inside()\n";
      let inside_link = Filename.concat directory "inside-link.jazz" in
      Unix.symlink inside inside_link;
      let elsewhere = Filename.temp_file "jide-engine-outside" ".jazz" in
      Fun.protect
        ~finally:(fun () -> try Sys.remove elsewhere with Sys_error _ -> ())
        (fun () ->
          write_file elsewhere "fn outside()\n";
          let escape = Filename.concat directory "escape.jazz" in
          Unix.symlink elsewhere escape;
          let engine = Engine.create () in
          Engine.set_root engine (Some directory);
          check_path "a symlink that stays inside the root" inside_link
            (Engine.resolve engine "inside-link.jazz");
          check_refused "a symlink that leaves the root"
            (Engine.resolve engine "escape.jazz")))

(* The comparison is byte for byte, so a path that differs from the root
   only in case is refused. That is the safe direction, and it is what a
   volume whose names are case-sensitive does anyway. *)
let test_resolve_compares_names_exactly () =
  let engine = Engine.create () in
  Engine.set_root engine (Some "/tmp/JIDE-Root");
  check_refused "a root written in another case"
    (Engine.resolve engine "/tmp/jide-root/a.jazz");
  Engine.set_root engine (Some "/tmp/jide-engine-root");
  check_refused "a parent written in another case"
    (Engine.resolve engine "../JIDE-ENGINE-ROOT/a.jazz")

(* The two edges of the lexical rule: the root of the filesystem accepts
   every absolute path, and the empty path is the root itself. *)
let test_resolve_special_paths () =
  let engine = Engine.create () in
  Engine.set_root engine (Some "/");
  check_path "a relative path under the root of the filesystem" "/tmp/main.jazz"
    (Engine.resolve engine "tmp/main.jazz");
  check_path "an absolute path under the root of the filesystem" "/etc/passwd"
    (Engine.resolve engine "/etc/passwd");
  Engine.set_root engine (Some "/tmp/jide-engine-root");
  check_path "the empty path is the root" "/tmp/jide-engine-root"
    (Engine.resolve engine "")

(* A file the document requires that holds an error comes out of the analysis
   with its own uri and the text its spans index, so a caller publishes the
   error where the compiler printed it instead of on the document's lines.
   The checker is the test's, so the required list is a value written here and
   not a corpus. *)
let required_text = "u16 x = MLKEM_NOPE;\n"

let required_diagnostic =
  Jide_lang.Diagnostic.make ~code:"typing" ~severity:Jide_lang.Diagnostic.Error
    (Jide_lang.Span.make ~start_byte:8 ~end_byte:18)
    "unknown variable: `MLKEM_NOPE'"

let check_reports_required_in file ~context:_ ~arch:_ ~call_conv:_ ~file:_
    ~text:_ ~idirs:_ =
  {
    Jide_jasmin.Adapter.diagnostics = [];
    warnings = [];
    facts = Jide_jasmin.Facts.empty;
    required =
      [
        {
          Jide_jasmin.Adapter.file;
          text = required_text;
          diagnostics = [ required_diagnostic ];
          site = Some 0;
        };
      ];
  }

let test_a_required_file_keeps_its_own_uri () =
  let engine =
    Jide_engine.Engine.create
      ~check:(check_reports_required_in "/tmp/prelude.jinc")
      ()
  in
  let uri = uri_of "required" in
  open_text engine ~uri ~version:1 valid;
  (match Jide_engine.Engine.check engine ~uri with
  | None -> check "required: an analysis" false
  | Some analysis -> (
      check "required: the document has no diagnostic of its own"
        (analysis.Jide_engine.Engine.diagnostics = []);
      match analysis.Jide_engine.Engine.required with
      | [ file ] ->
          check "required: the file is named by its own uri"
            (file.Jide_engine.Engine.uri = "file:///tmp/prelude.jinc");
          check "required: it carries the text its spans index"
            (String.equal file.Jide_engine.Engine.text required_text);
          check "required: the diagnostic is carried"
            (messages file.Jide_engine.Engine.diagnostics
            = [ required_diagnostic.Jide_lang.Diagnostic.message ]);
          check "required: its range indexes the file's own text"
            (Jide_lang.Line_index.position_of_byte
               (Jide_lang.Line_index.create file.Jide_engine.Engine.text)
               required_diagnostic.Jide_lang.Diagnostic.span
                 .Jide_lang.Span.start_byte
            = { Jide_lang.Position.line = 0; character = 8 })
      | other ->
          check
            (Printf.sprintf "required: one file, got %d" (List.length other))
            false));
  (* The client has the file open, spelled its own way, which is the document
     it can apply an answer to: the engine names the file by that spelling and
     not by the one its path would give. *)
  let spelled = "file:///tmp/prelude file.jinc" in
  let engine =
    Jide_engine.Engine.create
      ~check:(check_reports_required_in "/tmp/prelude file.jinc")
      ()
  in
  open_text engine ~uri:spelled ~version:1 required_text;
  open_text engine ~uri ~version:1 valid;
  match Jide_engine.Engine.check engine ~uri with
  | Some analysis -> (
      match analysis.Jide_engine.Engine.required with
      | [ file ] ->
          check "required: an open file keeps the client's spelling"
            (file.Jide_engine.Engine.uri = spelled)
      | _ -> check "required: the open file is named" false)
  | None -> check "required: the open file is named" false

let () =
  test "change without open is ignored" test_change_without_open_is_ignored;
  test "change after close is ignored" test_change_after_close_is_ignored;
  test "open then change updates the document"
    test_open_then_change_updates_the_text;
  test "close removes the document" test_close_removes_the_document;
  test "the analysis key follows the text" test_analysis_key_follows_the_text;
  test "the analysis key without a document"
    test_analysis_key_without_a_document;
  test "a check reuses the analysis" test_check_reuses_the_analysis;
  test "a check without a document" test_check_without_a_document;
  test "a clean document" test_clean_document;
  test "a type error's span" test_type_error_span;
  test "a byte-order mark shifts the spans" test_bom_shifts_the_spans;
  test "a byte-order mark without an error" test_bom_without_an_error;
  test "a byte-order mark alone" test_byte_order_mark_alone;
  test "a span after a byte-order mark matches the plain text"
    test_span_after_a_bom_matches_the_plain_text;
  test "a warning becomes a diagnostic" test_warning_becomes_a_diagnostic;
  test "a warning after a byte-order mark" test_warning_after_a_bom;
  test "errors come before warnings" test_errors_come_before_warnings;
  test "a document over the size limit" test_over_the_size_limit;
  test "a document at the size limit" test_at_the_size_limit_is_analysed;
  test "the adapter admits what the engine admits"
    test_the_adapter_admits_what_the_engine_admits;
  test "an exception becomes an internal diagnostic"
    test_exception_becomes_an_internal_diagnostic;
  test "a failing check does not break the store"
    test_a_failing_check_does_not_break_the_store;
  test "two spellings of one uri" test_two_spellings_of_one_uri;
  test "settings" test_settings;
  test "the settings reach the checker" test_the_settings_reach_the_checker;
  test "a settings change forgets the analysis"
    test_a_settings_change_forgets_the_analysis;
  test "an exception in the context stage is internal"
    test_an_exception_in_the_context_stage_is_internal;
  test "a snapshot without an analysis" test_snapshot_without_an_analysis;
  test "a snapshot after a check" test_snapshot_after_a_check;
  test "the snapshot is the last analysis" test_snapshot_is_the_last_analysis;
  test "the snapshot of a broken file" test_snapshot_of_a_broken_file;
  test "a required file keeps its own uri"
    test_a_required_file_keeps_its_own_uri;
  test "open and read back" test_open_and_read_back;
  test "change and read the new text" test_change_and_read_the_new_text;
  test "close and read from disk" test_close_then_read_from_disk;
  test "two spellings of a URI are one document"
    test_uri_spellings_are_one_document;
  test "a missing file is a message" test_missing_file_is_a_message;
  test "a file keeps its bytes" test_file_keeps_its_bytes;
  test "reads are bounded to regular files" test_reads_are_bounded;
  test "a relative path resolves against the root" test_resolve_relative_path;
  test "a path that escapes the root is refused" test_resolve_refuses_escape;
  test "a symlink that leaves the root is refused"
    test_resolve_refuses_a_symlink_escape;
  test "resolve compares names byte for byte"
    test_resolve_compares_names_exactly;
  test "resolve handles the empty path and the root" test_resolve_special_paths;
  test "resolve without a root is refused" test_resolve_without_root;
  test "the analysis key follows a file" test_analysis_key_follows_a_file;
  if !failures > 0 then (
    Printf.printf "\n%d tests, %d failed checks\n" !tests !failures;
    exit 1)
  else Printf.printf "\n%d tests, no failed checks\n" !tests
