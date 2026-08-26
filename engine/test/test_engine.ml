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
        (Jide_engine.Engine.analysis_key engine ~uri <> same)

let test_analysis_key_without_a_document () =
  let engine = create () in
  check "key: no document, no key"
    (Jide_engine.Engine.analysis_key engine ~uri:(uri_of "absent") = None)

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
  let check_raises ~arch:_ ~call_conv:_ ~file:_ ~text:_ ~idirs:_ =
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
  let check_raises ~arch:_ ~call_conv:_ ~file:_ ~text:_ ~idirs:_ =
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
  let check_captures ~arch:_ ~call_conv:_ ~file:_ ~text:_ ~idirs =
    seen := idirs;
    {
      Jide_jasmin.Adapter.diagnostics = [];
      warnings = [];
      facts = Jide_jasmin.Facts.empty;
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
  let check_reports ~arch:_ ~call_conv:_ ~file:_ ~text:_ ~idirs =
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
  let check_is_fine ~arch:_ ~call_conv:_ ~file:_ ~text:_ ~idirs:_ =
    {
      Jide_jasmin.Adapter.diagnostics = [];
      warnings = [];
      facts = Jide_jasmin.Facts.empty;
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

let () =
  test_change_without_open_is_ignored ();
  test_change_after_close_is_ignored ();
  test_open_then_change_updates_the_text ();
  test_close_removes_the_document ();
  test_analysis_key_follows_the_text ();
  test_analysis_key_without_a_document ();
  test_check_reuses_the_analysis ();
  test_check_without_a_document ();
  test_clean_document ();
  test_type_error_span ();
  test_bom_shifts_the_spans ();
  test_bom_without_an_error ();
  test_byte_order_mark_alone ();
  test_span_after_a_bom_matches_the_plain_text ();
  test_warning_becomes_a_diagnostic ();
  test_warning_after_a_bom ();
  test_errors_come_before_warnings ();
  test_over_the_size_limit ();
  test_at_the_size_limit_is_analysed ();
  test_the_adapter_admits_what_the_engine_admits ();
  test_exception_becomes_an_internal_diagnostic ();
  test_a_failing_check_does_not_break_the_store ();
  test_two_spellings_of_one_uri ();
  test_settings ();
  test_the_settings_reach_the_checker ();
  test_a_settings_change_forgets_the_analysis ();
  test_an_exception_in_the_context_stage_is_internal ();
  if !failures = 0 then print_endline "all engine tests passed"
  else (
    Printf.printf "%d engine test(s) failed\n" !failures;
    exit 1)
