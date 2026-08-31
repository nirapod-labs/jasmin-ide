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

(* The snapshot: what the last check built, read back without a second check.
   These four tests are the contract the completion and signature-help lanes
   are written against, so each of them names one half of it: nothing is
   analysed to answer, the answer is the last analysis's, it survives a broken
   file, and a caller can see when it is stale. *)

(* A checker that counts how many times it was called, so "no check ran" is a
   number and not an inference. Its result is empty, so the assertions below
   are about the index and the resolver, which are built from the text and do
   not read the checker's output. *)
let counting_check counter ~arch:_ ~call_conv:_ ~file:_ ~text:_ ~idirs:_ =
  incr counter;
  {
    Jide_jasmin.Adapter.diagnostics = [];
    warnings = [];
    facts = Jide_jasmin.Facts.empty;
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
        Jide_engine.Complete.completion ~index ~resolve ~byte:at ()
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
  test_snapshot_without_an_analysis ();
  test_snapshot_after_a_check ();
  test_snapshot_is_the_last_analysis ();
  test_snapshot_of_a_broken_file ();
  if !failures = 0 then print_endline "all engine tests passed"
  else (
    Printf.printf "%d engine test(s) failed\n" !failures;
    exit 1)
