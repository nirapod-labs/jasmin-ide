let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let text_report ?(corpus = [||]) ~seed case =
  let st = Jide_fuzz.Gen.make ~seed ~surface:Jide_fuzz.Gen.Text ~case in
  Jide_fuzz.Text.case ~corpus st

let edit_report ?(corpus = [||]) ~seed case =
  let st = Jide_fuzz.Gen.make ~seed ~surface:Jide_fuzz.Gen.Edit ~case in
  Jide_fuzz.Edit.case ~corpus st

let outcome_name = function
  | Jide_fuzz.Child.Passed -> "passed"
  | Jide_fuzz.Child.Failed _ -> "failed"
  | Jide_fuzz.Child.Hung -> "hung"
  | Jide_fuzz.Child.Crashed _ -> "crashed"

let test_the_stream_is_the_seed_and_the_ordinal () =
  let first = Jide_fuzz.Gen.make ~seed:1 ~surface:Jide_fuzz.Gen.Text ~case:3 in
  let again = Jide_fuzz.Gen.make ~seed:1 ~surface:Jide_fuzz.Gen.Text ~case:3 in
  check "case 3 of seed 1 is the same text twice"
    (String.equal (Jide_fuzz.Gen.source first) (Jide_fuzz.Gen.source again));
  let other_seed =
    Jide_fuzz.Gen.make ~seed:2 ~surface:Jide_fuzz.Gen.Text ~case:3
  in
  check "another seed is another text"
    (not
       (String.equal
          (Jide_fuzz.Gen.source again)
          (Jide_fuzz.Gen.source other_seed)));
  let other_case =
    Jide_fuzz.Gen.make ~seed:1 ~surface:Jide_fuzz.Gen.Text ~case:4
  in
  check "another case is another text"
    (not
       (String.equal
          (Jide_fuzz.Gen.source again)
          (Jide_fuzz.Gen.source other_case)));
  let edit = Jide_fuzz.Gen.make ~seed:1 ~surface:Jide_fuzz.Gen.Edit ~case:3 in
  check "the two surfaces do not share a stream"
    (not
       (String.equal
          (Jide_fuzz.Gen.document_text edit)
          (Jide_fuzz.Gen.source again)))

let test_the_generators_produce_text () =
  let st = Jide_fuzz.Gen.make ~seed:9 ~surface:Jide_fuzz.Gen.Text ~case:0 in
  check "a program is not empty" (String.length (Jide_fuzz.Gen.program st) > 0);
  check "junk is not empty" (String.length (Jide_fuzz.Gen.junk st) > 0);
  let texts = List.init 40 (fun _ -> Jide_fuzz.Gen.source st) in
  check "the generator varies its texts"
    (List.length (List.sort_uniq String.compare texts) > 20)

let test_a_generated_program_parses () =
  let st = Jide_fuzz.Gen.make ~seed:7 ~surface:Jide_fuzz.Gen.Text ~case:0 in
  let program = Jide_fuzz.Gen.program st in
  let engine = Jide_engine.Engine.create () in
  let uri = "untitled:fuzz/generated.jazz" in
  Jide_engine.Engine.open_document engine ~uri ~version:1 ~text:program;
  match Jide_engine.Engine.check engine ~uri with
  | None -> check "the engine analysed the generated program" false
  | Some analysis ->
      let parse_errors =
        List.filter
          (fun (diagnostic : Jide_lang.Diagnostic.t) ->
            match diagnostic.Jide_lang.Diagnostic.code with
            | Some "parse" -> true
            | _ -> false)
          analysis.Jide_engine.Engine.diagnostics
      in
      check "a generated program is not a parse error" (parse_errors = [])

let test_the_text_surface_holds_on_fixed_cases () =
  for case = 0 to 40 do
    let report = text_report ~seed:1 case in
    check
      (Printf.sprintf "text case %d of seed 1 holds: %s" case report)
      (String.equal report "")
  done

let test_the_edit_surface_holds_on_fixed_cases () =
  for case = 0 to 40 do
    let report = edit_report ~seed:1 case in
    check
      (Printf.sprintf "edit case %d of seed 1 holds: %s" case report)
      (String.equal report "")
  done

let test_a_case_that_holds_is_passed () =
  check "a case that reports nothing passed"
    (outcome_name (Jide_fuzz.Child.run ~timeout:5.0 (fun () -> "")) = "passed")

let test_a_failing_case_reports_its_line () =
  match
    Jide_fuzz.Child.run ~timeout:5.0 (fun () -> "the property does not hold")
  with
  | Jide_fuzz.Child.Failed report ->
      check "the report is the case's own line"
        (String.equal report "the property does not hold")
  | outcome -> check ("a failing case failed: " ^ outcome_name outcome) false

let test_an_exception_is_a_failure () =
  match Jide_fuzz.Child.run ~timeout:5.0 (fun () -> raise Not_found) with
  | Jide_fuzz.Child.Failed report ->
      check "the report names the exception" (String.length report > 0)
  | outcome ->
      check ("an exception is a failure: " ^ outcome_name outcome) false

let test_an_exit_is_a_crash () =
  match Jide_fuzz.Child.run ~timeout:5.0 (fun () -> exit 2) with
  | Jide_fuzz.Child.Crashed report ->
      check "the report names the exit code" (String.length report > 0)
  | outcome -> check ("an exit is a crash: " ^ outcome_name outcome) false

let test_a_signal_is_a_crash () =
  match
    Jide_fuzz.Child.run ~timeout:5.0 (fun () ->
        Unix.kill (Unix.getpid ()) Sys.sigterm;
        Unix.sleep 5;
        "")
  with
  | Jide_fuzz.Child.Crashed report ->
      check "the report names the signal" (String.length report > 0)
  | outcome -> check ("a signal is a crash: " ^ outcome_name outcome) false

let test_a_hang_is_killed () =
  let started = Unix.gettimeofday () in
  match
    Jide_fuzz.Child.run ~timeout:0.3 (fun () ->
        Unix.sleep 10;
        "")
  with
  | Jide_fuzz.Child.Hung ->
      check "the hang was killed before its own clock ran out"
        (Unix.gettimeofday () -. started < 5.0)
  | outcome -> check ("a hang is a hang: " ^ outcome_name outcome) false

let test_the_command_line () =
  let parse = Jide_fuzz.Driver.parse in
  (match parse [] with
  | Ok options ->
      check "the defaults are a two-minute run of seed 1"
        (options.Jide_fuzz.Driver.seconds = 120.0
        && options.Jide_fuzz.Driver.seed = 1
        && options.Jide_fuzz.Driver.cases = 0
        && options.Jide_fuzz.Driver.surface = Jide_fuzz.Driver.Both
        && options.Jide_fuzz.Driver.corpus = None)
  | Error message -> check ("no arguments parses: " ^ message) false);
  (match parse [ "--seed"; "12"; "--cases"; "3"; "--surface"; "edit" ] with
  | Ok options ->
      check "the seed, the count and the surface are read"
        (options.Jide_fuzz.Driver.seed = 12
        && options.Jide_fuzz.Driver.cases = 3
        && options.Jide_fuzz.Driver.surface = Jide_fuzz.Driver.Edit_only)
  | Error message -> check ("a full command line parses: " ^ message) false);
  (match parse [ "--corpus"; "somewhere" ] with
  | Ok options ->
      check "the corpus is read"
        (options.Jide_fuzz.Driver.corpus = Some "somewhere")
  | Error message -> check ("a corpus parses: " ^ message) false);
  check "a seed that is not a number is refused"
    (match parse [ "--seed"; "one" ] with Error _ -> true | Ok _ -> false);
  check "a count that is not a number is refused"
    (match parse [ "--cases"; "many" ] with Error _ -> true | Ok _ -> false);
  check "a surface that does not exist is refused"
    (match parse [ "--surface"; "wire" ] with Error _ -> true | Ok _ -> false);
  check "an unknown argument is refused"
    (match parse [ "--quick" ] with Error _ -> true | Ok _ -> false);
  check "help is not a run"
    (match parse [ "--help" ] with Error "" -> true | _ -> false)

let () =
  test_the_stream_is_the_seed_and_the_ordinal ();
  test_the_generators_produce_text ();
  test_a_generated_program_parses ();
  test_the_text_surface_holds_on_fixed_cases ();
  test_the_edit_surface_holds_on_fixed_cases ();
  test_a_case_that_holds_is_passed ();
  test_a_failing_case_reports_its_line ();
  test_an_exception_is_a_failure ();
  test_an_exit_is_a_crash ();
  test_a_signal_is_a_crash ();
  test_a_hang_is_killed ();
  test_the_command_line ();
  if !failures = 0 then print_endline "all fuzz tests passed"
  else (
    Printf.printf "%d fuzz test(s) failed\n" !failures;
    exit 1)
