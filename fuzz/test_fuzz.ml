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

