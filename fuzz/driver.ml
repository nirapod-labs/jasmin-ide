(** The harness: one child per case, a bounded run, and one line per run.

    Each case runs in a child of its own ([Child.run]) with a wall clock over
    it, so a case that raises its way out of the library's handler, dies on a
    signal or never returns is a result that is printed rather than a lost
    run. The clock a caller sets with [--seconds] bounds the whole run; the
    case count with [--cases] bounds it exactly, which is how a run is made
    short in a test and how a failure is replayed. *)

type surface = Both | Text_only | Edit_only

type options = {
  seconds : float;
  seed : int;
  cases : int;
  corpus : string option;
  surface : surface;
}

let default_seconds = 120.0
let default_seed = 1
let default_surface = Both

(* A case that outlives this is a hang: a 64 KB unit of the corpus is checked
   in milliseconds, so ten seconds is two orders of magnitude of room. *)
let case_timeout = 10.0

let usage =
  "usage: fuzz [--seconds S] [--seed N] [--cases N] [--surface both|text|edit]\n\
  \            [--corpus DIR]\n\n\
   Write source texts (from the corpus when one is given, from the generator\n\
   otherwise) and check them, and drive the protocol state machine with opens,\n\
   changes, closes and requests the server does not serve. Every case runs in\n\
   a child with a wall clock over it.\n\n\
   options:\n\
  \  --seconds S    run for S seconds (default: 120)\n\
  \  --seed N       the seed of the generators (default: 1)\n\
  \  --cases N      run N cases exactly, whatever the clock says (default: 0,\n\
  \                 which runs until --seconds is up)\n\
  \  --surface S    text, edit or both (default: both)\n\
  \  --corpus DIR   read the .jazz units of DIR as texts to alter\n\
  \  -h, --help     print this text\n\n\
   exit status: 0 when every case held, 1 when one did not, and 2 when the\n\
   command line cannot be used. A case's stream comes from the seed and the\n\
   case's ordinal, so a failure is replayed with the seed it printed and at\n\
   least that many cases.\n"

let surface_name = function
  | Both -> "both"
  | Text_only -> "text"
  | Edit_only -> "edit"

let parse (arguments : string list) : (options, string) result =
  let seconds = ref default_seconds in
  let seed = ref default_seed in
  let cases = ref 0 in
  let corpus = ref None in
  let surface = ref default_surface in
  let problem = ref None in
  let number value =
    match float_of_string_opt value with
    | Some number when number >= 0.0 -> Some number
    | _ -> None
  in
  let rec walk = function
    | [] -> ()
    | ("-h" | "--help") :: _ -> problem := Some ""
    | "--seconds" :: value :: rest -> (
        match number value with
        | Some number ->
            seconds := number;
            walk rest
        | None -> problem := Some ("--seconds takes a number, not " ^ value))
    | "--seed" :: value :: rest -> (
        match int_of_string_opt value with
        | Some number ->
            seed := number;
            walk rest
        | None -> problem := Some ("--seed takes an integer, not " ^ value))
    | "--cases" :: value :: rest -> (
        match int_of_string_opt value with
        | Some number when number >= 0 ->
            cases := number;
            walk rest
        | _ -> problem := Some ("--cases takes a count, not " ^ value))
    | "--corpus" :: value :: rest ->
        corpus := Some value;
        walk rest
    | "--surface" :: value :: rest -> (
        match value with
        | "both" ->
            surface := Both;
            walk rest
        | "text" ->
            surface := Text_only;
            walk rest
        | "edit" ->
            surface := Edit_only;
            walk rest
        | _ ->
            problem := Some ("--surface takes both, text or edit, not " ^ value)
        )
    | argument :: _ -> problem := Some ("unknown argument " ^ argument)
  in
  walk arguments;
  match !problem with
  | Some "" -> Error ""
  | Some message -> Error message
  | None ->
      Ok
        {
          seconds = !seconds;
          seed = !seed;
          cases = !cases;
          corpus = !corpus;
          surface = !surface;
        }

let surfaces = function
  | Both -> [ Gen.Text; Gen.Edit ]
  | Text_only -> [ Gen.Text ]
  | Edit_only -> [ Gen.Edit ]

let surface_label = function Gen.Text -> "text" | Gen.Edit -> "edit"

let run_case ~(options : options) ~(corpus : string array) ~(case : int) :
    string =
  let work () =
    let rec first = function
      | [] -> ""
      | (surface : Gen.surface) :: rest -> (
          let st = Gen.make ~seed:options.seed ~surface ~case in
          let report =
            match surface with
            | Gen.Text -> Text.case ~corpus st
            | Gen.Edit -> Edit.case ~corpus st
          in
          match report with
          | "" -> first rest
          | report -> Printf.sprintf "%s: %s" (surface_label surface) report)
    in
    first (surfaces options.surface)
  in
  match Child.run ~timeout:case_timeout work with
  | Child.Passed -> ""
  | Child.Failed report -> report
  | Child.Hung ->
      Printf.sprintf "the case outlived the %.0f s clock" case_timeout
  | Child.Crashed report -> report

let report_failure ~(options : options) ~(case : int) ~(report : string) =
  Printf.printf "fuzz: FAIL seed %d case %d\nfuzz: %s\n" options.seed case
    report;
  Printf.printf "fuzz: replay with --seed %d --cases %d\n%!" options.seed
    (case + 1)

let run (options : options) : int =
  let corpus =
    match options.corpus with
    | Some directory -> Gen.corpus directory
    | None -> [||]
  in
  let start = Unix.gettimeofday () in
  let deadline = start +. options.seconds in
  (match options.corpus with
  | Some directory ->
      Printf.printf "fuzz: seed %d, surfaces %s, %d corpus texts from %s\n%!"
        options.seed
        (surface_name options.surface)
        (Array.length corpus) directory;
      if Array.length corpus = 0 then
        Printf.printf
          "fuzz: the corpus holds no .jazz unit; texts are generated\n%!"
  | None ->
      Printf.printf "fuzz: seed %d, surfaces %s, generated texts\n%!"
        options.seed
        (surface_name options.surface));
  let heartbeat = ref start in
  let rec loop case =
    let now = Unix.gettimeofday () in
    let finished =
      if options.cases > 0 then case >= options.cases else now >= deadline
    in
    if finished then case
    else
      let report = run_case ~options ~corpus ~case in
      if not (String.equal report "") then (
        report_failure ~options ~case ~report;
        exit 1);
      if now -. !heartbeat >= 5.0 then (
        Printf.printf "fuzz: %d cases, %.0f s\n%!" case (now -. start);
        heartbeat := now);
      loop (case + 1)
  in
  let cases = loop 0 in
  let elapsed = Unix.gettimeofday () -. start in
  let per_surface =
    match options.surface with
    | Both -> (cases, cases)
    | Text_only -> (cases, 0)
    | Edit_only -> (0, cases)
  in
  Printf.printf
    "fuzz: seed %d, surfaces %s, %d cases (text %d, edit %d), 0 failures, %.1fs\n\
     %!"
    options.seed
    (surface_name options.surface)
    cases (fst per_surface) (snd per_surface) elapsed;
  0

let main (argv : string list) : int =
  match parse argv with
  | Error "" ->
      print_string usage;
      2
  | Error message ->
      Printf.printf "fuzz: %s\n" message;
      print_string usage;
      2
  | Ok options -> run options
