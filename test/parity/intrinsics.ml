(** The intrinsic measurement: the engine's table against the
    compiler's own.

    The requirement is that the intrinsic list for each architecture equals
    [jasminc -arch <arch> -help-intrinsics]. The compiler half of that is
    read here from a real compiler binary, never from the linked library:
    [--compiler] names a command that takes the compiler's arguments (a
    wrapper that runs the compiler somewhere else works as well as a binary),
    and every answer below comes from running it.

    Two questions are asked, because the printed list answers only the first.

    - The names, and the suffix class each name is printed under. The
      compiler prints seven groups, one per shape of suffix (none, a plain
      size, a signed size, a zero/sign extension, one vector description, two
      vector descriptions, and the ARM flag and condition pair), and sorts
      each group. The engine derives the same class from each entry's suffix
      list, so a name that moved groups is a difference even when both sides
      list it.
    - The suffixes themselves. The printed list does not carry them, so each
      name is asked directly. For a name whose class is not the ARM pair, one
      probe writes a suffix of another class: [Pretyping.tt_prim] then raises
      [PrimWrongSuffix], whose message is the compiler printing that name's
      whole valid suffix list ([pretyping.ml], the [tt_prim] of a [PrimX86]
      name). For a name in the ARM pair the constructor is [PrimARM], which
      reports only "unknown primitive", so the four spellings the grammar
      allows (none, [S], [cc], [Scc]) are each asked for and the ones the
      compiler resolves are the accepted set.

    A probe is one compiler run, and a run costs a process; [--jobs] bounds how
    many run at once. Every run's source, its written name and the compiler's
    own words are kept when [--raw-dir] names a directory, so the evidence a
    difference rests on can be read back.

    Usage:

    {v
      intrinsics --compiler <command> [--arch <arch>]... [--jobs <n>]
                 [--raw-dir <dir>]
      intrinsics --selftest
    v}

    Exit status: 0 when every architecture's names and suffixes are equal,
    1 when one is not, 2 when the run cannot be made (no compiler, a compiler
    that printed something this program does not understand, a bad
    argument). *)

open Compiler_help

let usage =
  String.concat "\n"
    [
      "usage: intrinsics --compiler <command> [--arch <arch>]... [--jobs <n>]";
      "                  [--raw-dir <dir>]";
      "       intrinsics --selftest";
      "";
      "For each architecture, compare the compiler's own intrinsic list";
      "(`-arch <arch> -help-intrinsics`) and the suffix each name accepts with";
      "the engine's table (Jide_jasmin.Intrinsics.all).";
    ]

(* ------------------------------------------------------------------ *)
(* The class a name is printed under *)

(* [Help.show_intrinsics] groups by the suffix class of the constructor's
   first entry, and writes the seven groups in the order [headers] lists.
   This is that classification read back from a suffix string, which is the
   shape the interface documents: a plain size, a signed size, a zero/sign
   extension, one vector description, two vector descriptions, and the ARM
   pair (the empty string, [S], [cc] and [Scc]). *)
let class_of_suffix (suffix : string) =
  let is_digits text =
    String.length text > 0
    && String.for_all
         (fun character -> character >= '0' && character <= '9')
         text
  in
  let signs part =
    let found = ref [] in
    String.iteri
      (fun index character ->
        if character = 'u' || character = 's' then found := index :: !found)
      part;
    List.rev !found
  in
  let sub text from to_ = String.sub text from (to_ - from) in
  let class_of_part part =
    match signs part with
    | [] -> if is_digits part then 1 else -1
    | [ index ] when index = 0 ->
        if is_digits (sub part 1 (String.length part)) then 2 else -1
    | [ index ] ->
        if
          is_digits (sub part 0 index)
          && is_digits (sub part (index + 1) (String.length part))
        then 4
        else -1
    | [ first; second ]
      when first = 0
           && is_digits (sub part 1 second)
           && is_digits (sub part (second + 1) (String.length part)) ->
        3
    | _ -> -1
  in
  if String.equal suffix "" then 6
  else if List.mem suffix [ "S"; "cc"; "Scc" ] then 6
  else
    match String.split_on_char '_' suffix with
    | [ part ] -> class_of_part part
    | [ one; two ] ->
        if class_of_part one = 4 && class_of_part two = 4 then 5 else -1
    | _ -> -1

(* The class of a whole suffix list: none for an empty one, the common class
   when every suffix has the same shape, and -1 when the list mixes shapes a
   group cannot describe. *)
let class_of_suffixes = function
  | [] -> 0
  | first :: rest ->
      let class_ = class_of_suffix first in
      if List.for_all (fun suffix -> class_of_suffix suffix = class_) rest then
        class_
      else -1

(* ------------------------------------------------------------------ *)
(* Asking the compiler about one name *)

type probe =
  | Report  (** write a suffix of another class; the answer is the list *)
  | Spelling of string
      (** write this whole name; the answer is accepted or not *)

(* The suffix a [Report] probe writes: one class away from the name's own, so
   that [tt_prim] answers with the valid list instead of resolving. A name
   with no suffix list answers "accepts no size annotation", which is the
   empty list. *)
let probe_suffix = function
  | 1 -> "s16" (* a signed size, against a plain-size list *)
  | 4 | 5 -> "s16" (* against a vector-description list *)
  | _ -> "64" (* against a list of signed sizes, an extension or nothing *)

let source ~(written : string) =
  Printf.sprintf
    "export fn probe(reg u64 x) -> reg u64 {\n\
    \  reg u64 y;\n\
    \  y = #%s(x);\n\
    \  return y;\n\
     }\n"
    written

let contains ~(needle : string) (haystack : string) =
  let rec find index =
    if index + String.length needle > String.length haystack then false
    else if
      String.equal (String.sub haystack index (String.length needle)) needle
    then true
    else find (index + 1)
  in
  find 0

(* The suffixes [Pretyping.pp_suffix] writes, read back from the compiler's
   own message: everything after the colon is the list, comma separated and
   wrapped over as many lines as the formatter needed. *)
let suffixes_of_message (text : string) =
  let marker = "only accepts the following size annotations:" in
  let rec find index =
    if index + String.length marker > String.length text then None
    else if String.equal (String.sub text index (String.length marker)) marker
    then Some (index + String.length marker)
    else find (index + 1)
  in
  match find 0 with
  | None -> None
  | Some start ->
      let rest = String.sub text start (String.length text - start) in
      let words =
        String.split_on_char ',' rest
        |> List.map (fun part ->
               String.to_seq part
               |> Seq.filter (fun character ->
                      match character with
                      | ' ' | '\t' | '\r' | '\n' | '"' -> false
                      | _ -> true)
               |> String.of_seq)
        |> List.filter (fun word -> String.length word > 0)
      in
      Some
        (List.map
           (fun suffix ->
             if String.length suffix > 0 && suffix.[0] = '_' then
               String.sub suffix 1 (String.length suffix - 1)
             else suffix)
           words)

type answer =
  [ `Known of string list
    (** the compiler printed the name's whole valid suffix list *)
  | `Unknown  (** "unknown primitive": this spelling resolves to nothing *)
  | `Other of string  (** the run said something else; [string] is why *)
  | `Accepted  (** a spelling probe resolved: the compiler accepts it *)
  | `Refused  (** a spelling probe the compiler calls unknown *)
  | `Broken of string  (** the wrapper failed, so nothing was measured *) ]

(* [label_of_answer] is an answer as the raw record writes it. It is total over
   [answer] on purpose: the record keeps every probe, spelling probes included,
   so that a difference can be read back against the run that produced it. *)
let label_of_answer (answer : answer) =
  match answer with
  | `Known suffixes -> "known:" ^ String.concat "," suffixes
  | `Unknown -> "unknown"
  | `Other reason -> "other:" ^ reason
  | `Accepted -> "accepted"
  | `Refused -> "refused"
  | `Broken reason -> "broken:" ^ reason

(* [classify ~probe ~status ~output] is what one run answered. A status other
   than 0 or 1 is not the compiler speaking: it is the wrapper failing, and it
   is reported rather than read as an answer. *)
let classify ~(probe : probe) ~(status : int) ~(output : string) : answer =
  match probe with
  | Spelling _ -> (
      match status with
      | 0 -> `Accepted
      | 1 when contains ~needle:"unknown primitive" output -> `Refused
      | 1 -> `Accepted
      | other -> `Broken (Printf.sprintf "exit %d" other))
  | Report -> (
      match status with
      | 0 -> `Other "the probe resolved, so the compiler printed no list"
      | 1 when contains ~needle:"unknown primitive" output -> `Unknown
      | 1 -> (
          match suffixes_of_message output with
          | Some suffixes -> `Known suffixes
          | None ->
              if contains ~needle:"accepts no size annotation" output then
                `Known []
              else
                let message =
                  match String.split_on_char '\n' output with
                  | [] -> "no message"
                  | lines -> (
                      let lines =
                        List.filter
                          (fun line -> not (String.equal line ""))
                          lines
                      in
                      match lines with
                      | [] -> "no message"
                      | line :: _ -> String.trim line)
                in
                `Other message)
      | other -> `Broken (Printf.sprintf "exit %d" other))

(* [run_pool ~jobs ~spawn ~read_answer tasks] runs every task, at most [jobs]
   at once, and returns their answers in the tasks' own order. [spawn task]
   starts one process and answers its pid and the file its output goes to. *)
let run_pool ~(jobs : int) ~(spawn : 'task -> int * string)
    ~(read_answer : 'task -> int -> string -> 'result) (tasks : 'task list) =
  let tasks = Array.of_list tasks in
  let answers = Array.make (Array.length tasks) None in
  let next = ref 0 and running = ref [] in
  let total = Array.length tasks in
  let fill () =
    while List.length !running < max 1 jobs && !next < total do
      let index = !next in
      incr next;
      let pid, log = spawn tasks.(index) in
      running := (pid, index, log) :: !running
    done
  in
  let rec pump () =
    fill ();
    if !running <> [] then
      let pid, status = Unix.waitpid [] (-1) in
      match List.find_opt (fun (other, _, _) -> other = pid) !running with
      | None -> pump ()
      | Some (_, index, log) ->
          running := List.filter (fun (other, _, _) -> other <> pid) !running;
          let output = try read_file log with Sys_error _ -> "" in
          answers.(index) <-
            Some (read_answer tasks.(index) (temp_suffix status) output);
          pump ()
  in
  pump ();
  (* [pump] returns only when every task has been spawned and reaped, and each
     reaping writes its own index, so no slot is left empty. *)
  Array.to_list answers
  |> List.map (function
       | Some answer -> answer
       | None -> invalid_arg "intrinsics: a task was never reaped")

(* ------------------------------------------------------------------ *)
(* One architecture *)

type entry = { name : string; suffixes : string list; class_ : int }
type difference = { subject : string; compiler : string; engine : string }

let sorted names = List.sort_uniq String.compare names
let print_set names = "[" ^ String.concat "; " names ^ "]"

(* The probes one name needs, before any answer is read. A name in the ARM
   class is asked for its four spellings; every other name is asked once, with
   a suffix of another class. *)
let planned_probes ~(class_ : int) ~(name : string) =
  if class_ = 6 then
    List.map
      (fun suffix -> (Spelling suffix, name ^ suffix))
      [ ""; "S"; "cc"; "Scc" ]
  else [ (Report, name ^ "_" ^ probe_suffix class_) ]

(* [probe_all] runs every planned probe of every name and answers, per name,
   the compiler's suffix list or why it was not measured. *)
let probe_all ~(compiler : string) ~(jobs : int) ~(arch : string)
    ~(entries : entry list) ~(raw_dir : string option) =
  let tasks =
    List.concat_map
      (fun (entry : entry) ->
        List.map
          (fun (probe, written) -> (entry.name, entry.class_, probe, written))
          (planned_probes ~class_:entry.class_ ~name:entry.name))
      entries
  in
  let answers : (answer * int) list =
    run_pool ~jobs
      ~spawn:(fun (_, _, _, written) ->
        let path =
          remember
            (Filename.temp_file (Printf.sprintf "probe-%s-" arch) ".jazz")
        in
        write_file path (source ~written);
        let log = remember (Filename.temp_file "probe-" ".log") in
        let out =
          Unix.openfile log [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
        in
        let devnull = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
        let argv = [| compiler; "-arch"; arch; "-until_typing"; path |] in
        let pid = Unix.create_process compiler argv devnull out out in
        Unix.close out;
        Unix.close devnull;
        (pid, log))
      ~read_answer:(fun (_, _, probe, _) status output ->
        (classify ~probe ~status ~output, status))
      tasks
  in
  let table = Hashtbl.create 128 in
  List.iter2
    (fun (name, class_, probe, written) (answer, status) ->
      let previous = Option.value (Hashtbl.find_opt table name) ~default:[] in
      Hashtbl.replace table name
        ((class_, probe, written, answer, status) :: previous))
    tasks answers;
  (match raw_dir with
  | None -> ()
  | Some directory ->
      let lines =
        List.map2
          (fun (name, class_, probe, written) (answer, status) ->
            Printf.sprintf "%s\t%d\t%s\t%s\t%d\t%s" name class_
              (match probe with
              | Report -> "report"
              | Spelling suffix -> "spelling:" ^ suffix)
              written status (label_of_answer answer))
          tasks answers
      in
      write_file
        (Filename.concat directory ("probes-" ^ arch ^ ".tsv"))
        (String.concat "\n" lines ^ "\n"));
  let suffixes_of name =
    let own = List.rev (Hashtbl.find table name) in
    match
      List.find_map
        (fun (_, probe, _, answer, _) ->
          match (probe, answer) with
          | Report, `Known suffixes -> Some suffixes
          | _ -> None)
        own
    with
    | Some suffixes -> `Known suffixes
    | None -> (
        let spellings =
          List.filter_map
            (fun (_, probe, _, answer, _) ->
              match (probe, answer) with
              | Spelling suffix, `Accepted -> Some suffix
              | _ -> None)
            own
        in
        if spellings <> [] then `Known spellings
        else
          match
            List.find_opt
              (fun (_, _, _, answer, _) ->
                match answer with `Unknown -> true | _ -> false)
              own
          with
          | Some _ -> `Unknown
          | None -> (
              match own with
              | (_, _, _, `Other reason, _) :: _ -> `Other reason
              | (_, _, _, `Broken reason, _) :: _ -> `Other ("broken: " ^ reason)
              | _ -> `Other "no answer"))
  in
  List.map (fun (entry : entry) -> (entry.name, suffixes_of entry.name)) entries

let compare_arch ~(compiler : string) ~(jobs : int) ~(arch : string)
    ~(raw_dir : string option) =
  let help_text = help_of ~compiler ~arch in
  (match raw_dir with
  | None -> ()
  | Some directory ->
      write_file (Filename.concat directory ("help-" ^ arch ^ ".txt")) help_text);
  let groups = groups_of_help help_text in
  check_headers ~arch groups;
  let printed =
    List.concat
      (List.mapi
         (fun class_ (_, names) -> List.map (fun name -> (name, class_)) names)
         groups)
  in
  let entries =
    List.map
      (fun (entry : Jide_jasmin.Intrinsics.t) ->
        {
          name = entry.Jide_jasmin.Intrinsics.name;
          suffixes = entry.Jide_jasmin.Intrinsics.suffixes;
          class_ = class_of_suffixes entry.Jide_jasmin.Intrinsics.suffixes;
        })
      (Jide_jasmin.Intrinsics.all ~arch)
  in
  let printed_names = List.map fst printed
  and engine_names = List.map (fun (entry : entry) -> entry.name) entries in
  let differences = ref [] in
  let add subject compiler engine =
    differences := { subject; compiler; engine } :: !differences
  in
  List.iter
    (fun name ->
      if not (List.mem name engine_names) then
        add ("name " ^ name) "listed" "absent")
    printed_names;
  List.iter
    (fun name ->
      if not (List.mem name printed_names) then
        add ("name " ^ name) "absent" "listed")
    engine_names;
  let moved =
    List.filter
      (fun name ->
        match
          ( List.assoc_opt name printed,
            List.find_opt
              (fun (entry : entry) -> String.equal entry.name name)
              entries )
        with
        | Some compiler_class, Some entry -> compiler_class <> entry.class_
        | _ -> false)
      engine_names
  in
  List.iter
    (fun name ->
      let compiler_class = Option.get (List.assoc_opt name printed) in
      let entry =
        Option.get
          (List.find_opt
             (fun (entry : entry) -> String.equal entry.name name)
             entries)
      in
      add ("group " ^ name)
        ("group " ^ string_of_int compiler_class)
        ("group " ^ string_of_int entry.class_))
    moved;
  let answers = probe_all ~compiler ~jobs ~arch ~entries ~raw_dir in
  let compared = ref 0 and equal = ref 0 and not_compared = ref 0 in
  let suffix_differences = ref [] in
  List.iter
    (fun (entry : entry) ->
      match List.assoc_opt entry.name answers with
      | Some (`Known compiler_suffixes) ->
          incr compared;
          if sorted compiler_suffixes = sorted entry.suffixes then incr equal
          else
            suffix_differences :=
              {
                subject = "suffixes " ^ entry.name;
                compiler = print_set compiler_suffixes;
                engine = print_set entry.suffixes;
              }
              :: !suffix_differences
      | Some `Unknown ->
          incr not_compared;
          suffix_differences :=
            {
              subject = "suffixes " ^ entry.name;
              compiler = "not measured: the compiler does not know the name";
              engine = print_set entry.suffixes;
            }
            :: !suffix_differences
      | Some (`Other reason) ->
          incr not_compared;
          suffix_differences :=
            {
              subject = "suffixes " ^ entry.name;
              compiler = "not measured: " ^ reason;
              engine = print_set entry.suffixes;
            }
            :: !suffix_differences
      | None ->
          incr not_compared;
          suffix_differences :=
            {
              subject = "suffixes " ^ entry.name;
              compiler = "not measured: no probe";
              engine = print_set entry.suffixes;
            }
            :: !suffix_differences)
    entries;
  let differences = List.rev (!differences @ !suffix_differences) in
  let same_bytes =
    String.equal (Jide_jasmin.Intrinsics.compiler_help ~arch) help_text
  in
  let missing_from_engine =
    List.length
      (List.filter (fun name -> not (List.mem name engine_names)) printed_names)
  and missing_from_compiler =
    List.length
      (List.filter (fun name -> not (List.mem name printed_names)) engine_names)
  in
  Printf.printf "== %s\n%!" arch;
  Printf.printf
    "names: the compiler prints %d in %d groups, the engine lists %d; missing \
     from the compiler %d, missing from the engine %d, printed in another \
     group %d\n\
     %!"
    (List.length printed_names)
    (List.length groups) (List.length engine_names) missing_from_compiler
    missing_from_engine (List.length moved);
  Printf.printf
    "suffixes: %d names probed, %d equal, %d different, %d not measured\n%!"
    !compared !equal
    (List.length !suffix_differences)
    !not_compared;
  Printf.printf "the linked library's own rendering: %s\n%!"
    (if same_bytes then "the same bytes as the compiler's"
     else "DIFFERENT from the compiler's");
  List.iter
    (fun difference ->
      Printf.printf "different %s: compiler %s, engine %s\n%!"
        difference.subject difference.compiler difference.engine)
    differences;
  flush stdout;
  ( differences <> [] || (not same_bytes) || !not_compared > 0,
    List.length printed_names,
    List.length engine_names,
    !compared,
    !equal )

(* ------------------------------------------------------------------ *)
(* The self-test *)

let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL %s\n%!" name)

let selftest () =
  let sample_help =
    String.concat "\n"
      [
        "Intrinsics accepting no size suffix:";
        " - CLC";
        " - swap";
        "";
        "Intrinsics accepting one optional size suffix, e.g., “_64”:";
        " - ADD";
        " - copy";
        "";
      ]
  in
  (match groups_of_help sample_help with
  | [ (first, [ "CLC"; "swap" ]); (second, [ "ADD"; "copy" ]) ] ->
      check "parse: the first header" (String.equal first (List.nth headers 0));
      check "parse: the second header"
        (String.equal second (List.nth headers 1))
  | other ->
      check
        (Printf.sprintf "parse: two groups (got %d)" (List.length other))
        false);
  check "classify: a wrapped suffix list"
    (classify ~probe:Report ~status:1
       ~output:
         "\"f.jazz\", line 3 (7-14):\n\
          typing error: primitive “ADD” only accepts the following size \
          annotations: _64,\n\
          _32,\n\
          _16,\n\
          _8\n"
    = `Known [ "64"; "32"; "16"; "8" ]);
  check "classify: no size annotation"
    (classify ~probe:Report ~status:1
       ~output:
         "\"f.jazz\", line 3 (7-13):\n\
          typing error: primitive accepts no size annotation: `CLC'\n"
    = `Known []);
  check "classify: an unknown name"
    (classify ~probe:Report ~status:1
       ~output:
         "\"f.jazz\", line 3 (7-14):\n\
          typing error: unknown primitive: \"add_s16\"\n"
    = `Unknown);
  check "classify: a spelling refused"
    (classify ~probe:(Spelling "S") ~status:1
       ~output:
         "typing error: unknown primitive: \"MLAS\" (this mnemonic cannot set \
          flags)\n"
    = `Refused);
  check "classify: a spelling accepted but ill-typed"
    (classify ~probe:(Spelling "") ~status:1
       ~output:
         "typing error: invalid number of arguments, 1 provided instead of 2\n"
    = `Accepted);
  check "classify: a wrapper that failed"
    (match classify ~probe:Report ~status:127 ~output:"wrapper: not found" with
    | `Broken _ -> true
    | _ -> false);
  check "class: none" (class_of_suffixes [] = 0);
  check "class: a plain size" (class_of_suffixes [ "64"; "32" ] = 1);
  check "class: a signed size" (class_of_suffixes [ "s16" ] = 2);
  check "class: a zero/sign extension" (class_of_suffixes [ "u32u16" ] = 3);
  check "class: one vector" (class_of_suffixes [ "4u64" ] = 4);
  check "class: two vectors" (class_of_suffixes [ "2u16_2u64" ] = 5);
  check "class: the probe suffix is never the name's own class"
    (List.for_all
       (fun class_ -> class_of_suffixes [ probe_suffix class_ ] <> class_)
       [ 0; 1; 2; 3; 4; 5 ]);
  (* The planned probes: a name in the ARM class has four spellings, every
     other name one report probe. *)
  check "probe plan: the ARM class"
    (List.map snd (planned_probes ~class_:6 ~name:"ADD")
    = [ "ADD"; "ADDS"; "ADDcc"; "ADDScc" ]);
  check "probe plan: a plain size list"
    (List.map snd (planned_probes ~class_:1 ~name:"ADD") = [ "ADD_s16" ]);
  check "probe plan: no suffix list"
    (List.map snd (planned_probes ~class_:0 ~name:"CLC") = [ "CLC_64" ]);
  (* The comparison itself: the same table on both sides differs in nothing,
     and one name removed or one suffix changed is a difference. *)
  let table = [ ("ADD", [ "64"; "32" ]); ("CLC", []) ] in
  let differs left right =
    sorted (List.map fst left) <> sorted (List.map fst right)
    || List.exists
         (fun (name, suffixes) ->
           match List.assoc_opt name right with
           | Some other -> sorted suffixes <> sorted other
           | None -> true)
         left
  in
  check "compare: the same table" (not (differs table table));
  check "compare: a missing name" (differs table (List.tl table));
  check "compare: another suffix list"
    (differs table ([ ("ADD", [ "64" ]) ] @ List.tl table));
  if !failures = 0 then Printf.printf "intrinsics: selftest passed\n%!"
  else (
    Printf.printf "intrinsics: %d selftest failure(s)\n%!" !failures;
    exit 1)

(* ------------------------------------------------------------------ *)
(* The command line *)

let () =
  let compiler = ref None and arches = ref [] and jobs = ref 4 in
  let raw_dir = ref None and selftest_requested = ref false in
  let spec =
    [
      ( "--compiler",
        Arg.String (fun command -> compiler := Some command),
        "the compiler command to run" );
      ( "--arch",
        Arg.String (fun arch -> arches := arch :: !arches),
        "an architecture (default: every one)" );
      ("--jobs", Arg.Set_int jobs, "how many compiler runs at once (default 4)");
      ( "--raw-dir",
        Arg.String (fun directory -> raw_dir := Some directory),
        "write the compiler's own words here" );
      ( "--selftest",
        Arg.Set selftest_requested,
        "check this program's own plumbing" );
    ]
  in
  let anonymous argument =
    raise (Arg.Bad ("unexpected argument: " ^ argument))
  in
  Arg.parse spec anonymous usage;
  if !selftest_requested then selftest ()
  else
    match !compiler with
    | None ->
        prerr_endline usage;
        exit 2
    | Some compiler ->
        let arches =
          match List.rev !arches with
          | [] -> Jide_jasmin.Intrinsics.arch_names
          | arches -> arches
        in
        let cleanup () =
          List.iter
            (fun path -> try Sys.remove path with Sys_error _ -> ())
            !scratch
        in
        (match !raw_dir with
        | Some directory when not (Sys.file_exists directory) ->
            Sys.mkdir directory 0o755
        | _ -> ());
        let status =
          try
            let results =
              List.map
                (fun arch ->
                  compare_arch ~compiler ~jobs:!jobs ~arch ~raw_dir:!raw_dir)
                arches
            in
            let failed =
              List.exists (fun (failed, _, _, _, _) -> failed) results
            in
            let sum pick =
              List.fold_left (fun sum result -> sum + pick result) 0 results
            in
            let names_compiler = sum (fun (_, c, _, _, _) -> c)
            and names_engine = sum (fun (_, _, e, _, _) -> e)
            and compared = sum (fun (_, _, _, c, _) -> c)
            and equal = sum (fun (_, _, _, _, e) -> e) in
            Printf.printf
              "intrinsics: %d architectures, %d names the compiler prints, %d \
               the engine lists; %d names probed, %d with the same suffix list\n\
               %!"
              (List.length results) names_compiler names_engine compared equal;
            if failed then (
              Printf.printf
                "intrinsics: the engine's table is not the compiler's\n%!";
              1)
            else (
              Printf.printf
                "intrinsics: every name and suffix list is the compiler's own\n\
                 %!";
              0)
          with
          | Unparsable message ->
              Printf.eprintf "intrinsics: %s\n%!" message;
              2
          | exn ->
              Printf.eprintf "intrinsics: the run raised: %s\n%!"
                (Printexc.to_string exn);
              2
        in
        cleanup ();
        exit status
