(** The recovery mutation harness.

    For a deterministic sample of a Jasmin corpus, mutate one token at a
    time, run each mutated text through the adapter, and print the three
    criteria of the recovery gate with the distribution behind them:

    - no exception or hang;
    - the outline of the items away from the edit unchanged;
    - at most two errors per single-token mutation.

    The corpus is an argument. Nothing about this host or about this lab is
    written into the program, and a run writes nothing: it prints.

    Every check runs in its own process with a wall-clock budget, so a hang
    is a measurement and not the end of the run. *)

let usage =
  String.concat "\n"
    [
      "usage: mutation --corpus DIR [--corpus DIR]... [options]";
      "";
      "Mutate a corpus one token at a time and print the recovery criteria.";
      "";
      "options:";
      "  --corpus DIR        a directory of .jazz files, searched recursively";
      "                      (repeatable; at least one is required)";
      "  DIR                 a corpus directory, as --corpus";
      "  --sample N          files to mutate, spread over the clean files";
      "                      (default 40)";
      "  --sites N           mutation sites per family per file, spread over";
      "                      the file (default 32)";
      "  --truncate-every K  truncate at every K-th token (default 5)";
      "  --budget S          seconds one check may take before it is a hang";
      "                      (default 5.0)";
      "  --arch NAME         target architecture (default x86-64)";
      "  --call-conv NAME    calling convention (default linux)";
      "  --list-all          print every case outside the criteria";
      "  --selftest          run the harness's own checks and exit";
      "  -h, --help          print this text";
      "";
      "exit status: 0 when the three criteria hold, 1 when one does not,";
      "2 when the command line or the corpus is unusable.";
    ]

type options = {
  corpora : string list;
  sample : int;
  sites : int;
  truncate_every : int;
  budget : float;
  arch_name : string;
  call_conv_name : string;
  list_all : bool;
  selftest : bool;
}

let default_options =
  {
    corpora = [];
    sample = 40;
    sites = 32;
    truncate_every = 5;
    budget = 5.0;
    arch_name = "x86-64";
    call_conv_name = "linux";
    list_all = false;
    selftest = false;
  }

type parsed = Run of options | Help | Invalid of string

let parse_arguments arguments =
  let count option value =
    match int_of_string_opt value with
    | Some number when number >= 0 -> Ok number
    | _ -> Error (Printf.sprintf "%s wants a count, got %S" option value)
  in
  let seconds option value =
    match float_of_string_opt value with
    | Some number when number > 0. -> Ok number
    | _ ->
        Error (Printf.sprintf "%s wants a positive number, got %S" option value)
  in
  let rec loop options = function
    | [] -> Run options
    | ("-h" | "--help") :: _ -> Help
    | "--corpus" :: value :: rest ->
        loop { options with corpora = options.corpora @ [ value ] } rest
    | "--corpus" :: [] -> Invalid "--corpus wants a directory"
    | "--sample" :: value :: rest -> (
        match count "--sample" value with
        | Ok sample -> loop { options with sample } rest
        | Error message -> Invalid message)
    | "--sample" :: [] -> Invalid "--sample wants a count"
    | "--sites" :: value :: rest -> (
        match count "--sites" value with
        | Ok sites -> loop { options with sites } rest
        | Error message -> Invalid message)
    | "--sites" :: [] -> Invalid "--sites wants a count"
    | "--truncate-every" :: value :: rest -> (
        match count "--truncate-every" value with
        | Ok truncate_every -> loop { options with truncate_every } rest
        | Error message -> Invalid message)
    | "--truncate-every" :: [] -> Invalid "--truncate-every wants a count"
    | "--budget" :: value :: rest -> (
        match seconds "--budget" value with
        | Ok budget -> loop { options with budget } rest
        | Error message -> Invalid message)
    | "--budget" :: [] -> Invalid "--budget wants a number of seconds"
    | "--arch" :: value :: rest -> loop { options with arch_name = value } rest
    | "--arch" :: [] -> Invalid "--arch wants a name"
    | "--call-conv" :: value :: rest ->
        loop { options with call_conv_name = value } rest
    | "--call-conv" :: [] -> Invalid "--call-conv wants a name"
    | "--list-all" :: rest -> loop { options with list_all = true } rest
    | "--selftest" :: rest -> loop { options with selftest = true } rest
    | argument :: rest when String.length argument > 0 && argument.[0] <> '-' ->
        loop { options with corpora = options.corpora @ [ argument ] } rest
    | argument :: _ -> Invalid (Printf.sprintf "unknown option: %s" argument)
  in
  loop default_options arguments

let read path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> Ok text
  | exception Sys_error message -> Error message

let is_jazz name = Filename.check_suffix name ".jazz"

(* Every .jazz file under one root, sorted by its path below the root, so
   the sample is the same on every host for the same corpus. *)
let rec walk relative directory found =
  match Sys.readdir directory with
  | exception Sys_error _ -> found
  | entries ->
      Array.fold_left
        (fun found entry ->
          if String.length entry > 0 && entry.[0] = '.' then found
          else
            let path = Filename.concat directory entry in
            let relative =
              if relative = "" then entry else Filename.concat relative entry
            in
            match Sys.is_directory path with
            | true -> walk relative path found
            | false ->
                if is_jazz entry then (path, relative) :: found else found
            | exception Sys_error _ -> found)
        found entries

let collect roots =
  List.concat_map
    (fun root ->
      let found = walk "" root [] in
      List.sort (fun (_, left) (_, right) -> String.compare left right) found
      |> List.map (fun (path, relative) -> (root, path, relative)))
    roots

type base = {
  path : string;
  display : string;
  text : string;
  tokens : Token.t array;
  outline : Outline.entry list;
}

type baseline = Kept of base | Skipped of string

let baseline ~arch ~call_conv ~budget path display =
  match read path with
  | Error message -> Skipped ("cannot be read: " ^ message)
  | Ok text -> (
      let tokens = Array.of_list (Token.tokenize text) in
      if Array.length tokens < 3 then Skipped "fewer than three tokens"
      else
        match
          Isolate.run ~budget (fun () ->
              Probe.run ~arch ~call_conv ~file:path text)
        with
        | Isolate.Returned { result; _ } ->
            if result.Probe.errors > 0 then
              Skipped
                (Printf.sprintf "the unmutated file has %d errors"
                   result.Probe.errors)
            else if List.length result.Probe.outline < 2 then
              (* A file with one item has nothing away from an edit inside
                 that item, so it cannot measure the outline criterion. *)
              Skipped "fewer than two items to compare"
            else Kept { path; display; text; tokens; outline = result.outline }
        | Isolate.Raised { message; _ } ->
            Skipped ("the unmutated file raises " ^ message)
        | Isolate.Timed_out { elapsed; _ } ->
            Skipped (Printf.sprintf "the unmutated file hangs (%.1f s)" elapsed)
        | Isolate.Crashed { detail; _ } ->
            Skipped ("the unmutated file crashes: " ^ detail))

let run options =
  let arch =
    match Jide_jasmin.Adapter.arch_of_string options.arch_name with
    | Some arch -> arch
    | None ->
        prerr_endline
          (Printf.sprintf "mutation: unknown architecture: %s (known: %s)"
             options.arch_name
             (String.concat ", " Jide_jasmin.Adapter.arch_names));
        exit 2
  in
  let call_conv =
    match Jide_jasmin.Adapter.call_conv_of_string options.call_conv_name with
    | Some call_conv -> call_conv
    | None ->
        prerr_endline
          (Printf.sprintf "mutation: unknown calling convention: %s"
             options.call_conv_name);
        exit 2
  in
  let found = collect options.corpora in
  if found = [] then (
    prerr_endline
      (Printf.sprintf "mutation: no .jazz file under %s"
         (String.concat ", " options.corpora));
    exit 2);
  let started = Unix.gettimeofday () in
  let kept = ref [] and skipped = ref [] in
  let seen = ref 0 in
  let total = List.length found in
  List.iter
    (fun (root, path, relative) ->
      incr seen;
      let display =
        if List.length options.corpora = 1 then relative
        else Filename.basename root ^ "/" ^ relative
      in
      (match baseline ~arch ~call_conv ~budget:options.budget path display with
      | Kept base -> kept := base :: !kept
      | Skipped reason -> skipped := (display, reason) :: !skipped);
      if !seen mod 50 = 0 then
        Printf.eprintf "  base check %d/%d: %d clean\r%!" !seen total
          (List.length !kept))
    found;
  Printf.eprintf "  base check %d/%d: %d clean\n%!" total total
    (List.length !kept);
  let kept = List.rev !kept in
  let skipped = List.rev !skipped in
  let sampled_list, kept_count = Mutant.sample ~maximum:options.sample kept in
  Printf.eprintf "  sampled %d of %d clean files\n%!" (List.length sampled_list)
    kept_count;
  let cases = ref [] in
  let sites = ref [] in
  List.iteri
    (fun index base ->
      List.iter
        (fun family ->
          let candidates =
            Mutant.sites ~every:options.truncate_every base.tokens family
          in
          let chosen, available =
            Mutant.sample ~maximum:options.sites candidates
          in
          sites := (family, List.length chosen, available) :: !sites;
          List.iter
            (fun site ->
              let mutation =
                Mutant.make ~source:base.text base.tokens family site
              in
              let outcome =
                Isolate.run ~budget:options.budget (fun () ->
                    Probe.run ~arch ~call_conv ~file:base.path
                      mutation.Mutant.text)
              in
              let comparison =
                match outcome with
                | Isolate.Returned { result; _ } ->
                    Outline.compare ~original:base.outline
                      ~mutant:result.Probe.outline
                      ~edit_start:mutation.Mutant.edit_start
                      ~edit_end:mutation.Mutant.edit_end
                      ~map_start:mutation.Mutant.map_start
                      ~map_end:mutation.Mutant.map_end
                | _ ->
                    {
                      Outline.away =
                        Outline.away_count base.outline
                          ~edit_start:mutation.Mutant.edit_start
                          ~edit_end:mutation.Mutant.edit_end;
                      identity_ok = false;
                      span_ok = false;
                      detail = "the check did not return an outline";
                    }
              in
              let enclosing =
                if Mutant.is_single_token family then
                  match
                    Outline.enclosing base.outline
                      ~start_byte:mutation.Mutant.edit_start
                      ~end_byte:mutation.Mutant.edit_end
                  with
                  | None -> None
                  | Some item ->
                      let present =
                        match outcome with
                        | Isolate.Returned { result; _ } ->
                            Outline.is_present result.Probe.outline item
                        | _ -> false
                      in
                      Some (item.Outline.kind ^ " " ^ item.Outline.name, present)
                else None
              in
              cases :=
                {
                  Report.path = base.display;
                  mutation;
                  outcome;
                  comparison;
                  enclosing;
                }
                :: !cases)
            chosen)
        Mutant.families;
      Printf.eprintf "  file %d/%d %s: %d cases\r%!" (index + 1)
        (List.length sampled_list) base.display (List.length !cases))
    sampled_list;
  Printf.eprintf "\n%!";
  let cases = List.rev !cases in
  let ran = Unix.gettimeofday () -. started in
  let site_totals =
    List.map
      (fun family ->
        let of_family =
          List.filter (fun (name, _, _) -> name = family) !sites
        in
        ( family,
          List.fold_left (fun total (_, used, _) -> total + used) 0 of_family,
          List.fold_left
            (fun total (_, _, available) -> total + available)
            0 of_family ))
      Mutant.families
  in
  Report.header options.corpora options.arch_name options.call_conv_name
    options.budget;
  Report.sample
    {
      Report.found = List.length found;
      clean = kept_count;
      sampled = List.length sampled_list;
      cases;
    };
  Report.sites site_totals;
  Report.distribution cases;
  Report.truncation cases;
  let quiet, unchanged, two = Report.criteria cases in
  Report.families cases;
  Report.causes cases;
  Report.failures ~limit:(if options.list_all then 0 else 25) cases;
  Report.files ~skipped (List.map (fun base -> base.display) sampled_list);
  Report.summary ~ran cases;
  if quiet && unchanged && two then 0 else 1

let main () =
  let arguments = Array.to_list Sys.argv |> List.tl in
  match parse_arguments arguments with
  | Invalid message ->
      prerr_endline ("mutation: " ^ message);
      prerr_endline usage;
      2
  | Help ->
      print_endline usage;
      0
  | Run options ->
      if options.selftest then if Selftest.run () then 0 else 1
      else if options.corpora = [] then (
        prerr_endline "mutation: --corpus is required";
        prerr_endline usage;
        2)
      else run options

let () = exit (main ())
