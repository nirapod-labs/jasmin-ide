exception Usage

let usage =
  "usage: mutation <corpus> [--seed N] [--files N]\n\n\
   Delete a token, insert a token, duplicate a token, swap ; and }, or cut the\n\
   text at a token boundary, one alteration at a time, and check each altered\n\
   text with the checker. <corpus> is a Jasmin corpus, normally the\n\
   compiler/tests directory of the release the server is built against.\n\n\
   options:\n\
  \  --seed N    the seed of the generator (default: 1)\n\
  \  --files N   the first N files of the corpus, sorted (default: all)\n\
  \  -h, --help  print this text\n\n\
   exit status: 0 when the three properties hold, 1 when one does not, and\n\
   2 when the command line or the corpus cannot be used.\n"

let () =
  let corpus = ref None in
  let seed = ref 1 in
  let limit = ref max_int in
  let number value =
    match int_of_string_opt value with
    | Some number -> number
    | None -> raise Usage
  in
  let rec parse = function
    | [] -> ()
    | ("-h" | "--help") :: _ -> raise Usage
    | "--seed" :: value :: rest ->
        seed := number value;
        parse rest
    | "--files" :: value :: rest ->
        limit := number value;
        parse rest
    | argument :: _ when String.length argument > 0 && argument.[0] = '-' ->
        raise Usage
    | argument :: rest -> (
        match !corpus with
        | None ->
            corpus := Some argument;
            parse rest
        | Some _ -> raise Usage)
  in
  match
    match
      parse (List.tl (Array.to_list Sys.argv));
      !corpus
    with
    | corpus -> `Run (corpus, !seed, !limit)
    | exception Usage -> `Usage
  with
  | `Usage ->
      print_string usage;
      exit 2
  | `Run (None, _, _) ->
      print_string usage;
      exit 2
  | `Run (Some corpus, seed, limit) ->
      if not (Sys.file_exists corpus) then (
        Printf.printf "mutation: no corpus at %s\n" corpus;
        exit 2);
      if not (Sys.is_directory corpus) then (
        Printf.printf "mutation: %s is not a directory\n" corpus;
        exit 2);
      let report = Jide_mutation.run ~limit ~seed ~corpus () in
      Jide_mutation.print_report report;
      exit (if Jide_mutation.passed report then 0 else 1)
