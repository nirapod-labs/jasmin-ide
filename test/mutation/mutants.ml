(** One single-token mutant per corpus file per family, written to disk.

    The first-error parity run puts a mutated file in front of the checker and
    in front of a real compiler and compares the two first diagnostics, so it
    needs the mutants as files, not as the in-memory texts {!Mutant} builds
    for the recovery harness. {!Mutant} defines what each single-token family
    does and {!Token} splits a text the way the compiler's lexer does; this
    program only chooses one site per file per family, writes the mutated
    text, and records the set in a manifest:

    - {!Mutant.Delete}: one token is removed;
    - {!Mutant.Duplicate}: one token is copied in place;
    - {!Mutant.Swap}: a [;] becomes [}] or a [}] becomes [;].

    {!Mutant.Truncate} is not a single-token mutation and is not emitted.

    The site is a function of the seed, the file's path below the corpus and
    the family's name, so the same seed over the same corpus emits the same
    set, in the same order, on every run and every host. Nothing about this
    host is read or written: the corpus and the output directory are
    arguments, and the program writes nothing outside the output directory. *)

let usage =
  String.concat "\n"
    [
      "usage: mutants --output DIR [options] <corpus directory>";
      "";
      "Write one single-token mutant per corpus file per family, with a";
      "manifest that names each one.";
      "";
      "options:";
      "  --output DIR   where to write the mutants and their manifest.tsv";
      "                 (required; the directory is made if it is absent)";
      "  --seed TEXT    the text that fixes which site each mutant touches";
      "                 (default 0)";
      "  --files N      mutate at most N files, spread evenly over the sorted";
      "                 file list (default 0: every file)";
      "  -h, --help     print this text";
      "";
      "The manifest holds one line per mutant, tab separated:";
      "";
      "  <index>  <family>  <path below the corpus>  <label>";
      "";
      "and the mutant itself is <output>/<index>.jazz.";
      "";
      "exit status: 0 on success, 2 when the command line or the corpus is";
      "unusable.";
    ]

type options = { output : string; seed : string; files : int; corpus : string }

let default_options = { output = ""; seed = "0"; files = 0; corpus = "" }

let parse_arguments arguments =
  let rec loop options = function
    | [] -> Ok options
    | ("-h" | "--help") :: _ -> Error `Help
    | "--output" :: value :: rest -> loop { options with output = value } rest
    | "--output" :: [] -> Error (`Message "--output wants a directory")
    | "--seed" :: value :: rest -> loop { options with seed = value } rest
    | "--seed" :: [] -> Error (`Message "--seed wants a text")
    | "--files" :: value :: rest -> (
        match int_of_string_opt value with
        | Some files when files >= 0 -> loop { options with files } rest
        | _ ->
            Error
              (`Message (Printf.sprintf "--files wants a count, got %S" value)))
    | "--files" :: [] -> Error (`Message "--files wants a count")
    | argument :: rest when String.length argument > 0 && argument.[0] <> '-' ->
        if options.corpus = "" then loop { options with corpus = argument } rest
        else Error (`Message ("one corpus directory, not two: " ^ argument))
    | argument :: _ -> Error (`Message ("unknown option: " ^ argument))
  in
  loop default_options arguments

let is_jazz name = Filename.check_suffix name ".jazz"

(* Every .jazz file under the corpus, as (path, path below the corpus), sorted
   by the path below the corpus so that two hosts emit the same order. *)
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

let collect corpus =
  walk "" corpus []
  |> List.sort (fun (_, left) (_, right) -> String.compare left right)

let read path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> Ok text
  | exception Sys_error message -> Error message

(* FNV-1a over the seed, the path and the family. A hash rather than the
   standard library's generator, so a site cannot move when a generator or a
   library version changes: the set has to be replayable from the seed alone. *)
let hash text =
  let offset = Int64.of_string "0xcbf29ce484222325" in
  let prime = Int64.of_string "0x100000001b3" in
  let result = ref offset in
  String.iter
    (fun character ->
      result := Int64.logxor !result (Int64.of_int (Char.code character));
      result := Int64.mul !result prime)
    text;
  (* 62 bits, the positive range of an [int] on the 64-bit hosts this runs
     on; a wider mask would let the conversion turn negative and the site
     would index behind the array. *)
  Int64.(to_int (logand !result 0x3fff_ffff_ffff_ffffL))

let site ~seed ~relative family sites =
  let candidates = Array.of_list sites in
  let count = Array.length candidates in
  if count = 0 then None
  else
    let key = seed ^ "\000" ^ relative ^ "\000" ^ Mutant.family_name family in
    Some candidates.(hash key mod count)

(* [spread maximum files] keeps at most [maximum] of [files], spread evenly
   over the list, so a bounded run measures the whole corpus and not its
   first files. *)
let spread maximum files =
  let total = List.length files in
  if maximum <= 0 || total <= maximum then files
  else
    let array = Array.of_list files in
    List.init maximum (fun index -> array.(index * total / maximum))

let write path text =
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel text)

let emitted_families = [ Mutant.Delete; Mutant.Duplicate; Mutant.Swap ]

let run options =
  let output = options.output in
  (try if not (Sys.file_exists output) then Sys.mkdir output 0o755
   with Sys_error message ->
     prerr_endline ("mutants: cannot make " ^ output ^ ": " ^ message);
     exit 2);
  if not (Sys.is_directory output) then (
    prerr_endline ("mutants: not a directory: " ^ output);
    exit 2);
  if
    (not (Sys.file_exists options.corpus))
    || not (Sys.is_directory options.corpus)
  then (
    prerr_endline ("mutants: no corpus directory: " ^ options.corpus);
    exit 2);
  let found = collect options.corpus in
  if found = [] then (
    prerr_endline ("mutants: no .jazz file under " ^ options.corpus);
    exit 2);
  let files = spread options.files found in
  let manifest = Filename.concat output "manifest.tsv" in
  let channel =
    try Out_channel.open_bin manifest
    with Sys_error message ->
      prerr_endline ("mutants: cannot write " ^ manifest ^ ": " ^ message);
      exit 2
  in
  let index = ref 0 in
  let per_family = Hashtbl.create 8 in
  let unreadable = ref 0 in
  List.iter
    (fun (path, relative) ->
      match read path with
      | Error _ -> incr unreadable
      | Ok text ->
          let tokens = Array.of_list (Token.tokenize text) in
          if Array.length tokens >= 1 then
            List.iter
              (fun family ->
                match
                  site ~seed:options.seed ~relative family
                    (Mutant.sites ~every:1 tokens family)
                with
                | None -> ()
                | Some chosen ->
                    let mutation =
                      Mutant.make ~source:text tokens family chosen
                    in
                    let number = !index in
                    incr index;
                    let mutant_path =
                      Filename.concat output (Printf.sprintf "%d.jazz" number)
                    in
                    write mutant_path mutation.Mutant.text;
                    Out_channel.output_string channel
                      (Printf.sprintf "%d\t%s\t%s\t%s\n" number
                         (Mutant.family_name family)
                         relative mutation.Mutant.label);
                    let seen =
                      match Hashtbl.find_opt per_family family with
                      | Some count -> count
                      | None -> 0
                    in
                    Hashtbl.replace per_family family (seen + 1))
              emitted_families)
    files;
  Out_channel.close channel;
  Printf.printf "mutants: %d of %d files, %d mutants" (List.length files)
    (List.length found) !index;
  List.iter
    (fun family ->
      let count =
        match Hashtbl.find_opt per_family family with
        | Some count -> count
        | None -> 0
      in
      Printf.printf ", %s %d" (Mutant.family_name family) count)
    emitted_families;
  Printf.printf "\n%!";
  if !unreadable > 0 then
    Printf.printf "mutants: %d files could not be read\n%!" !unreadable;
  if !index = 0 then (
    prerr_endline "mutants: no mutant was emitted";
    exit 2);
  0

