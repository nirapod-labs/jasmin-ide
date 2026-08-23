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

