(** The rename round trip, measured against the compiler.

    The requirement is two sentences:

    {v
      For sampled symbols of each corpus project, renaming and renaming back
      restores the text exactly; the renamed program gets the same verdict.
    v}

    This program measures it and fails when it does not hold. It copies the
    corpus into a scratch directory, opens that copy as a workspace in the
    engine the server runs, and for each sampled declaration asks the server's
    own rename ({!Jide_controller.Rename_features}): a prepareRename at the
    declaration's name, then a rename. When the rename is made, the edits are
    written into the copy, and the compiler is asked for the verdict of every
    entry that reads an edited file. The verdict is compared with the verdict
    the same entry had before. The renamed text is then opened in the engine,
    renamed back to the old name the same way, and every file either rename
    touched is compared with the original bytes. The original bytes go back
    into the copy before the next symbol.

    Each declaration is renamed twice:

    - to a fresh name, one that is a part of no word of the corpus. The
      verdicts are compared after the fresh name is written back as the old
      one in the compiler's output, so the comparison is of everything the
      compiler prints: the exit status, every error and warning with its file
      and line, and the typed program, which marks each global read as one;
    - to a taken name, one another declaration of the same file already has.
      Most of these are refused, and a refusal is counted by its reason. One
      that is made is compared the same way, with both names written as one
      placeholder on both sides.

    The verdict is the oracle's, never the engine's. With [--jasminc] it is the
    native compiler, run once per entry and rename: [jasminc -ptyping
    -until_typing] with the entry's architecture and the include paths given,
    or with [--full] [jasminc -ptyping -o out.s], every pass, whose assembly
    is not compared (see {!finish_binary} for why). Columns and runs of white
    space are left out, because a name of another length moves them. Without
    [--jasminc] (the self-test) the verdict is the linked library's own check:
    its errors and warnings, with their lines.

    A declaration is sampled from one of two populations, apart: the
    declarations of a file some entry reads together with another file (the
    multi-file projects), and the declarations of an entry that reads nothing
    else. [--sample N] draws N from each, or all of a smaller one, with the
    seed given; the run prints both sizes. A namespace's own name is not
    sampled, because a rename refuses it by design and its tests say so; a
    declaration in a file no entry reads is not sampled, because no verdict
    covers it. Both are printed as exclusions with their counts.

    [--plant skip] is the negative control: every made rename has one of its
    edits dropped before it is written, an occurrence other than the
    declaration's own, which is the defect a rename that missed a use would
    have. The run must see each such rename as a difference, by the verdict or
    by the round trip, and so exits 1; the counts say how many were planted
    and how many were caught.

    Usage:

    {v
      rename_trip --corpus <dir> [--jasminc <path>] [--idir NAME=PATH]...
                  [--seed N] [--sample N] [--jobs N] [--timeout S]
                  [--plant skip] [--full]
      rename_trip --selftest
      rename_trip --help
    v}

    <dir> is a tree of [.jazz] and [.jinc] files: a Jasmin release's
    [compiler/tests] (or the release root that holds it), or a library such as
    libjade. Every [.jazz] file under it is an entry, which is the rule the
    server applies to a workspace with no project file. An include path's
    [PATH] is read relative to <dir>, and the copy is bound in its place.

    The exit status is 0 when every made rename restores the text and keeps
    the verdict, 1 when one does not, and 2 when the command line, the corpus
    or the oracle is wrong. *)

module Engine = Jide_engine.Engine
module Index = Jide_engine.Index
module Resolve = Jide_engine.Resolve
module Rename = Jide_engine.Rename
module Features = Jide_controller.Features
module Rename_features = Jide_controller.Rename_features
module Span = Jide_lang.Span

let usage =
  String.concat "\n"
    [
      "usage: rename_trip --corpus <dir> [--jasminc <path>] [--idir \
       NAME=PATH]...";
      "                   [--seed N] [--sample N] [--jobs N] [--timeout S]";
      "                   [--plant skip] [--full]";
      "       rename_trip --selftest";
      "       rename_trip --help";
      "";
      "  --corpus <dir>     a tree of .jazz and .jinc files; every .jazz is an \
       entry";
      "  --jasminc <path>   the compiler whose verdict is compared (default: \
       the";
      "                     linked library's check)";
      "  --idir NAME=PATH   bind NAME for `from NAME require`, PATH under <dir>";
      "  --seed N           the sampling seed (default 1)";
      "  --sample N         declarations drawn from each population (default \
       120)";
      "  --jobs N           compilers run at once (default 4)";
      "  --timeout S        seconds one compile may take (default 300)";
      "  --plant skip       drop one edit of every rename: the negative control";
      "  --full             run the compiler through every pass, not to typing";
      "  --selftest         run the program against small projects";
      "";
      "See the comment at the top of test/parity/rename_trip.ml for what it";
      "measures and what each count means.";
    ]

(* ------------------------------------------------------------------ *)
(* Files *)

let read (path : string) : string option =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> Some text
  | exception _ -> None

let write (path : string) (text : string) =
  Out_channel.with_open_bin path (fun channel -> output_string channel text)

let is_directory path =
  match Sys.is_directory path with ok -> ok | exception Sys_error _ -> false

let rec mkdir_p path =
  if not (Sys.file_exists path) then (
    mkdir_p (Filename.dirname path);
    Sys.mkdir path 0o755)

let rec remove_tree path =
  if is_directory path then (
    Array.iter
      (fun name -> remove_tree (Filename.concat path name))
      (Sys.readdir path);
    Sys.rmdir path)
  else if Sys.file_exists path then Sys.remove path

let is_source name =
  Filename.check_suffix name ".jazz" || Filename.check_suffix name ".jinc"

(* Every source under a directory, as paths relative to it, sorted, so two
   runs read the files in one order. Hidden directories are not read. *)
let sources root =
  let rec walk relative acc =
    let directory =
      if relative = "" then root else Filename.concat root relative
    in
    match Sys.readdir directory with
    | exception Sys_error _ -> acc
    | names ->
        Array.fold_left
          (fun acc name ->
            if name = "" || name.[0] = '.' then acc
            else
              let child =
                if relative = "" then name else Filename.concat relative name
              in
              if is_directory (Filename.concat root child) then walk child acc
              else if is_source name then child :: acc
              else acc)
          acc names
  in
  List.sort String.compare (walk "" [])

let scratch_directory () =
  let base = Filename.get_temp_dir_name () in
  let rec attempt n =
    let path =
      Filename.concat base
        (Printf.sprintf "rename-trip-%d-%d" (Unix.getpid ()) n)
    in
    if Sys.file_exists path then attempt (n + 1)
    else (
      mkdir_p path;
      path)
  in
  attempt 0

(* ------------------------------------------------------------------ *)
(* Words *)

