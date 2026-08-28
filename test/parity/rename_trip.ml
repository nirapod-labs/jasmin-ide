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

let is_word_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_word_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

(* Every identifier-shaped word of a text, comments and strings included:
   a fresh name must not occur anywhere, not only where the index sees it. *)
let words text =
  let length = String.length text in
  let found = ref [] in
  let index = ref 0 in
  while !index < length do
    if
      is_word_start text.[!index]
      && (!index = 0 || not (is_word_char text.[!index - 1]))
    then (
      let stop = ref !index in
      while !stop < length && is_word_char text.[!stop] do
        incr stop
      done;
      found := String.sub text !index (!stop - !index) :: !found;
      index := !stop)
    else incr index
  done;
  !found

(* [replace_word ~word ~by text] writes [by] for every whole-word occurrence
   of [word] in [text]. *)
let replace_word ~word ~by text =
  let length = String.length text in
  let word_length = String.length word in
  if word_length = 0 then text
  else
    let buffer = Buffer.create (length + 16) in
    let index = ref 0 in
    while !index < length do
      if
        !index + word_length <= length
        && String.sub text !index word_length = word
        && (!index = 0 || not (is_word_char text.[!index - 1]))
        && (!index + word_length = length
           || not (is_word_char text.[!index + word_length]))
      then (
        Buffer.add_string buffer by;
        index := !index + word_length)
      else (
        Buffer.add_char buffer text.[!index];
        incr index)
    done;
    Buffer.contents buffer

(* The columns of a location, [(12-17)] or [(4)], are left out of a verdict:
   a rename to a name of another length moves the columns on the lines it
   touches, and never a line. Everything else the compiler prints stays. *)
let strip_columns text =
  let length = String.length text in
  let buffer = Buffer.create length in
  let rec digits index =
    if index < length && text.[index] >= '0' && text.[index] <= '9' then
      digits (index + 1)
    else index
  in
  let rec loop index =
    if index >= length then ()
    else if text.[index] = '(' then
      let first = digits (index + 1) in
      let after =
        if first > index + 1 then
          if first < length && text.[first] = '-' then
            let second = digits (first + 1) in
            if second > first + 1 then second else first
          else first
        else index + 1
      in
      if first > index + 1 && after < length && text.[after] = ')' then
        loop (after + 1)
      else (
        Buffer.add_char buffer '(';
        loop (index + 1))
    else (
      Buffer.add_char buffer text.[index];
      loop (index + 1))
  in
  loop 0;
  Buffer.contents buffer

(* ------------------------------------------------------------------ *)
(* The oracle *)

type oracle = Binary of { jasminc : string; full : bool } | Library

type verdict = {
  status : string;  (** how the compiler ended: [exit 0], [exit 1], [timeout] *)
  output : string;  (** everything it printed, and the assembly after it *)
}

let arch_names =
  [
    ("x86-64", "x86-64");
    ("arm-m4", "arm-m4");
    ("armv8a", "armv8a");
    ("risc-v", "riscv");
    ("riscv", "riscv");
  ]

(* The architecture an entry is compiled for: the deepest directory of its path
   that names one, and the compiler's default (x86-64) otherwise, as the
   release's own test layout reads it. *)
let arch_of_path (path : string) =
  List.fold_left
    (fun found part ->
      match List.assoc_opt part arch_names with
      | Some arch -> arch
      | None -> found)
    "x86-64"
    (String.split_on_char '/' path)

type job = { entry : string; arch : string }

(* The compiler on one entry, in a directory of its own: through typing, which
   is where every name is resolved, or with [full] through every pass. *)
let start_binary ~jasminc ~full ~idirs ~work (job : job) =
  let out = Filename.concat work "out.s" in
  let log = Filename.concat work "log" in
  let args =
    [ jasminc; "-arch"; job.arch ]
    @ List.concat_map (fun (name, path) -> [ "-I"; name ^ "=" ^ path ]) idirs
    @ [ "-ptyping" ]
    @ (if full then [ "-o"; out ] else [ "-until_typing" ])
    @ [ job.entry ]
  in
  let fd =
    Unix.openfile log [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
  in
  let null = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let pid = Unix.create_process jasminc (Array.of_list args) null fd fd in
  Unix.close fd;
  Unix.close null;
  pid

(* The verdict is what the compiler prints: the typed program, then every
   error and warning, and how it ended. The typed program marks every global
   read as one, so a local that captured a global's use prints another
   program, and a name bound to another declaration is another error or
   warning. The assembly is not part of it: the register allocator's choices
   follow the variables' names (probed with jasminc 2026.09.0: renaming one
   local of [exec/poly1305.jazz] moves a load from one register to another),
   so the same program under another name is not the same assembly. For the
   same reason the passes after typing are run only with [--full]: what they
   add is the compiler's own choices, and a rename only changes names, which
   typing has resolved. *)
let finish_binary ~work status =
  let log = Option.value ~default:"" (read (Filename.concat work "log")) in
  (try Sys.remove (Filename.concat work "out.s") with Sys_error _ -> ());
  { status; output = log }

(* The compiler on every job, [jobs] at a time, each with a wall clock. *)
let run_binary ~jasminc ~full ~idirs ~jobs ~timeout ~scratch (work : job list) =
  let results = Hashtbl.create 16 in
  let pending =
    Queue.of_seq (List.to_seq (List.mapi (fun i job -> (i, job)) work))
  in
  let running = ref [] in
  let start () =
    let i, job = Queue.pop pending in
    let dir = Filename.concat scratch (Printf.sprintf "job-%d" i) in
    mkdir_p dir;
    let pid = start_binary ~jasminc ~full ~idirs ~work:dir job in
    running := (pid, i, job, dir, Unix.gettimeofday ()) :: !running
  in
  while not (Queue.is_empty pending && !running = []) do
    while (not (Queue.is_empty pending)) && List.length !running < jobs do
      start ()
    done;
    let still = ref [] in
    List.iter
      (fun ((pid, i, job, dir, started) as one) ->
        match Unix.waitpid [ Unix.WNOHANG ] pid with
        | 0, _ ->
            if Unix.gettimeofday () -. started > timeout then (
              (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
              ignore (Unix.waitpid [] pid);
              Hashtbl.replace results i
                (job.entry, { status = "timeout"; output = "" }))
            else still := one :: !still
        | _, status ->
            let status =
              match status with
              | Unix.WEXITED code -> Printf.sprintf "exit %d" code
              | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
              | Unix.WSTOPPED signal -> Printf.sprintf "stopped %d" signal
            in
            Hashtbl.replace results i (job.entry, finish_binary ~work:dir status))
      !running;
    running := !still;
    if !running <> [] then Unix.sleepf 0.005
  done;
  List.mapi (fun i _ -> Hashtbl.find results i) work

(* The linked library's check of one entry: the diagnostics with their file
   and line, then the warnings, one per line and sorted, so the order the
   library collects them in is not part of the verdict. *)
let run_library ~idirs (job : job) =
  match read job.entry with
  | None -> (job.entry, { status = "unreadable"; output = "" })
  | Some text ->
      let arch =
        Option.get
          (Jide_jasmin.Adapter.arch_of_string
             (if job.arch = "riscv" then "riscv" else job.arch))
      in
      let call_conv =
        Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux")
      in
      let result =
        Jide_jasmin.Adapter.check arch call_conv ~file:job.entry ~text ~idirs
      in
      let lines = Jide_lang.Line_index.create text in
      let line_of byte =
        (Jide_lang.Line_index.position_of_byte lines byte)
          .Jide_lang.Position.line
      in
      let errors =
        List.map
          (fun (diagnostic : Jide_lang.Diagnostic.t) ->
            Printf.sprintf "%s line %d: %s"
              (match diagnostic.Jide_lang.Diagnostic.severity with
              | Jide_lang.Diagnostic.Error -> "error"
              | Jide_lang.Diagnostic.Warning -> "warning"
              | Jide_lang.Diagnostic.Information -> "information"
              | Jide_lang.Diagnostic.Hint -> "hint")
              (line_of diagnostic.Jide_lang.Diagnostic.span.Span.start_byte)
              diagnostic.Jide_lang.Diagnostic.message)
          result.Jide_jasmin.Adapter.diagnostics
      in
      let required =
        List.concat_map
          (fun (required : Jide_jasmin.Adapter.required) ->
            List.map
              (fun (diagnostic : Jide_lang.Diagnostic.t) ->
                Printf.sprintf "in %s: %s" required.Jide_jasmin.Adapter.file
                  diagnostic.Jide_lang.Diagnostic.message)
              required.Jide_jasmin.Adapter.diagnostics)
          result.Jide_jasmin.Adapter.required
      in
      let warnings =
        List.map
          (fun (warning : Jide_jasmin.Warnings.t) ->
            Printf.sprintf "warning in %s: %s" warning.Jide_jasmin.Warnings.file
              warning.Jide_jasmin.Warnings.message)
          result.Jide_jasmin.Adapter.warnings
      in
      let status = if errors = [] && required = [] then "ok" else "errors" in
      ( job.entry,
        {
          status;
          output =
            String.concat "\n"
              (List.sort compare (errors @ required @ warnings));
        } )

let verdicts ~oracle ~idirs ~jobs ~timeout ~scratch work =
  match oracle with
  | Binary { jasminc; full } ->
      run_binary ~jasminc ~full ~idirs ~jobs ~timeout ~scratch work
  | Library -> List.map (run_library ~idirs) work

(* How two names are written when two verdicts are compared. A fresh name is
   written back as the old one, which is exact because it occurs nowhere
   else; a taken name and the old one are both written as one placeholder,
   because the compiler's own text mentions both. *)
type naming =
  | Fresh of { old_name : string; new_name : string }
  | Taken of { old_name : string; new_name : string }

(* A name is written as itself, and as the assembly writes a symbol on a
   system that prefixes one with [_]. *)
let replace_name ~word ~by text =
  replace_word ~word:("_" ^ word) ~by:("_" ^ by) (replace_word ~word ~by text)

(* The typed program is printed with line breaks where its lines grow too
   long, and a longer name moves them, so runs of white space are one space in
   a verdict. No token changes. *)
let collapse_blanks text =
  let buffer = Buffer.create (String.length text) in
  let blank = ref false in
  String.iter
    (fun c ->
      match c with
      | ' ' | '\t' | '\n' | '\r' -> blank := true
      | c ->
          if !blank && Buffer.length buffer > 0 then Buffer.add_char buffer ' ';
          blank := false;
          Buffer.add_char buffer c)
    text;
  Buffer.contents buffer

let normalise_before naming text =
  match naming with
  | Fresh _ -> strip_columns text
  | Taken { old_name; new_name } ->
      strip_columns
        (replace_name ~word:new_name ~by:"\001"
           (replace_name ~word:old_name ~by:"\001" text))

(* A fresh name is a part of no word of the corpus, so every place it occurs
   in the compiler's text, inside a longer symbol too ([LDouble__map_rn0$1]),
   is a place the rename wrote it. *)
let replace_all ~part ~by text =
  let length = String.length text and lp = String.length part in
  let buffer = Buffer.create length in
  let rec loop index =
    if index >= length then ()
    else if index + lp <= length && String.sub text index lp = part then (
      Buffer.add_string buffer by;
      loop (index + lp))
    else (
      Buffer.add_char buffer text.[index];
      loop (index + 1))
  in
  if lp > 0 then loop 0 else Buffer.add_string buffer text;
  Buffer.contents buffer

let normalise_after naming text =
  match naming with
  | Fresh { old_name; new_name } ->
      strip_columns (replace_all ~part:new_name ~by:old_name text)
  | Taken _ -> normalise_before naming text

(* ------------------------------------------------------------------ *)
(* The workspace: the copy, the engine over it, the entries' resolutions *)

type workspace = {
  root : string;  (** the copy, which a printed path is relative to *)
  files : string list;  (** absolute paths of every source in the copy *)
  originals : (string, string) Hashtbl.t;  (** the bytes each file began with *)
  entries : string list;
  idirs : (string * string) list;  (** bound to paths in the copy *)
  engine : Engine.t;
  readers : (string, string list) Hashtbl.t;
      (** for each file, the entries whose resolution reads it *)
  multi : (string, bool) Hashtbl.t;
      (** whether some entry reads the file together with another file *)
  taken_words : (string, unit) Hashtbl.t;  (** every word of the corpus *)
  unique_words : string list;  (** the same words, once each *)
  before : (string, verdict) Hashtbl.t;  (** each entry's verdict, as copied *)
  scratch : string;
}

let uri_of path = Jide_lang.Uri.to_string (Jide_lang.Uri.of_path path)

let open_workspace ~corpus ~idirs =
  let scratch = scratch_directory () in
  let root = Filename.concat scratch "corpus" in
  let relative = sources corpus in
  let originals = Hashtbl.create 64 in
  let files =
    List.map
      (fun rel ->
        let source = Filename.concat corpus rel in
        let target = Filename.concat root rel in
        mkdir_p (Filename.dirname target);
        let text = Option.value ~default:"" (read source) in
        write target text;
        let path = Index.normalise target in
        Hashtbl.replace originals path text;
        path)
      relative
  in
  let idirs =
    List.map
      (fun (name, path) ->
        ( name,
          Index.normalise
            (if Filename.is_relative path then Filename.concat root path
             else path) ))
      idirs
  in
  let entries =
    List.filter (fun path -> Filename.check_suffix path ".jazz") files
  in
  let engine = Engine.create () in
  Engine.set_settings engine { Engine.default_settings with Engine.idirs };
  Engine.set_root engine (Some root);
  let readers = Hashtbl.create 64 in
  let multi = Hashtbl.create 64 in
  let cache = Resolve.cache () in
  List.iter
    (fun entry ->
      match Resolve.of_file ~cache ~idirs ~read entry with
      | None -> ()
      | Some resolution ->
          let read_files =
            List.map
              (fun (index : Index.t) -> Index.normalise index.Index.file)
              (Resolve.files resolution)
          in
          List.iter
            (fun path ->
              let earlier =
                Option.value ~default:[] (Hashtbl.find_opt readers path)
              in
              Hashtbl.replace readers path (earlier @ [ entry ]);
              if List.length read_files > 1 then Hashtbl.replace multi path true)
            read_files)
    entries;
  let taken_words = Hashtbl.create 4096 in
  Hashtbl.iter
    (fun _ text ->
      List.iter (fun word -> Hashtbl.replace taken_words word ()) (words text))
    originals;
  {
    root = Index.normalise root;
    files;
    originals;
    entries;
    idirs;
    engine;
    readers;
    multi;
    taken_words;
    unique_words = Hashtbl.fold (fun word () acc -> word :: acc) taken_words [];
    before = Hashtbl.create 64;
    scratch;
  }

let close_workspace workspace = remove_tree workspace.scratch

(* A path of the copy as the corpus names it, and every such path inside a
   message too, so a printed line names the corpus's own file. *)
let shown workspace text =
  let prefix = workspace.root ^ "/" in
  let lp = String.length prefix in
  let buffer = Buffer.create (String.length text) in
  let rec loop index =
    if index >= String.length text then ()
    else if
      index + lp <= String.length text && String.sub text index lp = prefix
    then loop (index + lp)
    else (
      Buffer.add_char buffer text.[index];
      loop (index + 1))
  in
  loop 0;
  Buffer.contents buffer

(* ------------------------------------------------------------------ *)
(* One rename through the server's own answer *)

let ask_rename workspace ~path ~byte ~new_name =
  let uri = uri_of path in
  let text = Option.value ~default:"" (read path) in
  Engine.open_document workspace.engine ~uri ~version:1 ~text;
  ignore (Engine.check workspace.engine ~uri);
  let answer =
    match Rename_features.prepare workspace.engine ~uri ~byte with
    | None -> `Refused "prepareRename gave no answer"
    | Some (Error reason) -> `Refused reason
    | Some (Ok span) -> (
        match
          Rename_features.rename workspace.engine ~uri
            ~byte:span.Span.start_byte ~new_name
        with
        | None -> `Refused "rename gave no answer"
        | Some (Error reason) -> `Refused reason
        | Some (Ok edits) ->
            `Edits
              ( span.Span.start_byte,
                List.map
                  (fun (edit : Features.edit) ->
                    let file = edit.Features.file.Features.uri in
                    ( Index.normalise
                        (Option.value ~default:file
                           (Jide_lang.Uri.path (Jide_lang.Uri.of_string file))),
                      edit.Features.edits ))
                  edits ))
  in
  Engine.close_document workspace.engine ~uri;
  answer

let apply_text text (edits : Jide_engine.Types.text_edit list) =
  let sorted =
    List.sort
      (fun (a : Jide_engine.Types.text_edit) (b : Jide_engine.Types.text_edit) ->
        compare b.Jide_engine.Types.span.Span.start_byte
          a.Jide_engine.Types.span.Span.start_byte)
      edits
  in
  List.fold_left
    (fun text (edit : Jide_engine.Types.text_edit) ->
      let start = edit.Jide_engine.Types.span.Span.start_byte in
      let stop = edit.Jide_engine.Types.span.Span.end_byte in
      String.sub text 0 start ^ edit.Jide_engine.Types.new_text
      ^ String.sub text stop (String.length text - stop))
    text sorted

(* Where [byte] of a file lands once the file's edits are made. *)
let moved (edits : Jide_engine.Types.text_edit list) byte =
  List.fold_left
    (fun after (edit : Jide_engine.Types.text_edit) ->
      let span = edit.Jide_engine.Types.span in
      if span.Span.end_byte <= byte then
        after
        + String.length edit.Jide_engine.Types.new_text
        - (span.Span.end_byte - span.Span.start_byte)
      else after)
    byte edits

(* ------------------------------------------------------------------ *)
(* Sampling *)

type symbol = {
  path : string;
  byte : int;
  name : string;
  kind : Index.kind;
  multi_file : bool;
  others : string list;  (** the names other declarations of the file have *)
}

let kind_name = function
  | Index.Function -> "function"
  | Index.Parameter -> "parameter"
  | Index.Variable -> "variable"
  | Index.Global -> "global"
  | Index.Namespace -> "namespace"
  | Index.Type_alias -> "type alias"
  | _ -> "other"

type population = { symbols : symbol list; namespaces : int; unread : int }

let population workspace =
  let namespaces = ref 0 in
  let unread = ref 0 in
  let symbols =
    List.concat_map
      (fun path ->
        let text = Hashtbl.find workspace.originals path in
        let index, _ = Index.of_text ~file:path text in
        let names =
          List.sort_uniq compare
            (List.map
               (fun (d : Index.declaration) -> d.Index.name)
               index.Index.declarations)
        in
        List.filter_map
          (fun (declaration : Index.declaration) ->
            if declaration.Index.kind = Index.Namespace then (
              incr namespaces;
              None)
            else if not (Hashtbl.mem workspace.readers path) then (
              incr unread;
              None)
            else
              let byte =
                match
                  Index.find_occurrence index
                    ~byte:declaration.Index.name_span.Span.start_byte
                with
                | Some occurrence -> occurrence.Index.last.Span.start_byte
                | None -> declaration.Index.name_span.Span.start_byte
              in
              Some
                {
                  path;
                  byte;
                  name = declaration.Index.name;
                  kind = declaration.Index.kind;
                  multi_file = Hashtbl.mem workspace.multi path;
                  others =
                    List.filter
                      (fun name ->
                        name <> declaration.Index.name
                        && not (String.contains name ':'))
                      names;
                })
          index.Index.declarations)
      workspace.files
  in
  { symbols; namespaces = !namespaces; unread = !unread }

(* [n] of [items], drawn without replacement by the seeded generator, in the
   order they were drawn. *)
let draw state n items =
  let array = Array.of_list items in
  let length = Array.length array in
  let n = min n length in
  for i = 0 to n - 1 do
    let j = i + Random.State.int state (length - i) in
    let swap = array.(i) in
    array.(i) <- array.(j);
    array.(j) <- swap
  done;
  Array.to_list (Array.sub array 0 n)

let contains ~part word =
  let lw = String.length word and lp = String.length part in
  let rec at i = i + lp <= lw && (String.sub word i lp = part || at (i + 1)) in
  at 0

