(* The arity and signature of every intrinsic call.

   The requirement is that signature help for every intrinsic call in a corpus
   has the arity of the compiler's instruction description. The check goes
   further: for every intrinsic of every architecture the linked Jasmin accepts,
   the signature the server answers equals the compiler's own table.

   The expected side is the native compiler, [--jasminc]. Its messages give the
   number of arguments and of results of each instruction (see oracle.ml), so
   the arity the server answers is compared with the compiler's own count and
   not with a copy of the library's table. The library is a second opinion,
   used for the parameter types and the whole label, which the compiler's
   messages do not carry; it is checked against the compiler's counts too.

   Two parts:

   - The table part takes every spelling the table accepts: each name with
     each of its suffixes, and the bare name where the table takes it, plus
     four names the compiler types by a rule of its own and two names the table
     does not hold. Each spelling is written in a document under a
     [jasmin.project] that names the architecture; the engine analyses the
     document, and [Complete.signature_help] is asked at the call with the
     snapshot's own architecture, which is the value the server passes.

   - The corpus part takes every intrinsic call in the [.jazz] and [.jinc]
     files under the directory named on the command line. Each call is asked
     under the architecture its path names, which is the corpus's own rule
     (see [archs_of_path]). A corpus has no project file, so this part calls
     [Complete.signature_help] directly.

   A comparison agrees when the server's answer has the compiler's number of
   arguments and results, or when both refuse. The table part also compares the
   parameters and the whole label with the library's description, and the
   library's counts with the compiler's. Anything else is a difference, and a
   difference fails the run.

   usage: arity [--jasminc <path>] [--control ignore-arch | --control drop-parameter]
                [<corpus-root>]

   Without [--jasminc] the table part compares the server with the library's
   description only, and a corpus cannot be run: its expected side is the
   compiler's. [--control] answers wrongly on purpose, so the comparison is seen
   to fail. [ignore-arch] answers every call as x86-64, which is what a server
   answers when it does not pass the project's architecture; [drop-parameter]
   answers without the last parameter of each call that has one. It applies to
   the table part and to the corpus part.

   Exit status: 0 when every comparison agrees, 1 when one differs, 2 when the
   run cannot be made. *)

open Jide_engine
module Intrinsics = Jide_jasmin.Intrinsics
module Arch = Jide_jasmin.Arch

let usage =
  "usage: arity [--jasminc <path>] [--control ignore-arch | --control \
   drop-parameter] [<corpus-root>]"

(* A run that cannot be made. It is raised, not exited, so the temporary tree
   is removed on the way out, and [main] prints it. *)
exception Run_failed of string

let die fmt = Printf.ksprintf (fun message -> raise (Run_failed message)) fmt

(* ------------------------------------------------------------------ *)
(* Controls *)

type control = No_control | Ignore_arch | Drop_parameter

let control_of_string = function
  | "ignore-arch" -> Ignore_arch
  | "drop-parameter" -> Drop_parameter
  | other -> die "unknown control %s (ignore-arch, drop-parameter)" other

let control_name = function
  | No_control -> "none"
  | Ignore_arch -> "ignore-arch"
  | Drop_parameter -> "drop-parameter"

(* ------------------------------------------------------------------ *)
(* The library's description, rendered the way the server labels it *)

let render_ty = function
  | Intrinsics.Bool -> "bool"
  | Intrinsics.Int -> "int"
  | Intrinsics.Word { signed; bits } ->
      (if signed then "s" else "u") ^ string_of_int bits
  | Intrinsics.Array { bits; length } -> Printf.sprintf "u%d[%s]" bits length

(* The parameters and the whole label one description gives a call written as
   [written]. This is a second rendering, written here, so the label check
   compares two renderings of one description and not one with itself. *)
let expected_of ~written (description : Intrinsics.t_signature) =
  let parameters = List.map render_ty description.Intrinsics.arguments in
  let results = List.map render_ty description.Intrinsics.results in
  let label =
    Printf.sprintf "%s(%s)%s" written
      (String.concat ", " parameters)
      (match results with [] -> "" | _ -> " -> " ^ String.concat ", " results)
  in
  (parameters, label)

(* The number of results in a server label: the items after the arrow, or none.
   The label is written by [Complete.label], and no type it prints has a comma. *)
let results_of_label label =
  let arrow = " -> " in
  let width = String.length arrow in
  let rec find index =
    if index + width > String.length label then None
    else if String.equal (String.sub label index width) arrow then Some index
    else find (index + 1)
  in
  match find 0 with
  | None -> 0
  | Some index ->
      let start = index + width in
      let after = String.sub label start (String.length label - start) in
      List.length (String.split_on_char ',' after)

(* ------------------------------------------------------------------ *)
(* Spellings of the table *)

(* The four names [Pretyping.tt_prim] types by a rule before it reaches the
   table. The compiler accepts them without an instruction description, so they
   have no count, and the server must refuse them. *)
let pseudo_names = [ "spill"; "unspill"; "randombytes"; "declassify" ]

(* Names the table does not hold, under any architecture. *)
let unknown_names = [ "NOSUCH_64"; "ADD_999" ]

(* One spelling the table accepts, with the library's own description of it. *)
type spelling = {
  text : string;  (** the name after the [\#], suffix included *)
  library : Intrinsics.t_signature option;
}

let spellings_of ~arch =
  let named =
    List.concat_map
      (fun (entry : Intrinsics.t) ->
        (* The bare name is a spelling of its own whenever the table takes
           it, which for an x86 name with suffixes is the default size. *)
        let suffixes =
          List.sort_uniq String.compare ("" :: entry.Intrinsics.suffixes)
        in
        List.map
          (fun suffix ->
            let text =
              match
                Intrinsics.written_of ~arch ~name:entry.Intrinsics.name ~suffix
              with
              | Some text -> text
              | None ->
                  die
                    "the table lists %s under %s but writes no spelling for it"
                    entry.Intrinsics.name arch
            in
            {
              text;
              library =
                Intrinsics.signature_of ~arch ~name:entry.Intrinsics.name
                  ~suffix;
            })
          suffixes)
      (Intrinsics.all ~arch)
  in
  let unlisted text = { text; library = None } in
  named @ List.map unlisted pseudo_names @ List.map unlisted unknown_names

(* ------------------------------------------------------------------ *)
(* Comparisons *)

(* The server's answer for one call, after a control has been applied. *)
let shown_under ~control (shown : Complete.signature option) =
  match (control, shown) with
  | Drop_parameter, Some signature ->
      let rec drop_last = function
        | [] -> []
        | [ _ ] -> []
        | item :: rest -> item :: drop_last rest
      in
      Some
        {
          signature with
          Complete.parameters = drop_last signature.Complete.parameters;
        }
  | _ -> shown

(* The server against the compiler's answer: the number of arguments and of
   results, or that both refuse. *)
let judge_against (answer : Oracle.answer) (shown : Complete.signature option) =
  match (answer, shown) with
  | Oracle.Described { args; results }, None ->
      [
        Printf.sprintf
          "refuses it, but the compiler describes %d arguments and %d results"
          args results;
      ]
  | Oracle.Described { args; results }, Some signature ->
      let shown_args = List.length signature.Complete.parameters in
      let shown_results = results_of_label signature.Complete.label in
      if shown_args = args && shown_results = results then []
      else
        [
          Printf.sprintf
            "answers %d arguments and %d results, the compiler %d and %d"
            shown_args shown_results args results;
        ]
  | (Oracle.Refused _ | Oracle.Rule), None -> []
  | (Oracle.Refused _ | Oracle.Rule), Some signature ->
      [
        Printf.sprintf "answers %d arguments where the compiler refuses it"
          (List.length signature.Complete.parameters);
      ]

(* The server against the library's own description: callee, parameters and
   label. *)
let judge_library ~written (library : Intrinsics.t_signature option)
    (shown : Complete.signature option) =
  match (library, shown) with
  | None, None -> []
  | None, Some signature ->
      [
        Printf.sprintf
          "answers %d parameters where the table holds no such spelling"
          (List.length signature.Complete.parameters);
      ]
  | Some description, None ->
      [
        Printf.sprintf "refuses it where the table describes %d parameters"
          (List.length description.Intrinsics.arguments);
      ]
  | Some description, Some signature ->
      let parameters, label = expected_of ~written description in
      if not (String.equal signature.Complete.callee written) then
        [ Printf.sprintf "callee %S" signature.Complete.callee ]
      else if
        List.length signature.Complete.parameters <> List.length parameters
      then
        [
          Printf.sprintf "arity %d, the table's %d"
            (List.length signature.Complete.parameters)
            (List.length parameters);
        ]
      else if signature.Complete.parameters <> parameters then
        [
          Printf.sprintf "parameters [%s], the table's [%s]"
            (String.concat "; " signature.Complete.parameters)
            (String.concat "; " parameters);
        ]
      else if not (String.equal signature.Complete.label label) then
        [
          Printf.sprintf "label %S, the table's %S" signature.Complete.label
            label;
        ]
      else []

(* The library against the compiler's counts. *)
let judge_library_against_compiler (library : Intrinsics.t_signature option)
    (answer : Oracle.answer) =
  match (library, answer) with
  | None, (Oracle.Refused _ | Oracle.Rule) -> []
  | Some _, (Oracle.Refused _ | Oracle.Rule) ->
      [ "the library describes it, the compiler refuses it" ]
  | None, Oracle.Described { args; results } ->
      [
        Printf.sprintf
          "the library refuses it, the compiler describes %d arguments and %d \
           results"
          args results;
      ]
  | Some description, Oracle.Described { args; results } ->
      let library_args = List.length description.Intrinsics.arguments in
      let library_results = List.length description.Intrinsics.results in
      if library_args = args && library_results = results then []
      else
        [
          Printf.sprintf
            "the library describes %d arguments and %d results, the compiler \
             %d and %d"
            library_args library_results args results;
        ]

(* ------------------------------------------------------------------ *)
(* The table part *)

type table_row = {
  arch : string;
  written : string;
  library : Intrinsics.t_signature option;
  compiler : Oracle.answer option;  (** [None] when no compiler is given *)
  shown : Complete.signature option;
}

let judge_table (row : table_row) =
  judge_library ~written:row.written row.library row.shown
  @
  match row.compiler with
  | None -> []
  | Some answer ->
      judge_against answer row.shown
      @ judge_library_against_compiler row.library answer

(* Remove a directory tree this run made. *)
let rec remove_tree path =
  if Sys.is_directory path then (
    Array.iter
      (fun name -> remove_tree (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path)
  else Sys.remove path

(* The server's answer to every spelling of one architecture. The document is
   written under its own project, so the engine analyses it as the server
   would, and each call is asked with the snapshot's own architecture. *)
let server_answers ~control ~root ~arch (spellings : spelling list) =
  let directory = Filename.concat root arch in
  if not (Sys.file_exists directory) then Sys.mkdir directory 0o700;
  let project = Filename.concat directory Project.file_name in
  Out_channel.with_open_text project (fun channel ->
      Printf.fprintf channel "[jasmin]\narch = %s\n" arch);
  let path = Filename.concat directory "probe.jazz" in
  let buffer = Buffer.create 65536 in
  Buffer.add_string buffer "fn probe() {\n  reg u64 x;\n";
  let calls =
    List.map
      (fun (spelling : spelling) ->
        let prefix = "  x = #" ^ spelling.text ^ "(" in
        let byte = Buffer.length buffer + String.length prefix in
        Buffer.add_string buffer prefix;
        Buffer.add_string buffer ");\n";
        byte)
      spellings
  in
  Buffer.add_string buffer "}\n";
  let text = Buffer.contents buffer in
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel text);
  let uri = "file://" ^ path in
  let engine = Engine.create () in
  Engine.set_root engine (Some directory);
  Engine.open_document engine ~uri ~version:1 ~text;
  ignore (Engine.check engine ~uri);
  match Engine.snapshot engine ~uri with
  | None -> die "the engine kept no analysis of %s" path
  | Some snapshot ->
      let analysed = Arch.to_string snapshot.Engine.arch in
      if not (String.equal analysed arch) then
        die "the engine analysed %s under %s, not %s" path analysed arch;
      List.map
        (fun byte ->
          let asked =
            match control with
            | Ignore_arch -> Arch.X86_64
            | No_control | Drop_parameter -> snapshot.Engine.arch
          in
          let shown =
            Complete.signature_help ~arch:asked ~facts:snapshot.Engine.facts
              ~index:snapshot.Engine.index ~resolve:snapshot.Engine.resolve
              ~byte ()
          in
          shown_under ~control shown)
        calls

(* ------------------------------------------------------------------ *)
(* The corpus part *)

let is_source name =
  Filename.check_suffix name ".jazz" || Filename.check_suffix name ".jinc"

(* Sorted, so two runs over the same tree print the same lines. *)
let rec source_files directory =
  let entries =
    Sys.readdir directory |> Array.to_list |> List.sort String.compare
  in
  List.concat_map
    (fun name ->
      let path = Filename.concat directory name in
      if Sys.is_directory path then source_files path
      else if is_source name then [ path ]
      else [])
    entries

let named_arch = function
  | "x86-64" -> Some Arch.X86_64
  | "arm-m4" -> Some Arch.Arm_m4
  | "armv8a" -> Some Arch.Arm_v8a
  | "riscv" -> Some Arch.Riscv
  | _ -> None

let add_unique arch archs = if List.mem arch archs then archs else arch :: archs

(* The corpus's own attribution. A directory that names an architecture adds
   it to the ones it inherits; [common] stands for the three the compiler's
   [tests/common.ml] lists. A path that names none is compared under every
   architecture the linked compiler has. *)
let archs_of_path path =
  let add archs component =
    match named_arch component with
    | Some arch -> add_unique arch archs
    | None ->
        if String.equal component "common" then
          List.fold_left
            (fun archs arch -> add_unique arch archs)
            archs
            [ Arch.X86_64; Arch.Arm_m4; Arch.Riscv ]
        else archs
  in
  match List.rev (List.fold_left add [] (String.split_on_char '/' path)) with
  | [] -> Arch.all
  | archs -> archs

let is_blank = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

(* The byte just inside the parenthesis of a call whose name ends at [start],
   or [None] when the name is not followed by one. *)
let open_paren_after text start =
  let length = String.length text in
  let rec skip index =
    if index >= length then None
    else if is_blank text.[index] then skip (index + 1)
    else if text.[index] = '(' then Some (index + 1)
    else None
  in
  skip start

let line_of text byte =
  let byte = min byte (String.length text) in
  let line = ref 1 in
  for index = 0 to byte - 1 do
    if text.[index] = '\n' then incr line
  done;
  !line

type corpus_row = {
  arch : string;
  where : string;
  written : string;
  answer : Oracle.answer;  (** the compiler's answer, always present here *)
  shown : Complete.signature option;
}

type corpus_stats = {
  mutable files : int;
  mutable unparsed : string list;  (** the files with a parse diagnostic *)
  mutable calls : int;
  mutable bare : int;
}

let corpus_rows ~control ~oracle root =
  let stats = { files = 0; unparsed = []; calls = 0; bare = 0 } in
  let rows = ref [] in
  List.iter
    (fun path ->
      stats.files <- stats.files + 1;
      let text =
        In_channel.with_open_bin path (fun channel ->
            really_input_string channel (in_channel_length channel))
      in
      let index, diagnostics = Index.of_text ~file:path text in
      if diagnostics <> [] then stats.unparsed <- path :: stats.unparsed;
      let resolve =
        Resolve.of_text ~idirs:[] ~read:(fun _ -> None) ~file:path text
      in
      List.iter
        (fun (occurrence : Index.occurrence) ->
          if occurrence.Index.role = Index.Prim then
            let span = occurrence.Index.span in
            match open_paren_after text span.Jide_lang.Span.end_byte with
            | None -> stats.bare <- stats.bare + 1
            | Some byte ->
                stats.calls <- stats.calls + 1;
                let written = occurrence.Index.name in
                let line = line_of text span.Jide_lang.Span.start_byte in
                List.iter
                  (fun (arch : Arch.t) ->
                    let asked =
                      match control with
                      | Ignore_arch -> Arch.X86_64
                      | No_control | Drop_parameter -> arch
                    in
                    let arch_name = Arch.to_string arch in
                    let answer =
                      Oracle.answer oracle ~arch:arch_name ~written
                        ~rule:(List.mem written pseudo_names)
                    in
                    let shown =
                      shown_under ~control
                        (Complete.signature_help ~arch:asked ~index ~resolve
                           ~byte ())
                    in
                    rows :=
                      {
                        arch = arch_name;
                        where = Printf.sprintf "%s:%d" path line;
                        written;
                        answer;
                        shown;
                      }
                      :: !rows)
                  (archs_of_path path))
        index.Index.occurrences)
    (source_files root);
  (List.rev !rows, stats)

(* ------------------------------------------------------------------ *)
(* The run *)

let make_root () =
  let root =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "jide-arity-%d" (Unix.getpid ()))
  in
  if Sys.file_exists root then remove_tree root;
  Sys.mkdir root 0o700;
  root

let report_limit = 40

(* Print the first differences, and return how many there are. *)
let print_differences lines =
  List.iteri
    (fun index line -> if index < report_limit then print_endline line)
    lines;
  if List.length lines > report_limit then
    Printf.printf "... %d more differences not shown\n"
      (List.length lines - report_limit);
  List.length lines

let described_of (answer : Oracle.answer) =
  match answer with
  | Oracle.Described _ -> true
  | Oracle.Refused _ | Oracle.Rule -> false

(* The whole run: the table, compared with the library and with the compiler,
   then the corpus when one is named. Returns the number of differences. *)
let run ~root ~control ~corpus ~jasminc =
  Printf.printf "intrinsic arity: library %s, control %s\n"
    Jide_jasmin.Version.jasmin (control_name control);
  let oracle =
    Option.map
      (fun jasminc ->
        let probes = Filename.concat root "probes" in
        Sys.mkdir probes 0o700;
        let oracle, version = Oracle.create ~jasminc ~dir:probes in
        Printf.printf "compiler %s: %s\n" jasminc version;
        oracle)
      jasminc
  in
  (* The table part. *)
  let table_rows =
    List.concat_map
      (fun arch ->
        let spellings = spellings_of ~arch in
        let shown = server_answers ~control ~root ~arch spellings in
        List.map2
          (fun (spelling : spelling) shown ->
            let compiler =
              Option.map
                (fun oracle ->
                  Oracle.answer oracle ~arch ~written:spelling.text
                    ~rule:(List.mem spelling.text pseudo_names))
                oracle
            in
            {
              arch;
              written = spelling.text;
              library = spelling.library;
              compiler;
              shown;
            })
          spellings shown)
      Intrinsics.arch_names
  in
  List.iter
    (fun arch ->
      let mine =
        List.filter
          (fun (row : table_row) -> String.equal row.arch arch)
          table_rows
      in
      let described (row : table_row) =
        match row.compiler with
        | Some answer -> described_of answer
        | None -> Option.is_some row.library
      in
      let described_count = List.length (List.filter described mine) in
      let differing =
        List.length
          (List.filter (fun (row : table_row) -> judge_table row <> []) mine)
      in
      Printf.printf
        "table %s: %d names, %d spellings, %d described, %d refused, %d differ\n"
        arch
        (List.length (Intrinsics.all ~arch))
        (List.length mine) described_count
        (List.length mine - described_count)
        differing)
    Intrinsics.arch_names;
  let table_lines =
    List.concat_map
      (fun (row : table_row) ->
        List.map
          (fun why ->
            Printf.sprintf "DIFF table [%s] %s: %s" row.arch row.written why)
          (judge_table row))
      table_rows
  in
  let table_differences = print_differences table_lines in
  (* The corpus part. *)
  let corpus_differences =
    match (corpus, oracle) with
    | None, _ -> 0
    | Some _, None ->
        die "a corpus is compared with the compiler, so --jasminc is needed"
    | Some dir, Some oracle ->
        let rows, stats = corpus_rows ~control ~oracle dir in
        Printf.printf
          "corpus %s: %d files (%d with a parse diagnostic), %d intrinsic \
           calls, %d primitives written without a call, %d comparisons\n"
          dir stats.files
          (List.length stats.unparsed)
          stats.calls stats.bare (List.length rows);
        List.iter
          (fun path -> Printf.printf "  parse diagnostic: %s\n" path)
          (List.rev stats.unparsed);
        List.iter
          (fun arch ->
            let mine =
              List.filter
                (fun (row : corpus_row) -> String.equal row.arch arch)
                rows
            in
            let described =
              List.length
                (List.filter
                   (fun (row : corpus_row) -> described_of row.answer)
                   mine)
            in
            let differing =
              List.length
                (List.filter
                   (fun (row : corpus_row) ->
                     judge_against row.answer row.shown <> [])
                   mine)
            in
            Printf.printf
              "corpus %s: %d comparisons, %d described, %d refused, %d differ\n"
              arch (List.length mine) described
              (List.length mine - described)
              differing)
          Intrinsics.arch_names;
        let lines =
          List.concat_map
            (fun (row : corpus_row) ->
              List.map
                (fun why ->
                  Printf.sprintf "DIFF corpus [%s] %s (%s): %s" row.arch
                    row.written row.where why)
                (judge_against row.answer row.shown))
            rows
        in
        let count = print_differences lines in
        Printf.printf "corpus differences: %d\n" count;
        count
  in
  Option.iter
    (fun (oracle : Oracle.t) ->
      Printf.printf "compiler probes: %d\n" oracle.Oracle.runs)
    oracle;
  table_differences + corpus_differences

(* The command line, then the run. A failure raised anywhere is printed by
   [main]'s caller, and the exit status follows it. *)
let main () =
  let control = ref No_control and corpus = ref None and jasminc = ref None in
  let rec parse = function
    | [] -> `Run
    | ("-h" | "--help") :: _ -> `Help
    | [ "--control" ] | [ "--jasminc" ] ->
        die "an option needs a value\n%s" usage
    | "--control" :: name :: rest ->
        control := control_of_string name;
        parse rest
    | "--jasminc" :: path :: rest ->
        jasminc := Some path;
        parse rest
    | argument :: rest
      when String.length argument > 0
           && argument.[0] <> '-'
           && Option.is_none !corpus ->
        corpus := Some argument;
        parse rest
    | argument :: _ -> die "unexpected argument %s\n%s" argument usage
  in
  match parse (List.tl (Array.to_list Sys.argv)) with
  | `Help ->
      print_endline usage;
      0
  | `Run ->
      (match !corpus with
      | Some dir when not (Sys.file_exists dir && Sys.is_directory dir) ->
          die "%s is not a directory" dir
      | _ -> ());
      if Option.is_some !corpus && Option.is_none !jasminc then
        die "a corpus is compared with the compiler, so --jasminc is needed";
      let root = make_root () in
      let total =
        Fun.protect
          ~finally:(fun () -> remove_tree root)
          (fun () ->
            run ~root ~control:!control ~corpus:!corpus ~jasminc:!jasminc)
      in
      if total = 0 then (
        print_endline "result: every comparison agrees";
        0)
      else (
        Printf.printf "result: %d comparisons differ\n" total;
        1)

