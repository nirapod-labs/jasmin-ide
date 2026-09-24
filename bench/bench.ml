(* Timing for the budgets of design section 15 that the server itself
   decides: the whole check of a unit, the interval from a keystroke to the
   diagnostics it produces, and the editor requests (completion, signature
   help, hover and definition from a snapshot, and references and rename over
   a project).

   Every measurement runs the server's own code in this process, so a number
   is the work the server does and not a model of it. A build must not run
   beside a measurement: `make bench` refuses to start while one is running,
   and bench/run.sh says how it decides. *)

let now = Unix.gettimeofday
let default_runs = 20
let default_count = 5
let default_arch_name = "x86-64"
let default_call_conv_name = "linux"

let die message =
  prerr_endline ("bench: " ^ message);
  exit 2

let parse_arch name =
  match Jide_jasmin.Adapter.arch_of_string name with
  | Some arch -> arch
  | None ->
      die
        (Printf.sprintf "unknown architecture: %s (known: %s)" name
           (String.concat ", " Jide_jasmin.Adapter.arch_names))

let parse_call_conv name =
  match Jide_jasmin.Adapter.call_conv_of_string name with
  | Some call_conv -> call_conv
  | None ->
      die
        (Printf.sprintf "unknown calling convention: %s (known: linux, windows)"
           name)

let usage =
  String.concat "\n"
    [
      "usage: bench <command> [options]";
      "";
      "commands:";
      "  whole      time a whole check (parse and type) of each unit";
      "  keystroke  time a didChange from the protocol loop to the diagnostics";
      "  request    time completion, signature help, hover, definition,";
      "             references and rename on a warm snapshot of each unit";
      "  largest    print the largest Jasmin files under a directory";
      "";
      "options:";
      "  --units FILE      a table of units, one per line:";
      "                    label<TAB>arch<TAB>idirs<TAB>path";
      "                    arch is a name or `auto`; idirs is `-` or a";
      "                    comma-separated list of name=path";
      "  --arch NAME       architecture for the files on the command line";
      "                    (default: the first path component that names one)";
      "  --call-conv NAME  calling convention (default: linux)";
      "  -I NAME=PATH      an include path, repeatable";
      "  --label NAME      the row name for the files on the command line";
      "  --runs N          checks per unit (default: 20)";
      "  --samples         print every run as well as the summary";
      "  --count N         how many files `largest` prints (default: 5)";
      "  -h, --help        print this text";
      "";
      "A file named on the command line without --arch takes the architecture";
      "its path names, so a corpus that keeps one directory per architecture";
      "needs no options.";
      "";
      "A request row is compared with its budget, in milliseconds: completion,";
      "signature help, hover and definition answer to 20, and references and";
      "rename to 100 over the project, which is every Jasmin file under the";
      "entry's directory. A request the server does not answer is reported as";
      "unavailable, one row for each, so the table is complete before the";
      "features are.";
      "";
      "The exit status is 0 when every unit ran, and 2 when the command line";
      "or an input is wrong.";
    ]

(* A unit is one file, the architecture it is checked for and the include
   paths its requires resolve through. [arch_name] is kept beside the parsed
   architecture because the adapter exposes the names and the parser, not
   the printer, and the table has to print what was used. *)
type unit_ = {
  label : string;
  arch_name : string;
  arch : Jide_jasmin.Adapter.arch;
  call_conv : Jide_jasmin.Adapter.call_conv;
  idirs : (string * string) list;
  file : string;
}

let read_file path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> text
  | exception Sys_error message -> die ("cannot read " ^ path ^ ": " ^ message)

(* The architecture a path names: the corpus keeps one directory per
   architecture, so the first component that is an architecture name is the
   one the file is checked for. *)
let arch_name_of_path path =
  let rec find = function
    | [] -> default_arch_name
    | part :: rest -> (
        match Jide_jasmin.Adapter.arch_of_string part with
        | Some _ -> part
        | None -> find rest)
  in
  find (String.split_on_char '/' path)

let make_unit ~label ~arch ~call_conv ~idirs ~file =
  if not (Sys.file_exists file) then die ("no such file: " ^ file);
  let arch_name =
    match arch with Some name -> name | None -> arch_name_of_path file
  in
  { label; arch_name; arch = parse_arch arch_name; call_conv; idirs; file }

let split_include value =
  match String.index_opt value '=' with
  | None | Some 0 -> die ("-I needs name=path, got: " ^ value)
  | Some index ->
      let name = String.sub value 0 index in
      let path =
        String.sub value (index + 1) (String.length value - index - 1)
      in
      (name, path)

let split_idirs = function
  | "-" | "" -> []
  | value -> List.map split_include (String.split_on_char ',' value)

(* -- statistics ---------------------------------------------------------- *)

let median values =
  match List.sort Float.compare values with
  | [] -> 0.0
  | sorted ->
      let count = List.length sorted in
      if count mod 2 = 1 then List.nth sorted (count / 2)
      else
        (List.nth sorted ((count / 2) - 1) +. List.nth sorted (count / 2))
        /. 2.0

(* Nearest rank: the ceil (q * n) -th smallest value. *)
let percentile q values =
  match List.sort Float.compare values with
  | [] -> 0.0
  | sorted ->
      let count = List.length sorted in
      let rank = int_of_float (ceil (q *. float_of_int count)) in
      List.nth sorted (max 1 (min count rank) - 1)

let maximum values =
  List.fold_left (fun best value -> Float.max best value) 0.0 values

let mean values =
  match values with
  | [] -> 0.0
  | values ->
      List.fold_left ( +. ) 0.0 values /. float_of_int (List.length values)

(* One run, in milliseconds, with the value the run produced. *)
let time f =
  let start = now () in
  let value = f () in
  ((now () -. start) *. 1000.0, value)

(* -- the whole check ----------------------------------------------------- *)

let check unit_ ~text =
  match
    Jide_jasmin.Adapter.check unit_.arch unit_.call_conv ~file:unit_.file ~text
      ~idirs:unit_.idirs
  with
  | result -> result
  | exception exn ->
      die
        (Printf.sprintf "%s: the check raised %s" unit_.label
           (Printexc.to_string exn))

let diagnostics_of unit_ ~text result =
  Jide_check.Report.of_check ~file:unit_.file ~text result

let errors_of diagnostics =
  List.length
    (List.filter
       (fun (diagnostic : Jide_lang.Diagnostic.t) ->
         diagnostic.severity = Jide_lang.Diagnostic.Error)
       diagnostics)

type whole_result = {
  whole_unit : unit_;
  checks : float list;
  parses : float list;
  diagnostics : int;
  errors : int;
}

let measure_whole ~runs unit_ =
  let text = read_file unit_.file in
  (* The parses come first: twenty checks of a closure-heavy unit leave a
     large heap behind, and a parse timed after them would carry the
     collector's work as well as its own. *)
  let parses =
    List.init runs (fun _ ->
        fst (time (fun () -> Jide_jasmin.Adapter.parse ~name:unit_.file text)))
  in
  let checks =
    List.init runs (fun _ -> fst (time (fun () -> check unit_ ~text)))
  in
  let diagnostics = diagnostics_of unit_ ~text (check unit_ ~text) in
  {
    whole_unit = unit_;
    checks;
    parses;
    diagnostics = List.length diagnostics;
    errors = errors_of diagnostics;
  }

let print_whole_samples results =
  List.iter
    (fun result ->
      List.iteri
        (fun index milliseconds ->
          Printf.printf "whole %s %d %.4f\n" result.whole_unit.label (index + 1)
            milliseconds)
        result.checks)
    results

let print_whole_table results =
  Printf.printf "%-44s %-7s %3s %10s %8s %8s %8s %5s %6s\n" "unit" "arch" "n"
    "median" "p95" "max" "parse" "diags" "errors";
  List.iter
    (fun result ->
      Printf.printf "%-44s %-7s %3d %10.3f %8.3f %8.3f %8.3f %5d %6d\n"
        result.whole_unit.label result.whole_unit.arch_name
        (List.length result.checks)
        (median result.checks)
        (percentile 0.95 result.checks)
        (maximum result.checks) (median result.parses) result.diagnostics
        result.errors)
    results

(* -- keystroke to diagnostics -------------------------------------------- *)

let request id method_ params =
  Jsonrpc.Packet.Request
    (Jsonrpc.Request.create ~id:(`Int id) ~method_
       ~params:(Jsonrpc.Structured.t_of_yojson params)
       ())

let notification method_ params =
  Jsonrpc.Packet.Notification
    (Jsonrpc.Notification.create ~method_
       ~params:(Jsonrpc.Structured.t_of_yojson params)
       ())

let initialize_params : Yojson.Safe.t = `Assoc [ ("capabilities", `Assoc []) ]

(* A server that is told the workspace root reads a project file there and
   takes the entries of the context rule from that directory, which is what a
   client sends and what a request over a project needs. *)
let initialize_params_at directory : Yojson.Safe.t =
  `Assoc
    [
      ("capabilities", `Assoc []);
      ( "rootUri",
        `String (Jide_lang.Uri.to_string (Jide_lang.Uri.of_path directory)) );
    ]

let open_params ~uri ~text : Yojson.Safe.t =
  `Assoc
    [
      ( "textDocument",
        `Assoc
          [
            ("uri", `String uri);
            ("languageId", `String "jasmin");
            ("version", `Int 1);
            ("text", `String text);
          ] );
    ]

let change_params ~uri ~version ~text : Yojson.Safe.t =
  `Assoc
    [
      ( "textDocument",
        `Assoc [ ("uri", `String uri); ("version", `Int version) ] );
      ("contentChanges", `List [ `Assoc [ ("text", `String text) ] ]);
    ]

(* One keystroke: a space typed at the start of the second line, which no
   Jasmin construct depends on, so the text stays valid and the check still
   measures a whole check. Attempt [n] inserts [n] spaces, so every run gets
   a text of its own, as a real edit does. *)
let edit text attempt =
  let offset =
    match String.index_opt text '\n' with Some index -> index + 1 | None -> 0
  in
  let rest = String.sub text offset (String.length text - offset) in
  String.sub text 0 offset ^ String.make attempt ' ' ^ rest

type keystroke_sample = {
  total : float;
  loop : float;
  checked : float;
  converted : float;
  found : int;
  failed : int;
}

type keystroke_result = {
  keystroke_unit : unit_;
  samples : keystroke_sample list;
  log : string list;
}

(* The interval the keystroke budget names: the packet arrives at the loop,
   the loop stores the change, the check runs and the diagnostics are built.
   The design's debounce and its publish step are not in the tree at this
   commit, so neither contributes here. *)
let run_keystroke ~attempt server unit_ base uri =
  let text = edit base attempt in
  let packet =
    notification "textDocument/didChange"
      (change_params ~uri ~version:(attempt + 1) ~text)
  in
  let start = now () in
  ignore (Jide_controller.Server.handle_packet server packet);
  let after_loop = now () in
  let stored =
    match
      Jide_engine.Engine.document (Jide_controller.Server.engine server) ~uri
    with
    | Some document -> Jide_engine.Document.text document
    | None -> die (unit_.label ^ ": the loop left the document out of the store")
  in
  if not (String.equal stored text) then
    die (unit_.label ^ ": the loop stored other text than the change carried");
  let result = check unit_ ~text:stored in
  let after_check = now () in
  let diagnostics = diagnostics_of unit_ ~text:stored result in
  let stop = now () in
  {
    total = (stop -. start) *. 1000.0;
    loop = (after_loop -. start) *. 1000.0;
    checked = (after_check -. after_loop) *. 1000.0;
    converted = (stop -. after_check) *. 1000.0;
    found = List.length diagnostics;
    failed = errors_of diagnostics;
  }

let measure_keystroke ~runs unit_ =
  let base = read_file unit_.file in
  let uri = Jide_lang.Uri.to_string (Jide_lang.Uri.of_path unit_.file) in
  let log = ref [] in
  let server =
    Jide_controller.Server.create
      ~log:(fun message -> log := message :: !log)
      ()
  in
  ignore
    (Jide_controller.Server.handle_packet server
       (request 1 "initialize" initialize_params));
  (match Jide_controller.Server.state server with
  | Jide_controller.Server.Running -> ()
  | _ -> die (unit_.label ^ ": the server did not reach the running state"));
  ignore
    (Jide_controller.Server.handle_packet server
       (notification "textDocument/didOpen" (open_params ~uri ~text:base)));
  let samples =
    List.init runs (fun index ->
        run_keystroke ~attempt:(index + 1) server unit_ base uri)
  in
  { keystroke_unit = unit_; samples; log = List.rev !log }

let print_keystroke_samples results =
  List.iter
    (fun result ->
      List.iteri
        (fun index sample ->
          Printf.printf "keystroke %s %d %.4f %.4f %.4f %.4f %d\n"
            result.keystroke_unit.label (index + 1) sample.total sample.loop
            sample.checked sample.converted sample.found)
        result.samples)
    results

let print_keystroke_table results =
  Printf.printf "%-44s %-7s %3s %10s %8s %8s %8s %8s %8s %5s %6s\n" "unit"
    "arch" "n" "median" "p95" "max" "loop" "check" "convert" "diags" "errors";
  List.iter
    (fun result ->
      let field f = List.map f result.samples in
      Printf.printf
        "%-44s %-7s %3d %10.3f %8.3f %8.3f %8.3f %8.3f %8.3f %5d %6d\n"
        result.keystroke_unit.label result.keystroke_unit.arch_name
        (List.length result.samples)
        (median (field (fun sample -> sample.total)))
        (percentile 0.95 (field (fun sample -> sample.total)))
        (maximum (field (fun sample -> sample.total)))
        (median (field (fun sample -> sample.loop)))
        (median (field (fun sample -> sample.checked)))
        (median (field (fun sample -> sample.converted)))
        (match result.samples with [] -> 0 | sample :: _ -> sample.found)
        (List.fold_left
           (fun total sample -> total + sample.failed)
           0 result.samples);
      List.iter
        (fun message ->
          Printf.printf "  log %s: %s\n" result.keystroke_unit.label message)
        result.log)
    results

(* -- the editor requests -------------------------------------------------- *)

(* Two budgets of design section 15 are decided by one request on a snapshot
   the server already holds: completion, signature help, hover and definition
   within 20 ms, and references and rename over a project of a library tree's
   size within 100 ms.

   Each request is sent through the server's own request path, the one an
   editor's client takes, and the interval is that path alone: the snapshot is
   open and analysed before the first run, so a number is the work the request
   does and not the check that came before it.

   A request the server does not answer yet is reported as unavailable, one
   row for each of the six requests. The harness is then complete before the
   features are, and the day a handler lands the same row measures it with no
   change here: the row is the server's answer, not a stand-in for it. *)

type feature =
  | Completion
  | Signature_help
  | Hover
  | Definition
  | References
  | Rename

let features =
  [ Completion; Signature_help; Hover; Definition; References; Rename ]

let feature_name = function
  | Completion -> "completion"
  | Signature_help -> "signature-help"
  | Hover -> "hover"
  | Definition -> "definition"
  | References -> "references"
  | Rename -> "rename"

let method_of_feature = function
  | Completion -> "textDocument/completion"
  | Signature_help -> "textDocument/signatureHelp"
  | Hover -> "textDocument/hover"
  | Definition -> "textDocument/definition"
  | References -> "textDocument/references"
  | Rename -> "textDocument/rename"

(* The budget each request answers to, in milliseconds. *)
let snapshot_budget = 20.0
let project_budget = 100.0

let budget_of_feature = function
  | Completion | Signature_help | Hover | Definition -> snapshot_budget
  | References | Rename -> project_budget

(* References and rename work over the project rather than one file, so they
   are measured with every Jasmin file under the entry's directory open: that
   is the project, and its size is printed beside the rows. *)
let over_project = function
  | References | Rename -> true
  | Completion | Signature_help | Hover | Definition -> false

let position_json (position : Jide_lang.Position.t) =
  `Assoc
    [ ("line", `Int position.line); ("character", `Int position.character) ]

(* The name every rename asks for. A symbol of the project, so the request
   reaches the rename rules rather than a refusal for a keyword. *)
let rename_name = "renamed_by_the_benchmark"

(* The parameters of the six requests: the document and the position, then
   the one field a references or a rename request adds. *)
let request_params feature ~uri ~position =
  let base =
    [
      ("textDocument", `Assoc [ ("uri", `String uri) ]);
      ("position", position_json position);
    ]
  in
  match feature with
  | References ->
      `Assoc
        (base @ [ ("context", `Assoc [ ("includeDeclaration", `Bool true) ]) ])
  | Rename -> `Assoc (base @ [ ("newName", `String rename_name) ])
  | Completion | Signature_help | Hover | Definition -> `Assoc base

(* The byte a request is made at. The name of the first function the entry
   declares is a symbol every Jasmin file has; signature help asks at the
   first call instead, because a signature belongs to a call, and falls back
   to the function's name when the file calls nothing. A file that declares
   and calls nothing is asked at its first byte. *)
let request_byte (index : Jide_engine.Index.t) feature =
  let declaration_byte =
    match
      List.find_opt
        (fun (declaration : Jide_engine.Index.declaration) ->
          declaration.kind = Jide_engine.Index.Function)
        index.declarations
    with
    | Some declaration -> Some declaration.name_span.start_byte
    | None -> (
        match index.declarations with
        | declaration :: _ -> Some declaration.name_span.start_byte
        | [] -> None)
  in
  let call_byte =
    match
      List.find_opt
        (fun (occurrence : Jide_engine.Index.occurrence) ->
          occurrence.role = Jide_engine.Index.Call)
        index.occurrences
    with
    | Some occurrence -> Some occurrence.span.start_byte
    | None -> None
  in
  match (feature, call_byte, declaration_byte) with
  | Signature_help, Some byte, _ -> byte
  | _, _, Some byte -> byte
  | _, Some byte, None -> byte
  | _, None, None -> 0

(* The analysis the server asks for, over its own engine: the same adapter the
   executable wires, so the snapshot a request reads is the one the server
   stores. *)
let engine_analysis engine : Jide_controller.Server.analysis =
  {
    check = (fun ~uri -> ignore (Jide_engine.Engine.check engine ~uri));
    diagnostics =
      (fun ~uri ->
        match Jide_engine.Engine.check engine ~uri with
        | Some (analysis : Jide_engine.Engine.analysis) ->
            Some (analysis.version, analysis.diagnostics)
        | None -> None);
    forget = (fun ~uri:_ -> ());
  }

let absolute path =
  match Unix.realpath path with
  | real -> real
  | exception Unix.Unix_error _ ->
      if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
      else path

let is_jasmin_file name =
  Filename.check_suffix name ".jazz" || Filename.check_suffix name ".jinc"

let skip_directory name =
  String.equal name "_build"
  || String.equal name "node_modules"
  || (String.length name > 0 && Char.equal name.[0] '.')

(* Open every Jasmin file under [directory] in the server's store, except
   [skip], which is the entry the snapshot already holds: the project a
   references or a rename request reads. The count and the bytes come back so
   the table can print the size of the tree a number belongs to. *)
let open_project ~skip server directory =
  let files = ref 0 in
  let bytes = ref 0 in
  let rec walk path =
    match Sys.readdir path with
    | exception Sys_error _ -> ()
    | entries ->
        Array.iter
          (fun entry ->
            let child = Filename.concat path entry in
            match Unix.stat child with
            | { Unix.st_kind = Unix.S_DIR; _ } ->
                if not (skip_directory entry) then walk child
            | { Unix.st_kind = Unix.S_REG; _ }
              when is_jasmin_file entry && not (String.equal child skip) -> (
                match In_channel.with_open_bin child In_channel.input_all with
                | text ->
                    let uri =
                      Jide_lang.Uri.to_string (Jide_lang.Uri.of_path child)
                    in
                    ignore
                      (Jide_controller.Server.handle_packet server
                         (notification "textDocument/didOpen"
                            (open_params ~uri ~text)));
                    incr files;
                    bytes := !bytes + String.length text
                | exception Sys_error _ -> ())
            | _ -> ()
            | exception Unix.Unix_error _ -> ())
          entries
  in
  walk directory;
  (!files, !bytes)

type request_status = Answered | Unavailable | Refused of string

type feature_result = {
  feature : feature;
  status : request_status;
  samples : float list;  (** milliseconds, one per run *)
}

type request_result = {
  request_unit : unit_;
  rows : feature_result list;
  project_files : int;  (** 0 when no project request was measured *)
  project_bytes : int;
}

let answer_of_response (response : Jsonrpc.Response.t) =
  match response.result with
  | Ok _ -> Answered
  | Error error -> (
      match error.Jsonrpc.Response.Error.code with
      | Jsonrpc.Response.Error.Code.MethodNotFound -> Unavailable
      | _ -> Refused error.message)

let measure_requests ~runs unit_ =
  let text = read_file unit_.file in
  let uri = Jide_lang.Uri.to_string (Jide_lang.Uri.of_path unit_.file) in
  let directory = absolute (Filename.dirname unit_.file) in
  let engine = Jide_engine.Engine.create () in
  let server =
    Jide_controller.Server.create
      ~log:(fun _ -> ())
      ~engine ~analysis:(engine_analysis engine) ()
  in
  ignore
    (Jide_controller.Server.handle_packet server
       (request 1 "initialize" (initialize_params_at directory)));
  (match Jide_controller.Server.state server with
  | Jide_controller.Server.Running -> ()
  | _ -> die (unit_.label ^ ": the server did not reach the running state"));
  Jide_engine.Engine.set_settings engine
    {
      Jide_engine.Engine.arch = unit_.arch;
      call_conv = unit_.call_conv;
      idirs = unit_.idirs;
    };
  let index, _ = Jide_engine.Index.of_text ~file:unit_.file text in
  ignore
    (Jide_controller.Server.handle_packet server
       (notification "textDocument/didOpen" (open_params ~uri ~text)));
  ignore (Jide_controller.Server.ensure_checked server ~uri);
  let document =
    match Jide_engine.Engine.document engine ~uri with
    | Some document -> document
    | None -> die (unit_.label ^ ": the loop left the document out of the store")
  in
  if not (String.equal (Jide_engine.Document.text document) text) then
    die (unit_.label ^ ": the loop stored other text than the one opened");
  (match Jide_engine.Engine.analysis engine ~uri with
  | Some _ -> ()
  | None -> die (unit_.label ^ ": the snapshot holds no analysis of the entry"));
  let next_id = ref 1 in
  let ask feature =
    incr next_id;
    let id = !next_id in
    let position =
      Jide_engine.Document.position_of_byte document
        (request_byte index feature)
    in
    let packets =
      Jide_controller.Server.handle_packet server
        (request id
           (method_of_feature feature)
           (request_params feature ~uri ~position))
    in
    match
      List.find_map
        (function
          | Jsonrpc.Packet.Response response
            when Jsonrpc.Id.equal response.Jsonrpc.Response.id (`Int id) ->
              Some response
          | _ -> None)
        packets
    with
    | Some response -> response
    | None ->
        die
          (Printf.sprintf "%s: no answer to %s" unit_.label
             (method_of_feature feature))
  in
  let project = ref None in
  let open_the_project () =
    match !project with
    | Some sizes -> sizes
    | None ->
        (* The entry is already open, so it is not opened twice; it is still
           part of the project the size counts. *)
        let files, bytes =
          open_project ~skip:(absolute unit_.file) server directory
        in
        let sizes = (files + 1, bytes + String.length text) in
        project := Some sizes;
        sizes
  in
  let measure feature =
    if over_project feature then ignore (open_the_project ());
    match answer_of_response (ask feature) with
    | Unavailable -> { feature; status = Unavailable; samples = [] }
    | Refused message -> { feature; status = Refused message; samples = [] }
    | Answered ->
        let samples =
          List.init runs (fun _ -> fst (time (fun () -> ignore (ask feature))))
        in
        { feature; status = Answered; samples }
  in
  let rows = List.map measure features in
  let project_files, project_bytes =
    match !project with Some sizes -> sizes | None -> (0, 0)
  in
  { request_unit = unit_; rows; project_files; project_bytes }

let request_state row =
  match row.status with
  | Unavailable -> "unavailable"
  | Refused message -> "refused: " ^ message
  | Answered ->
      if percentile 0.95 row.samples <= budget_of_feature row.feature then "met"
      else "missed"

let print_request_samples results =
  List.iter
    (fun result ->
      List.iter
        (fun row ->
          List.iteri
            (fun index milliseconds ->
              Printf.printf "request %s %s %d %.4f\n" result.request_unit.label
                (feature_name row.feature) (index + 1) milliseconds)
            row.samples)
        result.rows)
    results

let print_request_table results =
  Printf.printf "%-44s %-14s %-7s %3s %7s %10s %8s %8s %s\n" "unit" "request"
    "arch" "n" "budget" "median" "p95" "max" "status";
  List.iter
    (fun result ->
      List.iter
        (fun row ->
          let number f =
            match row.samples with
            | [] -> "-"
            | samples -> Printf.sprintf "%.3f" (f samples)
          in
          Printf.printf "%-44s %-14s %-7s %3d %7.1f %10s %8s %8s %s\n"
            result.request_unit.label (feature_name row.feature)
            result.request_unit.arch_name (List.length row.samples)
            (budget_of_feature row.feature)
            (number median)
            (number (percentile 0.95))
            (number maximum) (request_state row))
        result.rows;
      if result.project_files > 0 then
        Printf.printf "  project %s: %d files, %d bytes\n"
          result.request_unit.label result.project_files result.project_bytes)
    results

(* The rows in one line: a reader of a gate log sees whether the measurement
   is complete before reading any number. *)
let print_request_tally results =
  let rows = List.concat_map (fun result -> result.rows) results in
  let count what =
    List.fold_left
      (fun total row -> if what row then total + 1 else total)
      0 rows
  in
  let measured = count (fun row -> row.status = Answered) in
  let met = count (fun row -> String.equal (request_state row) "met") in
  let missed = count (fun row -> String.equal (request_state row) "missed") in
  let unavailable = count (fun row -> row.status = Unavailable) in
  let refused =
    count (function { status = Refused _; _ } -> true | _ -> false)
  in
  Printf.printf
    "request: %d rows, %d measured (%d met, %d missed), %d unavailable"
    (List.length rows) measured met missed unavailable;
  if refused > 0 then Printf.printf ", %d refused" refused;
  print_newline ();
  if refused > 0 then die "the server refused a request; the table says which"

(* -- the largest corpus files -------------------------------------------- *)

let largest ~count directory =
  if not (Sys.is_directory directory) then die ("not a directory: " ^ directory);
  let found = ref [] in
  let rec walk directory =
    match Sys.readdir directory with
    | entries ->
        Array.iter
          (fun entry ->
            let path = Filename.concat directory entry in
            match Unix.stat path with
            | { Unix.st_kind = Unix.S_DIR; _ } -> walk path
            | { Unix.st_kind = Unix.S_REG; _ } when is_jasmin_file entry ->
                found := (path, (Unix.stat path).Unix.st_size) :: !found
            | _ -> ()
            | exception Unix.Unix_error _ -> ())
          entries
    | exception Sys_error _ -> ()
  in
  walk directory;
  let sorted = List.sort (fun (_, a) (_, b) -> Int.compare b a) !found in
  let rec take wanted = function
    | _ when wanted <= 0 -> []
    | [] -> []
    | value :: rest -> value :: take (wanted - 1) rest
  in
  List.iter
    (fun (path, size) -> Printf.printf "%d\t%s\n" size path)
    (take count sorted)

(* -- the command line ---------------------------------------------------- *)

type command = Whole | Keystroke | Request | Largest

type options = {
  command : command option;
  units : string option;
  arch : string option;
  call_conv : string;
  idirs : (string * string) list;
  label : string option;
  runs : int;
  samples : bool;
  count : int;
  files : string list;
}

let empty_options =
  {
    command = None;
    units = None;
    arch = None;
    call_conv = default_call_conv_name;
    idirs = [];
    label = None;
    runs = default_runs;
    samples = false;
    count = default_count;
    files = [];
  }

let positive option value =
  match int_of_string_opt value with
  | Some number when number > 0 -> number
  | _ -> die (Printf.sprintf "%s needs a positive number, got: %s" option value)

let command_of_word = function
  | "whole" -> Whole
  | "keystroke" -> Keystroke
  | "request" -> Request
  | "largest" -> Largest
  | word ->
      die ("unknown command: " ^ word ^ " (whole, keystroke, request, largest)")

let parse_options arguments =
  let rec loop options rest =
    match rest with
    | [] -> options
    | ("-h" | "--help") :: _ ->
        print_endline usage;
        exit 0
    | "--samples" :: rest -> loop { options with samples = true } rest
    | "--units" :: value :: rest ->
        loop { options with units = Some value } rest
    | "--arch" :: value :: rest -> loop { options with arch = Some value } rest
    | "--call-conv" :: value :: rest ->
        loop { options with call_conv = value } rest
    | "-I" :: value :: rest ->
        loop
          { options with idirs = options.idirs @ [ split_include value ] }
          rest
    | "--label" :: value :: rest ->
        loop { options with label = Some value } rest
    | "--runs" :: value :: rest ->
        loop { options with runs = positive "--runs" value } rest
    | "--count" :: value :: rest ->
        loop { options with count = positive "--count" value } rest
    | option :: _ when String.length option > 0 && Char.equal option.[0] '-' ->
        die ("unknown option: " ^ option)
    | _ :: _ when options.units <> None ->
        die
          "the files of a units table are in the table, not on the command line"
    | word :: rest -> (
        match options.command with
        | None ->
            loop { options with command = Some (command_of_word word) } rest
        | Some _ -> loop { options with files = options.files @ [ word ] } rest)
  in
  loop empty_options arguments

let read_units ~call_conv path =
  let text = read_file path in
  let lines = String.split_on_char '\n' text in
  List.filter_map
    (fun line ->
      let line = String.trim line in
      if String.equal line "" || Char.equal line.[0] '#' then None
      else
        match String.split_on_char '\t' line with
        | [ label; arch; idirs; file ] ->
            let arch =
              match arch with "auto" | "-" -> None | name -> Some name
            in
            Some
              (make_unit ~label ~arch ~call_conv ~idirs:(split_idirs idirs)
                 ~file)
        | _ ->
            die
              ("a units line needs four tab-separated fields (label, arch, \
                idirs, path): " ^ line))
    lines

let units_of options =
  let call_conv = parse_call_conv options.call_conv in
  let from_files =
    List.map
      (fun file ->
        let label =
          match options.label with Some label -> label | None -> file
        in
        make_unit ~label ~arch:options.arch ~call_conv ~idirs:options.idirs
          ~file)
      options.files
  in
  match options.units with
  | Some path -> read_units ~call_conv path
  | None -> from_files

let main arguments =
  let options = parse_options arguments in
  match options.command with
  | None -> die "no command: one of whole, keystroke, largest"
  | Some Largest -> (
      match options.files with
      | [ directory ] -> largest ~count:options.count directory
      | _ -> die "largest takes one directory")
  | Some ((Whole | Keystroke) as command) -> (
      let units = units_of options in
      if units = [] then die "no unit to measure: name a file or pass --units";
      match command with
      | Whole ->
          Printf.printf "whole check: %d runs per unit, milliseconds\n"
            options.runs;
          let results = List.map (measure_whole ~runs:options.runs) units in
          if options.samples then print_whole_samples results;
          print_whole_table results
      | Keystroke ->
          Printf.printf
            "keystroke to diagnostics, no debounce, %d runs per unit, \
             milliseconds\n"
            options.runs;
          let results = List.map (measure_keystroke ~runs:options.runs) units in
          if options.samples then print_keystroke_samples results;
          print_keystroke_table results
      | Largest -> ())

let () = main (List.tl (Array.to_list Sys.argv))
