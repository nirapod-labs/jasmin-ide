type settings = {
  arch : Jide_jasmin.Adapter.arch;
  call_conv : Jide_jasmin.Adapter.call_conv;
  idirs : (string * string) list;
}

type checker =
  arch:Jide_jasmin.Adapter.arch ->
  call_conv:Jide_jasmin.Adapter.call_conv ->
  file:string ->
  text:string ->
  idirs:(string * string) list ->
  Jide_jasmin.Adapter.check_result

(* The context stage: [Context.of_file] with the engine's own entries, include
   paths and reader supplied. It is a value so that a test can replace it,
   the same way it replaces the check. *)
type context_stage =
  entries:string list ->
  idirs:(string * string) list ->
  read:(string -> string option) ->
  string ->
  Context.t

type analysis = {
  uri : string;
  version : int;
  key : int * string;
  text : string;
  bom : int;
  context : Context.t;
  diagnostics : Jide_lang.Diagnostic.t list;
}

type t = {
  documents : (string, Document.t) Hashtbl.t;
  analyses : (string, analysis) Hashtbl.t;
  mutable root : string option;
  mutable settings : settings;
  mutable project : Project.loaded option;
  mutable context_entry : string option;
  mutable entries : string list option;
  check : checker;
  context_stage : context_stage;
  mutable analysis_count : int;
}

(* The adapter's name table starts with x86-64 and [linux], and both are what
   a build without a project file runs on, so the option always holds. *)
let default_settings =
  {
    arch = Option.get (Jide_jasmin.Adapter.arch_of_string "x86-64");
    call_conv = Option.get (Jide_jasmin.Adapter.call_conv_of_string "linux");
    idirs = [];
  }

let adapter_check ~arch ~call_conv ~file ~text ~idirs =
  Jide_jasmin.Adapter.check arch call_conv ~file ~text ~idirs

let create ?(check = adapter_check) ?(context = Context.of_file) () =
  {
    documents = Hashtbl.create 16;
    analyses = Hashtbl.create 16;
    root = None;
    settings = default_settings;
    project = None;
    context_entry = None;
    entries = None;
    check;
    context_stage = context;
    analysis_count = 0;
  }

(* The settings and the context both change what an analysis says, so an
   analysis made before them is not reused. The key a document is found by is
   its version and digest, which know nothing about either. *)
let forget_analyses t = Hashtbl.reset t.analyses
let settings t = t.settings

let set_settings t settings =
  if t.settings <> settings then (
    t.settings <- settings;
    forget_analyses t)

let project t = t.project
let context_entry t = t.context_entry

let set_context_entry t entry =
  if t.context_entry <> entry then (
    t.context_entry <- entry;
    forget_analyses t)

let project_diagnostics t =
  match t.project with
  | None -> []
  | Some loaded ->
      let uri =
        match loaded.Project.path with
        | Some path -> Jide_lang.Uri.to_string (Jide_lang.Uri.of_path path)
        | None -> Project.file_name
      in
      Project.diagnostics ~uri loaded

(* The project file fills what it sets and leaves the defaults in place. A
   value the adapter does not know never becomes a setting: [Project] stores
   the validated ones only and reports the rest. *)
let settings_of_project (loaded : Project.loaded) =
  let project = loaded.Project.project in
  {
    arch = Option.value ~default:default_settings.arch project.Project.arch;
    call_conv =
      Option.value ~default:default_settings.call_conv project.Project.call_conv;
    idirs = project.Project.idirs;
  }

let set_root t root =
  t.root <- root;
  t.entries <- None;
  forget_analyses t;
  match root with
  | None ->
      t.project <- None;
      t.settings <- default_settings
  | Some directory ->
      let loaded = Project.load (Filename.concat directory Project.file_name) in
      t.project <- Some loaded;
      t.settings <- settings_of_project loaded

let root t = t.root

(* The key a document is found by: the identifier through [Jide_lang.Uri], so
   that two spellings of one document are one document. *)
let key_of uri = Jide_lang.Uri.to_string (Jide_lang.Uri.of_string uri)

let open_document t ~uri ~version ~text =
  Hashtbl.replace t.documents (key_of uri) (Document.create ~uri ~version ~text)

let change_document t ~uri ~version ~text =
  match Hashtbl.find_opt t.documents (key_of uri) with
  | Some document ->
      Hashtbl.replace t.documents (key_of uri)
        (Document.with_text document ~version text)
  | None -> ()

let close_document t ~uri = Hashtbl.remove t.documents (key_of uri)

let document t ~uri =
  match Hashtbl.find_opt t.documents (key_of uri) with
  | Some document -> Some document
  | None -> None

let documents t =
  Hashtbl.fold (fun _ document acc -> document :: acc) t.documents []
  |> List.sort (fun a b -> String.compare (Document.uri a) (Document.uri b))

let analysis_count t = t.analysis_count
let analysis t ~uri = Hashtbl.find_opt t.analyses (key_of uri)

let context t ~uri =
  Option.map (fun (analysis : analysis) -> analysis.context) (analysis t ~uri)

(* An analysis can be reused while this key is unchanged. The digest is the
   hexadecimal MD5 from Stdlib.Digest: stable across runs and platforms, no
   extra library, and only has to detect a change in the bytes. *)
let digest text = Digest.to_hex (Digest.string text)

let analysis_key t ~uri =
  match Hashtbl.find_opt t.documents (key_of uri) with
  | None -> None
  | Some document ->
      Some (Document.version document, digest (Document.text document))

(* A document may start with a UTF-8 byte-order mark, which the lexer rejects.
   The three bytes are removed before the text is analysed and every span is
   shifted back by them, so a reader never sees the shift. *)
let strip_bom text =
  let length = String.length text in
  if
    length >= 3
    && Char.code text.[0] = 0xEF
    && Char.code text.[1] = 0xBB
    && Char.code text.[2] = 0xBF
  then (3, String.sub text 3 (length - 3))
  else (0, text)

let max_bytes = 8 * 1024 * 1024
let blank_span = Jide_lang.Span.make ~start_byte:0 ~end_byte:0

let internal_diagnostic message =
  Jide_lang.Diagnostic.make ~code:"internal"
    ~severity:Jide_lang.Diagnostic.Error blank_span message

let size_diagnostic =
  Jide_lang.Diagnostic.make ~code:"size" ~severity:Jide_lang.Diagnostic.Error
    blank_span
    (Printf.sprintf
       "the document is larger than %d bytes, so it is not analysed" max_bytes)

let shift_span ~bom (span : Jide_lang.Span.t) =
  if bom = 0 then span else Jide_lang.Span.shift span bom

(* A warning carries a line number starting at 1 and byte columns counted
   from the start of that line, as the compiler prints them. The line index
   is built from the analysed text, and the byte-order mark, if any, is
   removed from the front of it, so every offset moves by the bytes removed. *)
let warning_span ~(text : string) ~bom (warning : Jide_jasmin.Warnings.t) =
  if warning.Jide_jasmin.Warnings.line <= 0 then blank_span
  else
    let index = Jide_lang.Line_index.create text in
    let line = warning.Jide_jasmin.Warnings.line - 1 in
    let start = Jide_lang.Line_index.line_start index line in
    let limit = Jide_lang.Line_index.line_end index line - start in
    let clamp column = start + min (max column 0) limit in
    Jide_lang.Span.shift
      (Jide_lang.Span.make
         ~start_byte:(clamp warning.Jide_jasmin.Warnings.start_column)
         ~end_byte:(clamp warning.Jide_jasmin.Warnings.end_column))
      bom

let diagnostic_of_warning ~text ~bom (warning : Jide_jasmin.Warnings.t) =
  Jide_lang.Diagnostic.make ~code:"warning"
    ~severity:Jide_lang.Diagnostic.Warning
    (warning_span ~text ~bom warning)
    warning.Jide_jasmin.Warnings.message

let rank (diagnostic : Jide_lang.Diagnostic.t) =
  match diagnostic.Jide_lang.Diagnostic.severity with
  | Jide_lang.Diagnostic.Error -> 0
  | _ -> 1

let compare_diagnostics a b =
  match Int.compare (rank a) (rank b) with
  | 0 -> Jide_lang.Diagnostic.compare a b
  | order -> order

let shift_diagnostic ~bom (diagnostic : Jide_lang.Diagnostic.t) =
  if bom = 0 then diagnostic
  else
    {
      diagnostic with
      Jide_lang.Diagnostic.span =
        shift_span ~bom diagnostic.Jide_lang.Diagnostic.span;
    }

let diagnostics_of_check ?(bom = 0) ~file ~text
    (result : Jide_jasmin.Adapter.check_result) =
  let warnings =
    List.filter
      (fun (warning : Jide_jasmin.Warnings.t) ->
        String.equal warning.Jide_jasmin.Warnings.file file)
      result.Jide_jasmin.Adapter.warnings
  in
  let diagnostics =
    List.map (shift_diagnostic ~bom) result.Jide_jasmin.Adapter.diagnostics
    @ List.map (diagnostic_of_warning ~text ~bom) warnings
  in
  List.stable_sort compare_diagnostics diagnostics

(* The entry set the context rule uses: the client's own choice when it made
   one, the project's list otherwise. *)
let entries t =
  match t.context_entry with
  | Some entry -> [ entry ]
  | None -> (
      match t.project with
      | Some loaded when loaded.Project.project.Project.entries <> [] ->
          loaded.Project.project.Project.entries
      | _ -> (
          match t.root with
          | None -> []
          | Some directory -> (
              match t.entries with
              | Some entries -> entries
              | None ->
                  let entries = Context.entries_of_directory directory in
                  t.entries <- Some entries;
                  entries)))

(* The text of a file: the editor's buffer when the file is open, the disk
   otherwise, which is what the compiler's own reader would find. *)
let read_file t path =
  let uri = Jide_lang.Uri.to_string (Jide_lang.Uri.of_path path) in
  match Hashtbl.find_opt t.documents (key_of uri) with
  | Some document -> Some (Document.text document)
  | None -> (
      match In_channel.with_open_bin path In_channel.input_all with
      | text -> Some text
      | exception Sys_error _ -> None)

(* The context a document is checked in, or the file alone when the stage that
   works it out fails: an analysis still has to have one. *)
let alone_context (document : Document.t) =
  match Document.path document with
  | Some path -> Context.alone path
  | None -> Context.alone (Document.uri document)

let context_of t (document : Document.t) =
  match Document.path document with
  | None -> alone_context document
  | Some path ->
      t.context_stage ~entries:(entries t) ~idirs:t.settings.idirs
        ~read:(read_file t) path

(* The whole analysis of one document. Nothing here raises: the check and the
   context stage both run under a catch-all, so an exception anywhere in the
   pipeline becomes one [internal] diagnostic. A document that is too large
   never reaches the check. *)
let analyse t ~uri (document : Document.t) key =
  let text = Document.text document in
  let bom, body = strip_bom text in
  let failed exn =
    {
      uri = Document.uri document;
      version = Document.version document;
      key;
      text = body;
      bom;
      context = alone_context document;
      diagnostics =
        [
          internal_diagnostic ("the analysis raised: " ^ Printexc.to_string exn);
        ];
    }
  in
  match
    let diagnostics =
      if String.length text > max_bytes then [ size_diagnostic ]
      else
        let file =
          match Document.path document with Some path -> path | None -> uri
        in
        match
          t.check ~arch:t.settings.arch ~call_conv:t.settings.call_conv ~file
            ~text:body ~idirs:t.settings.idirs
        with
        | result -> diagnostics_of_check ~bom ~file ~text:body result
        | exception exn ->
            [
              internal_diagnostic
                ("the analysis raised: " ^ Printexc.to_string exn);
            ]
    in
    let context = context_of t document in
    {
      uri = Document.uri document;
      version = Document.version document;
      key;
      text = body;
      bom;
      context;
      diagnostics;
    }
  with
  | analysis -> analysis
  | exception exn -> failed exn

let check t ~uri =
  match Hashtbl.find_opt t.documents (key_of uri) with
  | None -> None
  | Some document -> (
      let key = (Document.version document, digest (Document.text document)) in
      match Hashtbl.find_opt t.analyses (key_of uri) with
      | Some stored when stored.key = key -> Some stored
      | _ ->
          let analysis = analyse t ~uri document key in
          Hashtbl.replace t.analyses (key_of uri) analysis;
          t.analysis_count <- t.analysis_count + 1;
          Some analysis)
