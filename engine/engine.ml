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

(* What one analysis built from the text it read: the index over the untyped
   tree, the resolver over that index and the files it requires, and the typed
   facts the check left. These are the three a completion or a signature help
   reads, so they are kept beside the diagnostics rather than rebuilt when a
   request arrives. *)
type snapshot = {
  index : Index.t;
  resolve : Resolve.t;
  facts : Jide_jasmin.Facts.t;
}

type analysis = {
  uri : string;
  version : int;
  key : int * string;
  text : string;
  bom : int;
  snapshot : snapshot;
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

(* Lexical normalisation of an absolute path: empty components and "." fall
   away, and ".." removes the component before it. At the root ".." has
   nowhere to go, so it is dropped. Symbolic links are not followed here, so
   a path that does not exist yet still normalises; [resolve] follows them
   separately for the part of the path that exists. *)
let normalize_absolute path =
  let rec collect components kept =
    match components with
    | [] -> List.rev kept
    | ("" | ".") :: rest -> collect rest kept
    | ".." :: rest -> (
        match kept with
        | [] -> collect rest kept
        | _ :: kept -> collect rest kept)
    | component :: rest -> collect rest (component :: kept)
  in
  "/" ^ String.concat "/" (collect (String.split_on_char '/' path) [])

let is_within ~root path =
  let prefix = if String.equal root "/" then "/" else root ^ "/" in
  String.equal path root
  || String.length path >= String.length prefix
     && String.equal (String.sub path 0 (String.length prefix)) prefix

(* [path] with the part that exists resolved through the filesystem and the
   part that does not exist appended as text. A symbolic link in the existing
   part is therefore followed, and a path that does not exist yet still has a
   canonical form. A link whose target does not exist is treated as part of
   the text, because there is nothing to follow. *)
let canonical_absolute path =
  let rec split_existing suffix current =
    if Sys.file_exists current then (current, suffix)
    else
      let parent = Filename.dirname current in
      if String.equal parent current then (current, suffix)
      else split_existing (Filename.basename current :: suffix) parent
  in
  let existing, suffix = split_existing [] path in
  let base = try Unix.realpath existing with Unix.Unix_error _ -> existing in
  List.fold_left Filename.concat base suffix

(* [path] is relative to the workspace root when it is relative, and is
   accepted as it is when it is absolute and inside the root. The check is
   lexical first: a path is refused when its "." and ".." components,
   resolved in the text, would leave the root. It then repeats the check on
   the real path of the part of the path that exists, so a symbolic link
   that leaves the root is refused. Nothing is read, so a path that does not
   exist yet still resolves. *)
let resolve t path =
  match root t with
  | None -> Error "no workspace root is set"
  | Some root when Filename.is_relative root ->
      Error (Printf.sprintf "the workspace root is not absolute: %s" root)
  | Some root ->
      let root = normalize_absolute root in
      let absolute =
        if Filename.is_relative path then Filename.concat root path else path
      in
      let absolute = normalize_absolute absolute in
      if
        is_within ~root absolute
        && is_within ~root:(canonical_absolute root)
             (canonical_absolute absolute)
      then Ok absolute
      else Error (Printf.sprintf "%s is outside the workspace root" path)

(* The key a document is found by. [Jide_lang.Uri] decides identity: the
   scheme is compared without case and the path is decoded, so the canonical
   text gives one key to [file:///tmp/a%20b.jazz] and [file:///tmp/a b.jazz].
   A [file] URI is rebuilt from the path it names, so an empty authority and
   [localhost] meet too, as they already do where the file is read. *)
let key_of uri =
  let parsed = Jide_lang.Uri.of_string uri in
  match Jide_lang.Uri.path parsed with
  | Some path -> Jide_lang.Uri.to_string (Jide_lang.Uri.of_path path)
  | None -> Jide_lang.Uri.to_string parsed

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

(* The version and the text a URI names, taken from the open buffer when
   there is one and from disk otherwise. Reading is on demand: a file may
   change between two calls, and nothing here caches the old bytes. *)
let current t ~uri =
  match Hashtbl.find_opt t.documents (key_of uri) with
  | Some document -> Some (Document.version document, Document.text document)
  | None -> (
      match Jide_lang.Uri.path (Jide_lang.Uri.of_string uri) with
      | None -> None
      | Some path -> (
          match Document.read_file path with
          | Ok text -> Some (0, text)
          | Error _ -> None))

let text t ~uri = Option.map snd (current t ~uri)

(* A file's version is the constant 0, so its status is enough: there is no
   need to read the bytes to answer this request. *)
let version t ~uri =
  match Hashtbl.find_opt t.documents (key_of uri) with
  | Some document -> Some (Document.version document)
  | None -> (
      match Jide_lang.Uri.path (Jide_lang.Uri.of_string uri) with
      | None -> None
      | Some path -> if Document.readable path then Some 0 else None)

let analysis_count t = t.analysis_count
let analysis t ~uri = Hashtbl.find_opt t.analyses (key_of uri)

(* What the last check of [uri] built, and nothing else: no digest, no check,
   no file read. The stored analysis is the one the check left, whether or not
   the document's text has changed since, which is the contract a caller that
   must not block on a check relies on. [None] means no analysis is stored for
   the uri, so the document was never checked (or what it was is gone). *)
let snapshot t ~uri =
  Option.map
    (fun (analysis : analysis) -> analysis.snapshot)
    (Hashtbl.find_opt t.analyses (key_of uri))

let context t ~uri =
  Option.map (fun (analysis : analysis) -> analysis.context) (analysis t ~uri)

(* An analysis can be reused while this key is unchanged. The digest is the
   hexadecimal MD5 from Stdlib.Digest: stable across runs and platforms, no
   extra library, and only has to detect a change in the bytes. *)
let digest text = Digest.to_hex (Digest.string text)

(* The key is the text's, whether the text is an open buffer's or a file's:
   [text] is what an analysis reads, so the key is [None] exactly when the
   analysis would have nothing to read. *)
let analysis_key t ~uri =
  match current t ~uri with
  | None -> None
  | Some (version, text) -> Some (version, digest text)

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
   otherwise, which is what the compiler's own reader would find. The disk
   read is [Document.read_file]'s, so it is bounded and refuses anything that
   is not a regular file: a [require] of a device or a FIFO cannot make the
   analysis wait on it. *)
let read_file t path =
  let uri = Jide_lang.Uri.to_string (Jide_lang.Uri.of_path path) in
  match Hashtbl.find_opt t.documents (key_of uri) with
  | Some document -> Some (Document.text document)
  | None -> (
      match Document.read_file path with
      | Ok text -> Some text
      | Error _ -> None)

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

(* The snapshot of a text that yielded nothing to index. It stands for
   "analysed, and nothing came of it", which is what a document over
   [max_bytes] and a pipeline that raised both leave; [None] from {!snapshot}
   would say "never analysed" instead, which is a different thing.

   It is built once, at load time, from the empty text: [Resolve.of_text] on
   [""] is the one construction here that cannot depend on the document, and
   doing it eagerly keeps every path below total, because a fallback that had
   to build its own fallback could raise where the interface promises it
   cannot. The file name of the empty index is the document's, so a reader
   comparing [Index.file] sees the name it asked about. *)
let blank_snapshot =
  let file = "-" in
  let resolve = Resolve.of_text ~idirs:[] ~read:(fun _ -> None) ~file "" in
  let index =
    match Resolve.file resolve file with
    | Some index -> index
    | None -> fst (Index.of_text ~file "")
  in
  { index; resolve; facts = Jide_jasmin.Facts.empty }

let blank_snapshot_for ~(file : string) =
  { blank_snapshot with index = { blank_snapshot.index with Index.file } }

(* The index and the resolver of one analysed text, and the facts its check
   left. The index is the resolver's own for the document it resolved, so the
   document is parsed once for the two and not twice. The text is the
   document's, byte-order mark included: [Index.of_text] blanks the mark
   before the parser and keeps every range in the caller's own coordinates, so
   a byte that came from the line index is used as it arrives.

   This is the only place the index and the resolver are built. A request
   reads them back through {!snapshot} and never causes them to be built
   again. The resolver reads the files a [require] names through the engine's
   reader, so an open buffer comes before the disk, as it does for a check. *)
let snapshot_of t ~(file : string) ~(text : string)
    ~(facts : Jide_jasmin.Facts.t) =
  let build () =
    let resolve =
      Resolve.of_text ~idirs:t.settings.idirs ~read:(read_file t) ~file text
    in
    let index =
      match Resolve.file resolve file with
      | Some index -> index
      | None -> fst (Index.of_text ~file text)
    in
    { index; resolve; facts }
  in
  match build () with
  | snapshot -> snapshot
  | exception _ -> blank_snapshot_for ~file

(* The whole analysis of one document. Nothing here raises: the check, the
   index and the resolver, and the context stage all run under a catch-all, so
   an exception anywhere in the pipeline becomes one [internal] diagnostic. A
   document that is too large never reaches the check. *)
let analyse t ~uri (document : Document.t) key =
  let text = Document.text document in
  let bom, body = strip_bom text in
  let file =
    match Document.path document with Some path -> path | None -> uri
  in
  let failed exn =
    {
      uri = Document.uri document;
      version = Document.version document;
      key;
      text = body;
      bom;
      snapshot = blank_snapshot_for ~file;
      context = alone_context document;
      diagnostics =
        [
          internal_diagnostic ("the analysis raised: " ^ Printexc.to_string exn);
        ];
    }
  in
  match
    let diagnostics, facts =
      if String.length text > max_bytes then
        ([ size_diagnostic ], Jide_jasmin.Facts.empty)
      else
        match
          t.check ~arch:t.settings.arch ~call_conv:t.settings.call_conv ~file
            ~text:body ~idirs:t.settings.idirs
        with
        | result ->
            ( diagnostics_of_check ~bom ~file ~text:body result,
              result.Jide_jasmin.Adapter.facts )
        | exception exn ->
            ( [
                internal_diagnostic
                  ("the analysis raised: " ^ Printexc.to_string exn);
              ],
              Jide_jasmin.Facts.empty )
    in
    let snapshot = snapshot_of t ~file ~text ~facts in
    let context = context_of t document in
    {
      uri = Document.uri document;
      version = Document.version document;
      key;
      text = body;
      bom;
      snapshot;
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
