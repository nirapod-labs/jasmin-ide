(* The engine: open documents, and the analysis of each one.

   The project file is found by walking up from the directory of the file
   being analysed, so two documents in one workspace may be under two
   different projects, and a document under neither is analysed with the
   defaults. *)

type settings = {
  arch : Jide_jasmin.Adapter.arch;
  call_conv : Jide_jasmin.Adapter.call_conv;
  idirs : (string * string) list;
}

type checker =
  context:string list ->
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
   request arrives. The typed facts live here and nowhere else: the check puts
   them in this one value, and the navigation answers read them from it. *)
type snapshot = {
  index : Index.t;
  resolve : Resolve.t;
  facts : Jide_jasmin.Facts.t;
  arch : Jide_jasmin.Adapter.arch;
}

type required = {
  uri : string;
  text : string;
  diagnostics : Jide_lang.Diagnostic.t list;
}

type analysis = {
  uri : string;
  version : int;
  key : int * string;
  text : string;
  bom : int;
  snapshot : snapshot;
      (* The compiler's signature, kind and annotations are its [facts], so the
         navigation answers read [snapshot.facts] and there is one copy of them
         to keep in step with the text. *)
  context : Context.t;
  diagnostics : Jide_lang.Diagnostic.t list;
  required : required list;
}

type t = {
  documents : (string, Document.t) Hashtbl.t;
  analyses : (string, analysis) Hashtbl.t;
  resolutions : (string, Resolve.t) Hashtbl.t;
      (* The resolution of an entry file, found by that file's normalised
         path. A resolution is what the navigation answers read, and building
         one parses the entry and every file it requires, so it is kept until
         something it was built from changes: a document opening, changing or
         closing, the settings, the root or the context entry. A [Resolve.t]
         is kept rather than a [Locate.t] because [Locate] reads [Engine] and
         not the other way round; wrapping one costs a table of the typed
         facts, and no parse. *)
  parsed : Resolve.cache;
      (* Every file a resolution parsed, by path: a file whose bytes have not
         changed is not parsed again by the next resolution that reads it,
         whichever entry it is read from. *)
  mutable root : string option;
  mutable base : settings;
  mutable project : Project.loaded option;
  projects : (string, Project.loaded) Hashtbl.t;
  mutable context_entry : string option;
  discovered : (string, string list) Hashtbl.t;
      (* The entries found under a directory the search is bounded by, keyed
         by that directory. [set_root] drops them with the root. *)
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

let adapter_check ~context ~arch ~call_conv ~file ~text ~idirs =
  Jide_jasmin.Adapter.check ~context arch call_conv ~file ~text ~idirs

let create ?(check = adapter_check) ?(context = Context.of_file) () =
  {
    documents = Hashtbl.create 16;
    analyses = Hashtbl.create 16;
    resolutions = Hashtbl.create 16;
    parsed = Resolve.cache ();
    root = None;
    base = default_settings;
    project = None;
    projects = Hashtbl.create 16;
    context_entry = None;
    discovered = Hashtbl.create 4;
    check;
    context_stage = context;
    analysis_count = 0;
  }

(* The settings and the context both change what an analysis says, so an
   analysis made before them is not reused. The key a document is found by is
   its version and digest, which know nothing about either.

   A resolution is built from the same two, and from the text of every file
   it reads, so it is dropped with them. *)
let forget_resolutions t = Hashtbl.reset t.resolutions
let forget_analyses t = Hashtbl.reset t.analyses

(* The project file fills what it sets and leaves the base in place. A value
   the adapter does not know never becomes a setting: [Project] stores the
   validated ones only and reports the rest. A file that names no include path
   keeps the base's list, so a setting that names one is not lost when a
   project file happens to name none. *)
let settings_of_project ~(base : settings) (loaded : Project.loaded) =
  let project = loaded.Project.project in
  {
    arch = Option.value ~default:base.arch project.Project.arch;
    call_conv = Option.value ~default:base.call_conv project.Project.call_conv;
    idirs = (if project.Project.idirs = [] then base.idirs else project.idirs);
  }

(* The project a directory is under: the first [jasmin.project] at it or above
   it, read once per directory, because a check that walked the filesystem for
   every document would ask the same questions again for every document of one
   directory. *)
let project_above t directory =
  match Hashtbl.find_opt t.projects directory with
  | Some loaded -> loaded
  | None ->
      let loaded = Project.load_above directory in
      Hashtbl.replace t.projects directory loaded;
      loaded

(* The project a document is analysed under: the nearest file above the
   directory the document is in. A document with nothing above it is the
   ordinary case, and no diagnostic is made for it. *)
let project_of t (document : Document.t) =
  match Document.path document with
  | None -> Option.value ~default:Project.absent t.project
  | Some path -> project_above t (Filename.dirname path)

(* The settings one document is checked under: the base first, then the
   nearest project file above the document. The named include paths of the
   [JASMINPATH] environment variable come after the ones those give, as the
   compiler binds them after its [-I] options, so the check, the context walk
   and the resolver all read a [from NAME require] the same way. *)
let settings_for t (document : Document.t) =
  let settings = settings_of_project ~base:t.base (project_of t document) in
  { settings with idirs = Jide_jasmin.Adapter.with_jasmin_path settings.idirs }

(* The settings a document with the root's own project above it would get, so
   a caller that is not looking at one document may ask what the workspace is
   set up as. *)
let settings t =
  settings_of_project ~base:t.base
    (Option.value ~default:Project.absent t.project)

let set_settings t settings =
  if t.base <> settings then (
    t.base <- settings;
    forget_resolutions t;
    forget_analyses t)

let project t = t.project
let context_entry t = t.context_entry

let set_context_entry t entry =
  if t.context_entry <> entry then (
    t.context_entry <- entry;
    forget_resolutions t;
    forget_analyses t)

(* The project file a diagnostic belongs to, which is the file's own path when
   one was read and the bare name otherwise. *)
let uri_of_project (loaded : Project.loaded) =
  match loaded.Project.path with
  | Some path -> Jide_lang.Uri.to_string (Jide_lang.Uri.of_path path)
  | None -> Project.file_name

let project_diagnostics t =
  match t.project with
  | None -> []
  | Some loaded -> Project.diagnostics ~uri:(uri_of_project loaded) loaded

let set_root t root =
  t.root <- root;
  Hashtbl.reset t.discovered;
  Hashtbl.reset t.projects;
  forget_resolutions t;
  forget_analyses t;
  match root with
  | None -> t.project <- None
  | Some directory ->
      (* The root's own project, which is what [project] and
         [project_diagnostics] report. It is found by the same walk, so a root
         that holds no file but sits under one that does is under it. *)
      t.project <- Some (project_above t directory)

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

(* A resolution reads the text of every file a document requires, and its
   entry is read from that document's buffer, so a document opening, changing
   or closing can change what a resolution would say. The whole cache goes:
   an entry a change does not touch still required files that may have moved,
   and a resolution is cheap next to being wrong. *)
let open_document t ~uri ~version ~text =
  Hashtbl.reset t.resolutions;
  Hashtbl.replace t.documents (key_of uri) (Document.create ~uri ~version ~text)

let change_document t ~uri ~version ~text =
  match Hashtbl.find_opt t.documents (key_of uri) with
  | Some document ->
      Hashtbl.reset t.resolutions;
      Hashtbl.replace t.documents (key_of uri)
        (Document.with_text document ~version text)
  | None -> ()

let close_document t ~uri =
  Hashtbl.reset t.resolutions;
  Hashtbl.remove t.documents (key_of uri)

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

(* The uri a required file is published against. The check prints the path the
   [require] resolved to, which is absolute when the document's own path is;
   one printed relative to a relative document is resolved against that
   document, which is the directory the require read it in. An open document's
   own spelling wins over the path's, so a client that holds the file sees the
   diagnostics on the text it shows. *)
let required_uri t ~(document : Document.t) (file : string) =
  let path =
    if Filename.is_relative file then
      match Document.path document with
      | Some base when not (Filename.is_relative base) ->
          Filename.concat (Filename.dirname base) file
      | _ -> file
    else file
  in
  let uri =
    Jide_lang.Uri.to_string (Jide_lang.Uri.of_path (Index.normalise path))
  in
  match Hashtbl.find_opt t.documents (key_of uri) with
  | Some open_document -> Document.uri open_document
  | None -> uri

(* The files the check reached through a [require] that hold an error, each
   under the uri it is published by. The diagnostics are sorted as the
   document's are, so a caller publishes one file's set in source order. *)
let required_of_check t ~(document : Document.t)
    (result : Jide_jasmin.Adapter.check_result) =
  List.map
    (fun (entry : Jide_jasmin.Adapter.required) ->
      {
        uri = required_uri t ~document entry.Jide_jasmin.Adapter.file;
        text = entry.Jide_jasmin.Adapter.text;
        diagnostics =
          List.stable_sort compare_diagnostics
            entry.Jide_jasmin.Adapter.diagnostics;
      })
    result.Jide_jasmin.Adapter.required

(* The .jazz files under a directory the search is bounded by. They are found
   once per directory: the set changes only with the root, and [set_root]
   drops it. *)
let discovered_entries t directory =
  match Hashtbl.find_opt t.discovered directory with
  | Some entries -> entries
  | None ->
      let entries = Context.entries_of_directory directory in
      Hashtbl.replace t.discovered directory entries;
      entries

(* The entry set the context rule uses: the client's own choice when it made
   one, and the entry list of the project above the document otherwise. When
   that list is empty the entries are the .jazz files under the project root
   or, without a project file above the document, under the workspace root
   ([Context.search_root] says which, and never looks above either). A
   document under neither has no entries, so it is checked alone. The rule
   itself is [Context.of_file]'s; this only says where its entries come
   from. *)
let entries t (document : Document.t) =
  match t.context_entry with
  | Some entry -> [ entry ]
  | None -> (
      let loaded = project_of t document in
      match loaded.Project.project.Project.entries with
      | [] -> (
          match Document.path document with
          | None -> []
          | Some file -> (
              let project = Option.map Filename.dirname loaded.Project.path in
              match Context.search_root ?workspace:t.root ?project file with
              | None -> []
              | Some directory -> discovered_entries t directory))
      | entries -> entries)

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

(* The resolution of a document: the entry file and every file it requires,
   each parsed and walked, with what every name in them stands for. It is what
   the four navigation answers read, and building it parses the whole project
   the document touches, so it is kept until something it was built from
   changes: [open_document], [change_document], [close_document],
   [set_settings], [set_context_entry] and [set_root] all drop the cache.

   The entry is the document's own path, because a byte a caller hands in is a
   byte of that text. A document that is not a file has no path, and nothing
   to read a [require] relative to, so it has no resolution and the navigation
   answers about it are the empty answer. *)
let resolution t ~uri =
  match Hashtbl.find_opt t.documents (key_of uri) with
  | None -> None
  | Some document -> (
      match Document.path document with
      | None -> None
      | Some path -> (
          let file = Index.normalise path in
          match Hashtbl.find_opt t.resolutions file with
          | Some resolved -> Some resolved
          | None ->
              let resolved =
                Resolve.of_text ~cache:t.parsed
                  ~idirs:(settings_for t document).idirs ~read:(read_file t)
                  ~file (Document.text document)
              in
              Hashtbl.replace t.resolutions file resolved;
              Some resolved))

(* The resolution of a file read as an entry, from the editor's buffer when it
   is open and the disk otherwise, kept with the documents' own. *)
let file_resolution t ~idirs path =
  let file = Index.normalise path in
  match Hashtbl.find_opt t.resolutions file with
  | Some resolved -> Some resolved
  | None -> (
      match read_file t file with
      | None -> None
      | Some text ->
          let resolved =
            Resolve.of_text ~cache:t.parsed ~idirs ~read:(read_file t) ~file
              text
          in
          Hashtbl.replace t.resolutions file resolved;
          Some resolved)

let project_resolutions t ~uri =
  match Hashtbl.find_opt t.documents (key_of uri) with
  | None -> []
  | Some document ->
      let idirs = (settings_for t document).idirs in
      let own = Option.map Index.normalise (Document.path document) in
      List.filter_map
        (fun entry ->
          if Some (Index.normalise entry) = own then None
          else file_resolution t ~idirs entry)
        (entries t document)

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
      t.context_stage ~entries:(entries t document)
        ~idirs:(settings_for t document).idirs ~read:(read_file t) path

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
  {
    index;
    resolve;
    facts = Jide_jasmin.Facts.empty;
    arch = Jide_jasmin.Arch.X86_64;
  }

let blank_snapshot_for ~(file : string) ~(arch : Jide_jasmin.Adapter.arch) =
  { blank_snapshot with index = { blank_snapshot.index with Index.file }; arch }

(* The index and the resolver of one analysed text, and the facts its check
   left. The index is the resolver's own for the document it resolved, so the
   document is parsed once for the two and not twice. The text is the
   document's, byte-order mark included: [Index.of_text] blanks the mark
   before the parser and keeps every range in the caller's own coordinates, so
   a byte that came from the line index is used as it arrives.

   This is the only place the index and the resolver are built. A request
   reads them back through {!snapshot} and never causes them to be built
   again. The resolver reads the files a [require] names through the engine's
   reader, so an open buffer comes before the disk, as it does for a check,
   and it resolves under the include paths the document's own project gives.

   A resolver that raises leaves the empty index of {!blank_snapshot_for}, and
   the facts go in all the same: they came from the check, which is a different
   stage, and the value holds one copy of them either way. *)
let snapshot_of t ~idirs ~(file : string) ~(text : string)
    ~(facts : Jide_jasmin.Facts.t) ~(arch : Jide_jasmin.Adapter.arch) =
  let build () =
    let resolve =
      Resolve.of_text ~cache:t.parsed ~idirs ~read:(read_file t) ~file text
    in
    let index =
      match Resolve.file resolve file with
      | Some index -> index
      | None -> fst (Index.of_text ~file text)
    in
    { index; resolve; facts; arch }
  in
  match build () with
  | snapshot -> snapshot
  | exception _ -> { (blank_snapshot_for ~file ~arch) with facts }

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
  let settings = settings_for t document in
  let failed exn =
    {
      uri = Document.uri document;
      version = Document.version document;
      key;
      text = body;
      bom;
      snapshot = blank_snapshot_for ~file ~arch:settings.arch;
      context = alone_context document;
      diagnostics =
        [
          internal_diagnostic ("the analysis raised: " ^ Printexc.to_string exn);
        ];
      required = [];
    }
  in
  match
    (* The context comes first: a file an entry requires is checked in the
       context of that entry, so the check that follows is run in it. The
       files before the document are the chain without its last element,
       which is the document itself. *)
    let context = context_of t document in
    let in_context =
      match List.rev (Context.chain context) with
      | _document :: files -> List.rev files
      | [] -> []
    in
    let diagnostics, facts, required =
      if String.length text > max_bytes then
        ([ size_diagnostic ], Jide_jasmin.Facts.empty, [])
      else
        match
          t.check ~context:in_context ~arch:settings.arch
            ~call_conv:settings.call_conv ~file ~text:body ~idirs:settings.idirs
        with
        | result ->
            ( diagnostics_of_check ~bom ~file ~text:body result,
              result.Jide_jasmin.Adapter.facts,
              required_of_check t ~document result )
        | exception exn ->
            ( [
                internal_diagnostic
                  ("the analysis raised: " ^ Printexc.to_string exn);
              ],
              Jide_jasmin.Facts.empty,
              [] )
    in
    let snapshot =
      snapshot_of t ~idirs:settings.idirs ~file ~text ~facts ~arch:settings.arch
    in
    {
      uri = Document.uri document;
      version = Document.version document;
      key;
      text = body;
      bom;
      snapshot;
      context;
      diagnostics;
      required;
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
