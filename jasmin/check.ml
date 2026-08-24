open Jasmin

type required = {
  file : string;
  text : string;
  diagnostics : Jide_lang.Diagnostic.t list;
  site : int option;
}
(** The diagnostics of one file reached through a [require]. Every span in
    [diagnostics] indexes [text], the bytes of [file], and not the document
    the caller asked about: a caller that prints them names [file] with them,
    which is the place the compiler itself reports them at. [file] is the path
    the compiler prints, not the normalised one the loop compares with.
    [site] is the byte of the document's item whose typing first reached the
    file, [None] for a file of the context chain. *)

type result = {
  diagnostics : Jide_lang.Diagnostic.t list;
  warnings : Warnings.t list;
  facts : Facts.t;
  required : required list;
      (** The files reached through a [require] that hold an error, in the
          order their first error was found, each with the text its spans
          index. A file that typed without error is not here. *)
}

let empty =
  { diagnostics = []; warnings = []; facts = Facts.empty; required = [] }

(** The most diagnostics one check returns, syntax and typing together. *)
let max_diagnostics = 200

(** The first [count] entries of a list. *)
let rec take count = function
  | _ when count <= 0 -> []
  | [] -> []
  | entry :: rest -> entry :: take (count - 1) rest

(** The kind of name an item declares, as the compiler's own messages spell
    it. An item that did not reach the environment is remembered by its kind,
    because the message a later reference to it produces depends on that
    kind: a function is an unknown function, a parameter or a global is an
    unknown variable, and a type alias is a type the compiler cannot find. *)
type declaration = Function of string | Variable of string | Alias of string

let name_of_declaration = function
  | Function name | Variable name | Alias name -> name

(** [qualified prefix name] is the name the compiler prints for [name]
    declared inside the namespace [prefix]. *)
let qualified prefix name =
  if String.equal prefix "" then name else prefix ^ "::" ^ name

(** The same name, for a declaration. *)
let qualify_declaration prefix = function
  | Function name -> Function (qualified prefix name)
  | Variable name -> Variable (qualified prefix name)
  | Alias name -> Alias (qualified prefix name)

(** [between ~prefix ~suffix message] is the part of [message] between the
    two, when it has both. *)
let between ~prefix ~suffix message =
  let prefix_length = String.length prefix
  and suffix_length = String.length suffix
  and length = String.length message in
  if
    length >= prefix_length + suffix_length
    && String.sub message 0 prefix_length = prefix
    && String.sub message (length - suffix_length) suffix_length = suffix
  then
    Some
      (String.sub message prefix_length
         (length - prefix_length - suffix_length))
  else None

(** The declaration an unresolved-name message is about, when it is one of
    the three messages the compiler prints for a name it cannot resolve and
    nothing else. The declaration, not the rendered text, is the key a
    suppression is matched with: a message that merely reads like one of
    these is not a name the compiler could not find. *)
let declaration_of_message (message : string) =
  match between ~prefix:"unknown function: `" ~suffix:"'" message with
  | Some name -> Some (Function name)
  | None -> (
      match between ~prefix:"unknown variable: `" ~suffix:"'" message with
      | Some name -> Some (Variable name)
      | None -> (
          match between ~prefix:"Type '" ~suffix:"' not found" message with
          | Some name -> Some (Alias name)
          | None -> None))

(** A name this file can no longer resolve: a declaration an item failed to
    make, every name under a namespace whose contents recovery dropped, or
    every name at all when the file a [require] read did not parse, because
    then the tree does not say which names it declared. *)
type suppression = Name of declaration | Namespace of string | Any_name

(** Whether [suppression] covers [declaration]. A namespace covers every name
    the compiler would print qualified with it, which is how a reference to
    an item of a dropped namespace reads; [Any_name] covers everything. *)
let covers (suppression : suppression) (declaration : declaration) =
  match suppression with
  | Name expected -> expected = declaration
  | Any_name -> true
  | Namespace prefix ->
      let name = name_of_declaration declaration in
      let length = String.length prefix in
      String.length name > length + 2
      && String.sub name 0 length = prefix
      && String.sub name length 2 = "::"

(** The same suppression, for a name declared inside the namespace [prefix].
    [Any_name] is not a name the namespace changes: it covers every name
    already. *)
let qualify_suppression prefix = function
  | Name declaration -> Name (qualify_declaration prefix declaration)
  | Namespace name -> Namespace (qualified prefix name)
  | Any_name -> Any_name

(** Whether a diagnostic with this message is the same failure seen again. *)
let suppressed (suppressions : suppression list) message =
  match declaration_of_message message with
  | None -> false
  | Some declaration -> List.exists (fun s -> covers s declaration) suppressions

(** Every declaration an item makes, with the namespace path in front of the
    names a namespace contains. A namespace is not a declaration of its own:
    a reference to one of its items is printed as [`N::name'], so the names
    inside it are what a failure to type it takes away. *)
let rec declarations_of_item ~(prefix : string)
    (item : Syntax.pitem Location.located) : declaration list =
  let qualify name = qualified prefix name in
  match Location.unloc item with
  | Syntax.PFundef definition ->
      [ Function (qualify (Location.unloc definition.pdf_name)) ]
  | Syntax.PParam parameter ->
      [ Variable (qualify (Location.unloc parameter.ppa_name)) ]
  | Syntax.PGlobal global ->
      [ Variable (qualify (Location.unloc global.pgd_name)) ]
  | Syntax.PTypeAlias (name, _, _) -> [ Alias (qualify (Location.unloc name)) ]
  | Syntax.PNamespace (name, items) ->
      let prefix = qualify (Location.unloc name) in
      List.concat_map (declarations_of_item ~prefix) items
  | Syntax.Pexec _ | Syntax.Prequire _ -> []

let declarations_of_item item = declarations_of_item ~prefix:"" item

(** What a stub stands for. A dropped namespace takes every name under it
    with it, which is why it is remembered by prefix rather than by name. *)
let suppression_of_dropped stub =
  let qualify =
    match stub with
    | Recover.Dropped { namespace = Some prefix; _ } ->
        qualify_suppression prefix
    | Recover.Dropped { namespace = None; _ } -> Fun.id
  in
  Option.map qualify
  @@
  match stub with
  | Recover.Dropped { kind = Recover.Function; name = Some name; _ } ->
      Some (Name (Function name))
  | Recover.Dropped { kind = Recover.Parameter; name = Some name; _ }
  | Recover.Dropped { kind = Recover.Global; name = Some name; _ } ->
      Some (Name (Variable name))
  | Recover.Dropped { kind = Recover.Type; name = Some name; _ } ->
      Some (Name (Alias name))
  | Recover.Dropped { kind = Recover.Namespace; name = Some name; _ } ->
      Some (Namespace name)
  | Recover.Dropped _ -> None

type lookup = string -> string option
(** [lookup path] is the bytes of a file a [require] names, when the caller
    has them: an open buffer first, a file on disk when the buffer is not
    the one asked for. *)

(** The default lookup: the file on disk. *)
let read_file (path : string) : string option =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> Some text
  | exception _ -> None

(** The directory a [require] item's [from] names, or the name itself when
    no [-I NAME=PATH] bound one. [Ok None] is an item with no [from]: the
    file it names is read beside the file that writes the item.

    An [Error] is a name the compiler refuses at the name's own location:
    [Pretyping.Env.enter_file] raises [unknown name %s] for it rather than
    reading anything. There is no fallback to the writer's directory here,
    because a program that builds only because the file happens to sit
    beside the document is one the compiler rejects. *)
let include_directory ~(idirs : (string * string) list)
    (from : string Location.located option) :
    (string option, string Location.located) Stdlib.result =
  match from with
  | None -> Ok None
  | Some name -> (
      match List.assoc_opt (Location.unloc name) idirs with
      | Some directory -> Ok (Some directory)
      | None -> Error name)

(** A name is a file name as the compiler resolves it: relative to the file
    that requires it, or to the directory an idir binds when the item names
    a [from]. [from] is already resolved by {!include_directory}, so it is
    either no directory at all or the one an idir bound. *)
let include_path ~(base : string) ~(from : string option) (name : string) :
    string =
  let base = match from with None -> base | Some directory -> directory in
  if Filename.is_relative name then Filename.concat base name else name

(** [lexical path] is one spelling for one path: no [.] and no [..] left in
    it, absolute when [path] was and relative when it was. The fold is
    lexical, like the engine's own resolution: nothing here asks the
    filesystem, so a symbolic link is not resolved and two paths that name one
    file through different links stay different. *)
let lexical (path : string) : string =
  let absolute = String.length path > 0 && path.[0] = '/' in
  let parts = String.split_on_char '/' path in
  let rec fold stack = function
    | [] -> List.rev stack
    | ("" | ".") :: rest -> fold stack rest
    | ".." :: rest -> (
        match stack with [] -> fold stack rest | _ :: stack -> fold stack rest)
    | part :: rest -> fold (part :: stack) rest
  in
  let body = String.concat "/" (fold [] parts) in
  if absolute then "/" ^ body else if body = "" then "." else body

(** [normalise_path path] is one spelling for one file: an absolute path with
    no [.] and no [..] left in it, a relative [path] taken from the process's
    directory. It is what makes a cycle compare equal to itself however the
    require spelled the file, and what the [loaded] set is keyed by. *)
let normalise_path (path : string) : string =
  if Filename.is_relative path then
    lexical (Filename.concat (Sys.getcwd ()) path)
  else lexical path

(** How many required files one document may nest. A cycle is caught by the
    chain before this, and this bounds a tree of distinct files. *)
let max_include_depth = 8

(* The bytes of one location, so a diagnostic can be placed on the text it
   is about. *)
let span_of_location (location : Location.t) : Jide_lang.Span.t =
  Jide_lang.Span.make
    ~start_byte:(max 0 location.Location.loc_bchar)
    ~end_byte:(max 0 location.Location.loc_echar)

(* The bytes of one item, so the loop can ask whether recovery rewrote any
   of them. *)
let span_of_item (item : Syntax.pitem Location.located) : Jide_lang.Span.t =
  span_of_location item.Location.pl_loc

(** [within ~length span] is [span] cut to the bytes of a document [length]
    long. Nothing is returned at an offset past the document's end: such a
    span indexes some other file, and a caller that places it would either
    reject it or highlight none of the document. *)
let within ~(length : int) (span : Jide_lang.Span.t) : Jide_lang.Span.t =
  let start_byte = max 0 (min length span.Jide_lang.Span.start_byte) in
  let end_byte = max start_byte (min length span.Jide_lang.Span.end_byte) in
  if
    start_byte = span.Jide_lang.Span.start_byte
    && end_byte = span.Jide_lang.Span.end_byte
  then span
  else Jide_lang.Span.make ~start_byte ~end_byte

module Make (A : Arch_full.Arch) = struct
  type environment = A.extended_op Pretyping.Env.env

  let type_item (environment : environment) item =
    match Pretyping.tt_item A.arch_info environment item with
    | environment -> Ok environment
    | exception exn -> Error exn

  (** [added ~before ~after] is the run of declarations [after] holds and
      [before] does not. Typing pushes every declaration at the front
      ([e_decls = P.MIfun v :: env.e_decls] in the compiler's
      [pretyping.ml]), so an item's own declarations are the head of [after]
      and the old ones are the tail that follows them. An item that declares
      nothing, an [exec] for example, leaves the list alone, and a [require]
      pushes a whole file's declarations, so the size of the run is read from
      the two lists and never assumed to be one.

      The run is kept only when the tail it leaves is [before] itself. When
      it is not, the declarations did not arrive by pushing and the whole of
      [after] is offered instead: the collector keeps only the declarations
      whose location lies inside the item, so the wider run is read through
      this item's own range and the item is not left without its
      declarations. *)
  let added ~(before : (unit, A.extended_op) Prog.pmod_item list)
      ~(after : (unit, A.extended_op) Prog.pmod_item list) :
      (unit, A.extended_op) Prog.pmod_item list =
    let grown = List.length after - List.length before in
    let rec split count prefix rest =
      if count <= 0 then (List.rev prefix, rest)
      else
        match rest with
        | [] -> (List.rev prefix, [])
        | declaration :: rest -> split (count - 1) (declaration :: prefix) rest
    in
    match split (Stdlib.max grown 0) [] after with
    | prefix, rest when rest == before -> prefix
    | _ -> after

  (** Entering and leaving a file are calls into the library like any other:
      the environment they build is threaded through the items, so a raise
      in either is turned into a diagnostic here rather than escaping. *)
  let enter_file (environment : environment) (file : string) =
    match Pretyping.Env.enter_file environment None None file with
    | entered -> Ok entered
    | exception exn -> Error exn

  let exit_file (environment : environment) =
    match Pretyping.Env.exit_file environment with
    | environment -> Ok environment
    | exception exn -> Error exn

  (** The compiler's own preprocessing, [Compile.preprocess], run over the
      declarations the environment holds once the document is typed: the
      program the compiler hands it. Parameter substitution, the
      copy-and-fix-length pass and [Typing.check_prog] are inside the
      compiler's preprocessing and reject programs that pretyping accepts, so
      the verdict up to the end of preprocessing needs them. The program is
      not used afterwards; only its failure is. *)
  let preprocess_error (environment : environment) : Error.t option =
    match
      Compile.preprocess A.pointer_data A.msf_size A.asmOp
        (Pretyping.Env.decls environment)
    with
    | _program -> None
    | exception exn -> Some (Error.of_exn exn)

  type loop = {
    file : string;
        (** The one spelling every comparison uses: [normalise_path] of the
            file, so a cycle and the [loaded] set compare equal however a
            require spelled it. *)
    name : string;
        (** The path the compiler prints for [file]: the document's own as the
            caller spelled it, and for a file reached through a [require] the
            path that require resolved to, relative when the document was.
            A diagnostic about the file is printed with it. *)
    text : string;
        (** The bytes of [file]. A diagnostic of a required file is printed
            against the text its span indexes, which is that file's own. *)
    document : string;
        (** The file the caller asked about, which stays the same however deep
            a [require] chain goes. *)
    length : int;
        (** The document's length in bytes. Nothing is placed past its end,
            because a span that runs past it indexes some other file. *)
    resolve : lookup;
    idirs : (string * string) list;
    place : bool;
        (** True while the loop types the document the caller asked about, and
            false for a file reached through a [require]. Only the document's
            own diagnostics have spans the caller can place; an error from an
            included file names that file, or names no file at all, and either
            way its offsets do not index the document. *)
    origin : Jide_lang.Span.t ref;
        (** The span of the document's item being typed. A failure of the
            require chain itself is reported there, because it is the
            document's item that cannot be read, not the included file's. *)
    environment : environment ref;
    suppressions : suppression list ref;
    diagnostics : Error.t list ref;
    facts : Facts.t ref;
        (** The typed facts of the document's own items, merged as each one
            types. Only the document's loop collects them: the facts of an
            included file carry that file's offsets, which do not index the
            document the caller asked about. *)
    required_diagnostics :
      (string * string * Jide_lang.Diagnostic.t * int option) list ref;
        (** The diagnostics of the files reached through a [require], newest
            first, each with the name that file is printed by, the text its
            span indexes, and the byte of the document's item whose typing
            reached the file, [None] for a file of the context chain, which
            the compiler enters before the document. They are reported against
            the file they belong to, never against the document. *)
    in_document : bool;
        (** False while the context chain is typed, and true for the document
            and every file a require of the document reaches. *)
    awaiting : string list;
        (** The rest of the context chain from the file this loop types: the
            file it has to reach, then the ones below it, the document last.
            Empty for the document and for a file a require pulled in on its
            own, which are typed whole. *)
    mutable reached : bool;
        (** True once the head of [awaiting] has been typed. The loop stops
            there: the file is checked in the environment the entry builds
            up to its [require], and not in the one the items after it would
            leave behind. *)
    kept : int ref;
    budget : int;
    stopped : bool ref;
    include_errors : int ref;
    loaded : (string * string) list ref;
        (** The files this check has read, as the namespace they were read
            in and the normalised path they were read under. It is the
            compiler's own record ([Pretyping.Env.e_loader.loaded], which
            [enter_file] consults before it reads anything): a [require] of a
            file its namespace has already loaded reads nothing and declares
            nothing a second time. Without it a file that requires one file
            under two spellings — `"file1.jazz"` and `"../common/file1.jazz"`
            — splices that file's items twice and its declarations collide
            with themselves, which the compiler never sees. The same record stops
            a diamond, a file reached through two chains of requires, which
            libjade's ML-KEM unit is full of, from being typed twice and every
            declaration it makes reported as already declared. *)
    sites : (string * Jide_lang.Span.t * string) list ref;
        (** For each file a require read: its normalised path, the span of the
            document's item that reached it, and the name the require wrote.
            A failure found after typing is placed on the item that read its
            file, the way a failure of typing is. *)
  }
  (** Everything one pass over one item stream learns. A required file is
      typed by the same pass with its own [file] and its own recovered
      regions, and shares the environment, the suppressions and the budget.
      The diagnostics are the document's own: an error attributed to another
      file is counted in [include_errors] and recorded in
      [required_diagnostics] instead of being placed. *)

  type spliced = {
    written : string;
        (** The name the [require] item wrote, which is what a diagnostic
            about the file calls it. *)
    path : string;
        (** The normalised path the lookup was asked for, which is the file
            an error inside the spliced items names. *)
    at : Jide_lang.Span.t;  (** The [require] item's span. *)
    span_file : string;
        (** The file {!at} indexes: the document itself for a require the
            document wrote, an included file for one written in a file a
            top-level require pulled in. *)
    reported : bool;
        (** True when the expansion already reported on this item, so a
            failure among the spliced items is not reported twice. *)
  }
  (** A [require] inside a namespace whose file was read and spliced where
      the item stood. A namespace is typed as one item, so the first error
      inside it ends the item; this is what lets the loop say which of the
      namespace's files the error came from. *)

  (** The span of a diagnostic about an item written in [file]: the item's
      own span when the document wrote it, and otherwise the span of the
      document's item whose typing reached [file], which is [loop.origin],
      because an included file's offsets do not index the document. The
      answer is cut to the document's bytes. *)
  let span_of_writer loop ~(file : string) ~(at : Jide_lang.Span.t) =
    within ~length:loop.length
      (if loop.place && String.equal file loop.file then at else !(loop.origin))

  (** The diagnostic a [from] name no [-I NAME=PATH] bound is refused with:
      the compiler's own message, on the name's own bytes when the document
      wrote the [require] item and on the document's item otherwise. *)
  let unknown_name loop ~(file : string) (name : string Location.located) =
    Error.make ~file:(Some loop.document) ~kind:Error.Typing
      ~span:
        (span_of_writer loop ~file ~at:(span_of_location (Location.loc name)))
      (Printf.sprintf "unknown name %s" (Location.unloc name))

  (** A namespace whose required file was not read has no names the document
      can know: nothing says which of them are the namespace's own, so every
      name under it is treated as one the file would have declared. *)
  let forget_names (loop : loop) (prefix : string) =
    if not (String.equal prefix "") then
      loop.suppressions := Namespace prefix :: !(loop.suppressions)

  (** Whether [namespace] has already read [path]. It is the test the
      compiler's [enter_file] makes against [e_loader.loaded] before it reads
      anything, and its answer there is [None]: the file is not read and its
      items are not typed again. *)
  let already_loaded (loop : loop) ~(namespace : string) (path : string) =
    List.mem (namespace, path) !(loop.loaded)

  (** Record that [namespace] read [path], before the file's items are typed,
      so a second [require] of the same path in the same namespace is the
      compiler's skip whether the first read succeeded or not: a broken file
      is read once and reported once. *)
  let remember_loaded (loop : loop) ~(namespace : string) (path : string) =
    if not (already_loaded loop ~namespace path) then
      loop.loaded := (namespace, path) :: !(loop.loaded)

  (** [reachable_site ~floor site] is false for a required file's diagnostic
      the compiler could not report: when the document has a syntax error,
      which [floor] is the first byte of, the compiler stops at it before it
      types anything, so a file whose [require] item starts at or before that
      byte is never read. A file the context chain reaches ([None]) is entered
      before the document and is kept. *)
  let reachable_site ~(floor : int option) (site : int option) =
    match (floor, site) with Some floor, Some site -> site > floor | _ -> true

  (** [type_items ~file ~text ~items ~idirs ~context] types the items of one
      file one by one and returns the diagnostics, plus the environment the
      loop leaves behind when it started at all.

      Before each item the environment is kept, and a failed item leaves the
      environment as it was, so the items after it still type. The names of
      the items recovery dropped, and of the items whose typing failed, are
      remembered: a later reference to one of them is the same failure seen
      again and is not reported a second time. A diagnostic whose span lies
      in an item recovery rewrote is dropped for the same reason.

      The environment kept and restored here is the one the loop can observe:
      its maps and its declarations. [Pretyping.Env.env] also carries
      [e_declared], a mutable cell the library adds a function's locals to as
      it types them, and the type is abstract, so a failure cannot undo that
      cell. The library resets it when it enters the next function
      ([Env.Vars.clear_locals]), which is the only place the loop would read
      it, so the leak is not observable. Recorded rather than papered over.

      A [require] is read through {!resolve} and typed here, so a file the
      user has open is used as it is and a missing file or a cycle is a
      diagnostic on the require item rather than an exception from inside
      the compiler. An error in a required file is not placed on this
      document, whose bytes it does not describe: it is returned in
      {!result.required}, against the file its span indexes, which is where
      the compiler prints it.

      [context] is the chain of files the compiler would enter before this
      one, outermost first, when the file is not an entry: each is typed up
      to the [require] that names the next, and the document is then typed in
      the environment that leaves behind. The empty list is a file checked on
      its own.

      At most {!budget} diagnostics are kept. Every call into the library
      this function makes is wrapped, and {!check}, the only entry point the
      engine uses, ends in a catch-all: no exception leaves the adapter. *)
  let rec type_items ~(resolve : lookup) ~(budget : int)
      ~(dropped : Recover.dropped list) ~(recovered : Jide_lang.Span.t list)
      ~(context : string list) ~(file : string) ~(text : string)
      ~(items : Syntax.pprogram) ~(idirs : (string * string) list)
      ~(floor : int option) ~(preprocess : bool) : result * environment option =
    let length = String.length text in
    let initial =
      match List.fold_left Pretyping.Env.add_from Pretyping.Env.empty idirs with
      | environment -> Ok environment
      | exception exn -> Error exn
    in
    match initial with
    | Error exn ->
        ({ empty with diagnostics = [ Error.diagnostic_of_exn exn ] }, None)
    | Ok initial -> (
        match enter_file initial file with
        | Error exn ->
            ({ empty with diagnostics = [ Error.diagnostic_of_exn exn ] }, None)
        | Ok None ->
            (* [enter_file] answers [None] for a file already in
               [e_loader.loaded]. Every check starts from [Env.empty], so a
               top-level document cannot be in it and this branch is
               unreachable from the engine today; it is kept because the
               answer is a value, not an exception, and a caller that reuses
               an environment would meet it. *)
            ( {
                empty with
                diagnostics =
                  [
                    Error.to_diagnostic
                      (Error.make ~file:(Some file) ~kind:Error.Internal
                         ~span:(Jide_lang.Span.make ~start_byte:0 ~end_byte:0)
                         ("cannot enter " ^ file));
                  ];
              },
              None )
        | Ok (Some (environment, _resolved)) ->
            let document = normalise_path file in
            let loop =
              {
                file;
                name = file;
                text;
                document = file;
                length;
                resolve;
                idirs;
                place = true;
                origin = ref (Jide_lang.Span.make ~start_byte:0 ~end_byte:0);
                environment = ref environment;
                suppressions = ref [];
                diagnostics = ref [];
                facts = ref Facts.empty;
                required_diagnostics = ref [];
                in_document = true;
                awaiting = [];
                reached = false;
                kept = ref 0;
                budget;
                stopped = ref false;
                include_errors = ref 0;
                loaded = ref [ ("", document) ];
                sites = ref [];
              }
            in
            type_context loop ~chain:[ document ] ~document
              ~context:(List.map normalise_path context);
            type_sequence loop ~chain:[ document ] ~dropped ~recovered ~items;
            let environment =
              match exit_file !(loop.environment) with
              | Ok environment -> Some environment
              | Error exn ->
                  loop.diagnostics := Error.of_exn exn :: !(loop.diagnostics);
                  None
            in
            (match environment with
            | Some environment
              when preprocess && !(loop.diagnostics) = []
                   && !(loop.include_errors) = 0 -> (
                match preprocess_error environment with
                | Some error -> placed_preprocess loop error
                | None -> ())
            | _ -> ());
            ( {
                empty with
                diagnostics =
                  List.rev_map Error.to_diagnostic !(loop.diagnostics);
                facts = !(loop.facts);
                required =
                  required_of_errors
                    (List.filter
                       (fun (_, _, _, site) -> reachable_site ~floor site)
                       !(loop.required_diagnostics));
              },
              environment ))

  (** The files a [require] chain left errors in, one entry per file, in the
      order the first error of each was found. A file named the same way by
      two requires is one entry, because the caller prints one block of
      diagnostics for it. The spans of each entry index that entry's own text,
      and its [file] is the name the compiler prints it by. *)
  and required_of_errors
      (errors : (string * string * Jide_lang.Diagnostic.t * int option) list) :
      required list =
    let errors = List.rev errors in
    let files =
      List.fold_left
        (fun (files : (string * string * int option) list) (file, text, _, site) ->
          if List.exists (fun (held, _, _) -> String.equal held file) files then
            files
          else files @ [ (file, text, site) ])
        [] errors
    in
    List.map
      (fun (file, text, site) ->
        let diagnostics =
          List.filter_map
            (fun (held, _, diagnostic, _) ->
              if String.equal held file then Some diagnostic else None)
            errors
        in
        { file; text; diagnostics; site })
      files

  (** Record one diagnostic of a file reached through a [require] against
      that file, which is where the compiler prints it. It is counted against
      the same budget as a diagnostic placed on the document, so a broken
      include cannot return more lines than a broken document can. *)
  and record_required loop ~(file : string) ~(text : string)
      (diagnostic : Jide_lang.Diagnostic.t) =
    let site =
      if loop.in_document then Some !(loop.origin).Jide_lang.Span.start_byte
      else None
    in
    loop.required_diagnostics :=
      (file, text, diagnostic, site) :: !(loop.required_diagnostics);
    incr loop.kept;
    if !(loop.kept) >= loop.budget then loop.stopped := true

  (** Type the chain of files a document that is not an entry is checked in.
      [context] is the files the compiler enters before it, outermost first,
      and [document] the normalised path of the document the caller asked
      about, which the chain ends at and which the caller types itself. The
      first file is typed up to the [require] that names the next; the walk
      below it is {!type_require}, which hands the rest of the chain to the
      file it reaches. A file the lookup cannot answer is not a context: the
      document is then checked on its own, which is what the engine does for
      a file no entry requires. *)
  and type_context loop ~(chain : string list) ~(document : string)
      ~(context : string list) =
    match context with
    | [] -> ()
    | file :: rest -> (
        match loop.resolve file with
        | None -> ()
        | Some text ->
            let parsed = Parse.parse ~name:file text in
            let child =
              {
                loop with
                file;
                name = file;
                text;
                place = false;
                in_document = false;
                awaiting = rest @ [ document ];
                reached = false;
              }
            in
            type_sequence child ~chain:(file :: chain)
              ~dropped:(Parse.dropped parsed)
              ~recovered:(Parse.recovered parsed) ~items:(Parse.items parsed))

  and type_sequence loop ~(chain : string list)
      ~(dropped : Recover.dropped list) ~(recovered : Jide_lang.Span.t list)
      ~(items : Syntax.pprogram) =
    List.iter
      (fun dropped ->
        Option.iter
          (fun suppression ->
            loop.suppressions := suppression :: !(loop.suppressions))
          (suppression_of_dropped dropped))
      dropped;
    List.iter
      (fun item ->
        if (not !(loop.stopped)) && not loop.reached then (
          let snapshot = !(loop.environment) in
          let item_span = span_of_item item in
          if loop.place then loop.origin := item_span;
          (* A diagnostic in an item recovery rewrote the syntax of is a
             consequence of the broken text: the design drops every typing
             diagnostic in a function that contains a recovered region, not
             only the ones whose own span happens to overlap it. The item's
             span decides, so a use just after the rewritten bytes is
             covered too. *)
          let in_recovered_item =
            List.exists
              (fun (span : Jide_lang.Span.t) ->
                Jide_lang.Span.overlaps span item_span)
              recovered
          in
          match Location.unloc item with
          | Syntax.Prequire (from, names) ->
              type_require loop ~chain ~from names
          | Syntax.PNamespace (name, inner) -> (
              (* The library types a namespace's items itself, and its own
                 [tt_file] reads the disk. The namespace cannot be entered
                 here, so its requires are routed before it is typed. *)
              let rewritten, spliced =
                expand_namespace loop ~chain ~file:loop.file
                  ~prefix:(Location.unloc name) inner
              in
              let item =
                Location.mk_loc (Location.loc item)
                  (Syntax.PNamespace (name, rewritten))
              in
              match type_ordinary loop ~in_recovered_item snapshot item with
              | None -> ()
              | Some error ->
                  (* The namespace failed, and the failure names a file one
                     of its requires read: that file is the one that did not
                     type, and the item that asked for it has to say so. *)
                  report_spliced_failure loop spliced error)
          | _ -> ignore (type_ordinary loop ~in_recovered_item snapshot item)))
      items

  (** One item typed with the environment kept and restored around it. The
      failure is returned as well as kept, because a caller that knows what
      the item contained — a namespace whose requires were spliced into it —
      can tell which file the error came from and report it on the item that
      asked for that file. *)
  and type_ordinary loop ~(in_recovered_item : bool) snapshot item =
    match type_item snapshot item with
    | Ok next ->
        (* What this item typed left behind, read from the front of the typed
           list. The document's own items give its facts whole. A file a
           top-level [require] reaches gives its uses and nothing else: its
           items and calls index that file, not the document, and the
           document's consumers read only the items, but each use names its own
           file ([u_file]) and namespace ([u_reading]), so it is compared
           exactly. *)
        (let decls =
           added
             ~before:(Pretyping.Env.decls snapshot)
             ~after:(Pretyping.Env.decls next)
         in
         match Collect.of_item ~file:loop.file ~env:next ~item ~decls with
         | collected ->
             let collected =
               if loop.place then collected
               else { collected with Facts.items = []; Facts.calls = [] }
             in
             loop.facts := Facts.merge !(loop.facts) collected
         | exception exn ->
             (* A fact the adapter cannot read is a failure of this server,
                never of the user's file: it is reported as such instead of
                ending the check. A required file's failure has no span of its
                own in the document, so it is placed on the item being typed,
                and it is not counted as an error of the required file. *)
             if loop.place then (
               let error = Error.of_exn exn in
               if Error.belongs_to ~file:loop.file error then
                 keep loop ~in_recovered_item error)
             else
               note loop
                 (Error.make ~file:(Some loop.document) ~kind:Error.Internal
                    ~span:!(loop.origin)
                    (Printf.sprintf
                       "the facts of the required file `%s' cannot be read"
                       loop.file)));
        loop.environment := next;
        None
    | Error exn ->
        let error = Error.of_exn exn in
        keep loop ~in_recovered_item error;
        (* The names the item declares are now absent from the environment; a
           later reference to one of them would be reported as unresolved,
           and it is this failure seen again, not a second one. A namespace
           takes every name under it with it. *)
        List.iter
          (fun declaration ->
            loop.suppressions := Name declaration :: !(loop.suppressions))
          (declarations_of_item item);
        loop.environment := snapshot;
        Some error

  (** A namespace is typed as one item and the first error inside it ends the
      item. When that error names a file a [require] inside the namespace
      read, the file is the one that did not type: its offsets index that
      file, not the document, so {!keep} counted it instead of placing it and
      nothing else would ever say why the namespace is empty. Every [require]
      item between the document and that file says the file it read has
      errors, one per file of the chain. A top-level require of the same
      file takes the attributed path instead, because the file's own offsets
      can be placed against its own text; a namespace's items are spliced
      into one item, so they cannot. The names under the namespace stay
      suppressed, because a reference to one of them is this failure seen
      again.

      A suppression never hides this report: only an unresolved-name message
      can be suppressed, and this one is not one. What the report must not do
      is fire for an error that came from the document, or twice for one
      item, which is what [spliced] and [reported] decide. *)
  and report_spliced_failure loop (spliced : spliced list) (error : Error.t) =
    match error.Error.file with
    | None -> ()
    | Some failed -> (
        match
          List.find_opt
            (fun (site : spliced) -> String.equal site.path failed)
            spliced
        with
        | Some site -> report_spliced_chain loop spliced site
        | None -> ())

  (** The [require] items that read the failing file and the files that read
      them, innermost first: the file that did not type, then the file whose
      [require] read it, up to the document's own item. The chain is already
      in {!spliced}: a require written in a file records that file as its
      [span_file], and the require that read that file records the same file
      as its [path]. A site the expansion already reported is not repeated,
      but the chain above it still is, because those files did not type
      either. *)
  and report_spliced_chain loop (spliced : spliced list) (site : spliced) =
    if not site.reported then
      note loop
        (Error.make ~file:(Some loop.document) ~kind:Error.Typing
           ~span:(span_of_site loop site)
           (Printf.sprintf "the required file `%s' has errors" site.written));
    if not (String.equal site.span_file loop.file) then
      match
        List.find_opt
          (fun (parent : spliced) -> String.equal parent.path site.span_file)
          spliced
      with
      | Some parent -> report_spliced_chain loop spliced parent
      | None -> ()

  (** The span a spliced failure is placed at, which says which item it
      attaches to. A require item that belongs to the document is reported at
      its own span, [spliced.at]. One written in an included file is reported
      at the span of the document's item being typed, [loop.origin] — the
      item whose expansion reached the file — because the included file's
      offsets do not index the document. The answer is cut to the document's
      bytes either way. *)
  and span_of_site loop (site : spliced) =
    span_of_writer loop ~file:site.span_file ~at:site.at

  (** A [require] inside a namespace cannot go through {!type_require}: the
      library types a namespace's items itself ([Pretyping.tt_item]) and its
      own include reads the file from disk, not from the caller's lookup.
      [Pretyping.Env.enter_namespace] is not exported, so the namespace
      cannot be entered and left here; instead each required file is read
      through the same lookup a top-level require uses and its items are
      spliced where the [require] item stood, which is what the library does
      with them once it has read them. A file that cannot be read is
      reported on the [require] item, and every name under that namespace is
      treated as one the include would have declared: a later reference to
      one of them is this failure seen again, not a second error.

      A file that was read and does not type is reported on the [require]
      item too, by {!report_spliced_failure}, which is why the answer carries
      a record for every [require] the expansion made, the ones written in a
      required file included: a failure in the deepest file has to be
      attributable to the item of the file that read it. The suppression
      above is for the fabricated cascade only; it is never what an error of
      a file that was read depends on, because such an error is counted in
      [include_errors] rather than filtered. *)
  and expand_namespace loop ~(chain : string list) ~(file : string)
      ~(prefix : string) (items : Syntax.pitem Location.located list) :
      Syntax.pitem Location.located list * spliced list =
    let expanded =
      List.map
        (fun (item : Syntax.pitem Location.located) ->
          match Location.unloc item with
          | Syntax.Prequire (from, names) -> (
              match include_directory ~idirs:loop.idirs from with
              | Error name ->
                  (* One name for the item, not one per file it lists: the
                     compiler refuses the item at the name and reads nothing
                     for it, so the file it names is not read beside the
                     document in the name's place. *)
                  note loop (unknown_name loop ~file name);
                  forget_names loop prefix;
                  ([], [])
              | Ok from ->
                  let expanded =
                    List.map
                      (fun (name : string Location.located) ->
                        expand_required loop ~chain ~file ~prefix ~from
                          ~at:(span_of_item item) (Location.unloc name))
                      names
                  in
                  (List.concat_map fst expanded, List.concat_map snd expanded))
          | Syntax.PNamespace (name, inner) ->
              let rewritten, spliced =
                expand_namespace loop ~chain ~file
                  ~prefix:(qualified prefix (Location.unloc name))
                  inner
              in
              ( [
                  Location.mk_loc (Location.loc item)
                    (Syntax.PNamespace (name, rewritten));
                ],
                spliced )
          | _ -> ([ item ], []))
        items
    in
    (List.concat_map fst expanded, List.concat_map snd expanded)

  (** One required file of a namespace, read through the lookup and expanded
      into the items that take the place of the [require] item. A file that
      was read is answered with the records that let a failure inside its
      items be reported: its own [require] item and, below it, every require
      the file made in turn. A file that could not be read has no items to
      fail, and nothing is known about the names it would have declared. *)
  and expand_required loop ~(chain : string list) ~(file : string)
      ~(prefix : string) ~(from : string option) ~(at : Jide_lang.Span.t)
      (written : string) : Syntax.pitem Location.located list * spliced list =
    (* [from] is already resolved by {!include_directory}: an item whose
       [from] names no directory an idir bound never reaches here, so no
       file is read from the writer's directory in the name's place. *)
    let path =
      normalise_path (include_path ~base:(Filename.dirname file) ~from written)
    in
    (* [at] is the [require] item's span in [file], the file that wrote it:
       the document's item when the document wrote it, and otherwise an
       included file's, which does not index the document. The failure is
       placed on the document item that reached the file in that case. *)
    let report message =
      note loop
        (Error.make ~file:(Some loop.document) ~kind:Error.Typing
           ~span:(span_of_writer loop ~file ~at)
           message)
    in
    (* A file the namespace has loaded is skipped whether it closes a cycle or
       not, and without a diagnostic: the compiler's loader does the same, so
       a cycle or a self-require is accepted. *)
    if already_loaded loop ~namespace:prefix path then
      (* The compiler's [enter_file] answers [None] here: the file this
         namespace already read is not read again, so nothing is spliced a
         second time and nothing its first read reported is reported again. *)
      ([], [])
    else if List.length chain > max_include_depth then (
      report
        (Printf.sprintf "the required file `%s' is nested too deeply" written);
      forget_names loop prefix;
      ([], []))
    else
      match loop.resolve path with
      | None ->
          report (Printf.sprintf "cannot read the required file `%s'" written);
          forget_names loop prefix;
          ([], [])
      | Some text ->
          remember_loaded loop ~namespace:prefix path;
          loop.sites :=
            (path, span_of_writer loop ~file ~at, written) :: !(loop.sites);
          let parsed = Parse.parse ~name:path text in
          let failed = Parse.failed parsed in
          if failed then (
            (* The items below are spliced, so the names the file does
               declare are typed with the namespace's own items. The names
               recovery dropped are not, and a reference to one is this
               failure seen again. *)
            List.iter
              (fun (item : Syntax.pitem Location.located) ->
                List.iter
                  (fun declaration ->
                    loop.suppressions :=
                      Name (qualify_declaration prefix declaration)
                      :: !(loop.suppressions))
                  (declarations_of_item item))
              (Parse.items parsed);
            List.iter
              (fun dropped ->
                Option.iter
                  (fun suppression ->
                    loop.suppressions :=
                      qualify_suppression prefix suppression
                      :: !(loop.suppressions))
                  (suppression_of_dropped dropped))
              (Parse.dropped parsed);
            report (Printf.sprintf "the required file `%s' has errors" written));
          let items, spliced =
            expand_namespace loop ~chain:(path :: chain) ~file:path ~prefix
              (Parse.items parsed)
          in
          ( items,
            { written; path; at; span_file = file; reported = failed }
            :: spliced )

  (** [type_require loop ~chain names] types the files one [require] item
      names, in the environment the item stream has reached, by the same
      rules as this file. The item's [from] is refused once, at the name
      itself, and no file is read for the item when no idir bound the name:
      falling back to the writer's directory would accept a program the
      compiler rejects.

      A file already entered is not entered again, which is the compiler's
      own rule: [Env.enter_file] answers [None] for a path in
      [e_loader.loaded] and [tt_file] then reads and types nothing. A file
      reached twice through two chains would otherwise have every declaration
      it makes reported as already declared.

      A file that did not type leaves the environment it found. The compiler
      would not type the rest of the program at all, so there is no later
      environment to copy; leaving the half a broken file typed behind would
      put declarations in scope that the compiler never reaches, and the
      error that follows from one of them would be the first error seen
      again.

      A file of the context chain — the head of [loop.awaiting] — is typed
      with the rest of the chain below it, and the loop stops there, so the
      document is checked in the environment the entry builds up to its
      [require] and not in the one the items after it leave behind. The last
      element of the chain is the document itself, which the caller types.

      The name is the item's own, so one refusal is the answer however many
      files the item lists; the empty list is how the loop reads nothing for
      it. *)
  and type_require loop ~(chain : string list)
      ~(from : string Location.located option)
      (names : string Location.located list) =
    let base = Filename.dirname loop.file in
    let from, names =
      match include_directory ~idirs:loop.idirs from with
      | Ok from -> (from, names)
      | Error name ->
          note loop (unknown_name loop ~file:loop.file name);
          (None, [])
    in
    List.iter
      (fun (name : string Location.located) ->
        if (not !(loop.stopped)) && not loop.reached then
          let written = Location.unloc name in
          let path = normalise_path (include_path ~base ~from written) in
          (* The path the compiler prints for the file: the same resolution,
             spelled from the writer's own name, which is the document's as
             the caller gave it or a require's path. A cycle compares the two
             normalised paths instead; this one is only ever printed. *)
          let printed =
            lexical
              (include_path ~base:(Filename.dirname loop.name) ~from written)
          in
          (* A failure of the chain itself is the document's: its item names a
             file that cannot be read, whatever depth the failure was found
             at, so it is placed on the document's item and not counted as an
             error of an included file. *)
          let report message =
            note loop
              (Error.make ~file:(Some loop.document) ~kind:Error.Typing
                 ~span:!(loop.origin) message)
          in
          let awaited =
            match loop.awaiting with
            | next :: rest when String.equal next path -> Some rest
            | _ -> None
          in
          match awaited with
          | Some [] ->
              (* The chain ends at the document: the caller types it, with its
                 diagnostics placed on it. Nothing is read here, and this is
                 not a cycle: the document is the file the chain was built
                 for, not one it requires. *)
              loop.reached <- true
          | _ -> (
              if List.length chain > max_include_depth then
                report
                  (Printf.sprintf "the required file `%s' is nested too deeply"
                     written)
              else if already_loaded loop ~namespace:"" path then (
                if
                  (* Entered before: nothing is read and nothing is typed, as
                     [Env.enter_file] answers [None]. *)
                  awaited <> None
                then loop.reached <- true)
              else
                match loop.resolve path with
                | None ->
                    report
                      (Printf.sprintf "cannot read the required file `%s'"
                         written)
                | Some text ->
                    let parsed = Parse.parse ~name:path text in
                    let before = !(loop.include_errors) in
                    let recorded = List.length !(loop.required_diagnostics) in
                    let environment = !(loop.environment) in
                    remember_loaded loop ~namespace:"" path;
                    loop.sites :=
                      (path, !(loop.origin), written) :: !(loop.sites);
                    let include_loop =
                      {
                        loop with
                        file = path;
                        name = printed;
                        text;
                        place = false;
                        awaiting =
                          (match awaited with Some rest -> rest | None -> []);
                        reached = false;
                      }
                    in
                    type_sequence include_loop ~chain:(path :: chain)
                      ~dropped:(Parse.dropped parsed)
                      ~recovered:(Parse.recovered parsed)
                      ~items:(Parse.items parsed);
                    if awaited <> None then loop.reached <- true;
                    let failed =
                      !(loop.include_errors) > before || Parse.failed parsed
                    in
                    if failed then (
                      (* The file did not type, so its names are not in the
                         environment and every later reference to one of them
                         is this failure seen again. Its header is readable
                         even when its bodies are not, which is where the
                         names come from. *)
                      loop.environment := environment;
                      List.iter
                        (fun (item : Syntax.pitem Location.located) ->
                          List.iter
                            (fun declaration ->
                              loop.suppressions :=
                                Name declaration :: !(loop.suppressions))
                            (declarations_of_item item))
                        (Parse.items parsed);
                      List.iter
                        (fun dropped ->
                          Option.iter
                            (fun suppression ->
                              loop.suppressions :=
                                suppression :: !(loop.suppressions))
                            (suppression_of_dropped dropped))
                        (Parse.dropped parsed);
                      (* A file that did not parse has no item list to read
                         the names from: recovery drops the items it cannot
                         repair, so a reference to a name one of them would
                         have declared cannot be told from a use the file
                         never declared. The compiler stops at the file's
                         error and reports none of them, so every unresolved
                         name is suppressed until the file parses. *)
                      if Parse.failed parsed then
                        loop.suppressions := Any_name :: !(loop.suppressions));
                    (* A file that does not parse returns its own parse
                       diagnostics against itself, whatever else was recorded
                       for it or below it: the parse error never reaches
                       {!keep}, and a file with a typing error in one item and
                       a parse error in another has both. The compiler reports
                       the failure at the file. A file that parses and failed
                       with nothing recorded for it or below it, which happens
                       when an error inside an item recovery rewrote is only
                       counted, has no diagnostic to return, so the require
                       item says the file has errors. An error recorded below
                       the file is already attributed to the file that holds
                       it, so the require item says nothing about it. A check
                       that said nothing here would report a broken file
                       clean. *)
                    if Parse.failed parsed then
                      List.iter
                        (fun (diagnostic : Jide_lang.Diagnostic.t) ->
                          if not !(loop.stopped) then
                            record_required loop ~file:printed ~text diagnostic)
                        (Parse.diagnostics parsed)
                    else if
                      failed
                      && List.length !(loop.required_diagnostics) = recorded
                    then
                      report
                        (Printf.sprintf "the required file `%s' has errors"
                           written)))
      names

  (** A failure of preprocessing is placed the way a failure of typing is. One
      the document owns is kept with its span. One in an included file is not
      placed at its offsets, which index that file; the require item that read
      the file says it has errors instead. *)
  and placed_preprocess loop (error : Error.t) =
    match error.Error.file with
    | Some failed when not (String.equal failed loop.document) -> (
        match
          List.find_opt
            (fun (path, _, _) -> String.equal path failed)
            !(loop.sites)
        with
        | Some (_, span, written) ->
            note loop
              (Error.make ~file:(Some loop.document) ~kind:Error.Typing ~span
                 (Printf.sprintf "the required file `%s' has errors" written))
        | None ->
            note loop
              {
                error with
                Error.file = Some loop.document;
                span = Jide_lang.Span.make ~start_byte:0 ~end_byte:0;
              })
    | _ -> note loop error

  and keep loop ~(in_recovered_item : bool) (error : Error.t) =
    (* Only the document's own errors have spans the caller can place. With
       [place] false an error of the file this loop types is the file's own:
       it is reported against that file, with the text its span indexes, which
       is where the compiler prints it too. *)
    let here = loop.place && Error.belongs_to ~file:loop.file error in
    if not here then incr loop.include_errors;
    if here then (if not in_recovered_item then note loop error)
    else if (not in_recovered_item) && Error.belongs_to ~file:loop.file error
    then
      record_required loop ~file:loop.name ~text:loop.text
        (Error.to_diagnostic error)

  and note loop (error : Error.t) =
    (* Everything [note] is given is placed on the document, so its span is
       cut to the document's bytes first. The budget is enforced here rather
       than at the call sites as well, so no path — the expansion of a
       namespace's requires included, which writes several diagnostics for
       one item — can return more than the budget it was given. *)
    let error =
      { error with Error.span = within ~length:loop.length error.Error.span }
    in
    if
      (not !(loop.stopped))
      && not (suppressed !(loop.suppressions) error.Error.message)
    then (
      loop.diagnostics := error :: !(loop.diagnostics);
      incr loop.kept;
      if !(loop.kept) >= loop.budget then loop.stopped := true)

  (** [check ~resolve ~file ~text ~idirs ~context] parses and types one
      document. [context] is the chain of files the compiler enters before it
      when the file is not an entry, outermost first, and the empty list is a
      file checked on its own. The parser's formatter is redirected for the
      whole check, and everything that follows it, the warning parse of the
      captured text included, is inside a catch-all: an exception the library
      did not turn into a diagnostic is reported as one, so nothing reaches
      the engine as a raise. *)
  let check ~(resolve : lookup) ~(file : string) ~(text : string)
      ~(idirs : (string * string) list) ~(context : string list) : result =
    match
      Warnings.with_capture (fun () ->
          Options.apply ();
          let parsed = Parse.parse ~name:file text in
          let parse_diagnostics = Parse.diagnostics parsed in
          let budget =
            max 0 (max_diagnostics - List.length parse_diagnostics)
          in
          let typed, _environment =
            type_items ~resolve ~budget ~dropped:(Parse.dropped parsed)
              ~recovered:(Parse.recovered parsed) ~context ~file ~text
              ~items:(Parse.items parsed) ~idirs
          in
          { typed with diagnostics = parse_diagnostics @ typed.diagnostics })
    with
    | outcome, printed -> (
        match Warnings.parse ~file printed with
        | warnings -> { outcome with warnings }
        | exception exn ->
            {
              empty with
              diagnostics =
                outcome.diagnostics @ [ Error.diagnostic_of_exn exn ];
            })
    | exception exn ->
        { empty with diagnostics = [ Error.diagnostic_of_exn exn ] }
end
