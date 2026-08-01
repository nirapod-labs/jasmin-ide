open Jasmin

type result = {
  diagnostics : Jide_lang.Diagnostic.t list;
  warnings : Warnings.t list;
  facts : Facts.t;
}

let empty = { diagnostics = []; warnings = []; facts = Facts.empty }

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
    make, or every name under a namespace whose contents recovery dropped. *)
type suppression = Name of declaration | Namespace of string

(** Whether [suppression] covers [declaration]. A namespace covers every name
    the compiler would print qualified with it, which is how a reference to
    an item of a dropped namespace reads. *)
let covers (suppression : suppression) (declaration : declaration) =
  match suppression with
  | Name expected -> expected = declaration
  | Namespace prefix ->
      let name = name_of_declaration declaration in
      let length = String.length prefix in
      String.length name > length + 2
      && String.sub name 0 length = prefix
      && String.sub name length 2 = "::"

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
let suppression_of_dropped = function
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

(** A name is a file name as the compiler resolves it: relative to the file
    that requires it, or to the directory an idir binds when the item names
    a [from]. *)
let include_path ~(idirs : (string * string) list) ~(base : string)
    ~(from : string option) (name : string) : string =
  let base =
    match from with
    | None -> base
    | Some from -> (
        match List.assoc_opt from idirs with Some dir -> dir | None -> base)
  in
  if Filename.is_relative name then Filename.concat base name else name

(** [normalise_path path] is one spelling for one file: an absolute path
    with no [.] and no [..] left in it. The fold is lexical, like the
    engine's own resolution: nothing here asks the filesystem, so a symbolic
    link is not resolved and two paths that name one file through different
    links stay different. It is what makes a cycle compare equal to itself
    however the require spelled the file. *)
let normalise_path (path : string) : string =
  let path =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path
  in
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

(** How many required files one document may nest. A cycle is caught by the
    chain before this, and this bounds a tree of distinct files. *)
let max_include_depth = 8

(* The bytes of one item, so the loop can ask whether recovery rewrote any
   of them. *)
let span_of_item (item : Syntax.pitem Location.located) : Jide_lang.Span.t =
  let location = item.Location.pl_loc in
  Jide_lang.Span.make
    ~start_byte:(max 0 location.Location.loc_bchar)
    ~end_byte:(max 0 location.Location.loc_echar)

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

  type loop = {
    file : string;
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
    kept : int ref;
    budget : int;
    stopped : bool ref;
    include_errors : int ref;
  }
  (** Everything one pass over one item stream learns. A required file is
      typed by the same pass with its own [file] and its own recovered
      regions, and shares the environment, the suppressions and the budget.
      The diagnostics are the document's own: an error attributed to another
      file is counted in [include_errors] instead of being placed. *)

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

  (** [type_items ~file ~items ~idirs] types the items of one file one by one
      and returns the diagnostics, plus the environment the loop leaves
      behind when it started at all.

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
      document, whose bytes it does not describe: the require item says the
      file has one instead.

      At most {!budget} diagnostics are kept. Every call into the library
      this function makes is wrapped, and {!check}, the only entry point the
      engine uses, ends in a catch-all: no exception leaves the adapter. *)
  let rec type_items ~(resolve : lookup) ~(budget : int)
      ~(dropped : Recover.dropped list) ~(recovered : Jide_lang.Span.t list)
      ~(file : string) ~(length : int) ~(items : Syntax.pprogram)
      ~(idirs : (string * string) list) : result * environment option =
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
            let loop =
              {
                file;
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
                kept = ref 0;
                budget;
                stopped = ref false;
                include_errors = ref 0;
              }
            in
            type_sequence loop
              ~chain:[ normalise_path file ]
              ~dropped ~recovered ~items;
            let environment =
              match exit_file !(loop.environment) with
              | Ok environment -> Some environment
              | Error exn ->
                  loop.diagnostics := Error.of_exn exn :: !(loop.diagnostics);
                  None
            in
            ( {
                empty with
                diagnostics =
                  List.rev_map Error.to_diagnostic !(loop.diagnostics);
                facts = !(loop.facts);
              },
              environment ))

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
        if not !(loop.stopped) then (
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
              type_require loop ~chain
                ~from:(Option.map Location.unloc from)
                names
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
           list. Only the document's loop collects facts: an included file's
           ranges index that file, not the document. *)
        if loop.place then (
          let decls =
            added
              ~before:(Pretyping.Env.decls snapshot)
              ~after:(Pretyping.Env.decls next)
          in
          match Collect.of_item ~file:loop.file ~env:next ~item ~decls with
          | collected -> loop.facts := Facts.merge !(loop.facts) collected
          | exception exn ->
              (* A fact the adapter cannot read is a failure of this server,
                 never of the user's file: it is reported as such instead of
                 ending the check. *)
              let error = Error.of_exn exn in
              if Error.belongs_to ~file:loop.file error then
                keep loop ~in_recovered_item error);
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
      errors — the same diagnostic a top-level require of the same file gives,
      one per file of the chain — and the names under the namespace stay
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
    within ~length:loop.length
      (if loop.place && String.equal site.span_file loop.file then site.at
       else !(loop.origin))

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
          | Syntax.Prequire (from, names) ->
              let from = Option.map Location.unloc from in
              let expanded =
                List.map
                  (fun (name : string Location.located) ->
                    expand_required loop ~chain ~file ~prefix ~from
                      ~at:(span_of_item item) (Location.unloc name))
                  names
              in
              (List.concat_map fst expanded, List.concat_map snd expanded)
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
    let path =
      normalise_path
        (include_path ~idirs:loop.idirs ~base:(Filename.dirname file) ~from
           written)
    in
    (* [at] is the [require] item's span in [file], the file that wrote it:
       the document's item when the document wrote it, and otherwise an
       included file's, which does not index the document. The failure is
       placed on the document item that reached the file in that case. *)
    let report message =
      let span =
        within ~length:loop.length
          (if loop.place && String.equal file loop.file then at
           else !(loop.origin))
      in
      note loop
        (Error.make ~file:(Some loop.document) ~kind:Error.Typing ~span message)
    in
    let forget_names () =
      if not (String.equal prefix "") then
        loop.suppressions := Namespace prefix :: !(loop.suppressions)
    in
    if List.mem path chain then (
      report (Printf.sprintf "the required file `%s' requires itself" written);
      forget_names ();
      ([], []))
    else if List.length chain > max_include_depth then (
      report
        (Printf.sprintf "the required file `%s' is nested too deeply" written);
      forget_names ();
      ([], []))
    else
      match loop.resolve path with
      | None ->
          report (Printf.sprintf "cannot read the required file `%s'" written);
          forget_names ();
          ([], [])
      | Some text ->
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
                match suppression_of_dropped dropped with
                | Some (Name declaration) ->
                    loop.suppressions :=
                      Name (qualify_declaration prefix declaration)
                      :: !(loop.suppressions)
                | Some (Namespace name) ->
                    loop.suppressions :=
                      Namespace (qualified prefix name) :: !(loop.suppressions)
                | None -> ())
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
      rules as this file. *)
  and type_require loop ~(chain : string list) ~(from : string option)
      (names : string Location.located list) =
    let base = Filename.dirname loop.file in
    List.iter
      (fun (name : string Location.located) ->
        if not !(loop.stopped) then
          let written = Location.unloc name in
          let path =
            normalise_path (include_path ~idirs:loop.idirs ~base ~from written)
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
          if List.mem path chain then
            report
              (Printf.sprintf "the required file `%s' requires itself" written)
          else if List.length chain > max_include_depth then
            report
              (Printf.sprintf "the required file `%s' is nested too deeply"
                 written)
          else
            match loop.resolve path with
            | None ->
                report
                  (Printf.sprintf "cannot read the required file `%s'" written)
            | Some text ->
                let parsed = Parse.parse ~name:path text in
                let before = !(loop.include_errors) in
                let include_loop = { loop with file = path; place = false } in
                type_sequence include_loop ~chain:(path :: chain)
                  ~dropped:(Parse.dropped parsed)
                  ~recovered:(Parse.recovered parsed)
                  ~items:(Parse.items parsed);
                if !(loop.include_errors) > before || Parse.failed parsed then (
                  (* The file did not type, so its names are not in the
                     environment and every later reference to one of them is
                     this failure seen again. Its header is readable even
                     when its bodies are not, which is where the names come
                     from. *)
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
                  report
                    (Printf.sprintf "the required file `%s' has errors" written)))
      names

  and keep loop ~(in_recovered_item : bool) (error : Error.t) =
    (* Only the document's own errors have spans the caller can place. With
       [place] false every error is the included file's, whatever file it
       names, and it is counted rather than placed. *)
    let here = loop.place && Error.belongs_to ~file:loop.file error in
    if not here then incr loop.include_errors;
    if here && not in_recovered_item then note loop error

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

  (** [check ~resolve ~file ~text ~idirs] parses and types one document. The
      parser's formatter is redirected for the whole check, and everything
      that follows it, the warning parse of the captured text included, is
      inside a catch-all: an exception the library did not turn into a
      diagnostic is reported as one, so nothing reaches the engine as a
      raise. *)
  let check ~(resolve : lookup) ~(file : string) ~(text : string)
      ~(idirs : (string * string) list) : result =
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
              ~recovered:(Parse.recovered parsed) ~file
              ~length:(String.length text) ~items:(Parse.items parsed) ~idirs
          in
          {
            typed with
            diagnostics = parse_diagnostics @ typed.diagnostics;
          })
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
