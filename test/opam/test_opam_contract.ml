(* The opam contract of the package, checked against the tree it ships.

   Two files describe the same package: `dune-project` carries the `depends`
   list and `jasmin-lsp.opam` is generated from it. This test reads both, plus
   every `dune` file of the workspace and the two files the dependency install
   reads, and answers four questions:

    1. is every library a dune file names either a library of this workspace,
       one the compiler ships, or a package the `depends` list names? A
       dependency missing from the list is a defect even when another package
       happens to pull the library in: the next release of that package would
       take it away silently.
    2. do `jasmin-lsp.opam` and `dune-project` name the same packages? A
       dependency added to one file alone is a defect.
    3. does an install file provide every package the opam file names? The
       Jasmin library is the one exception: this repository builds it from the
       release tarball rather than installing a package for it.
    4. does the `jasmin` constraint hold for the release `vendor/jasmin.lock`
       pins? A release that no longer satisfies the constraint is a defect.

   The workspace root is the first argument, given by the dune stanza as
   `%{workspace_root}`: the test reads the copy of the tree the current build
   was made from, not a path of its own. *)

(* The directories that are not part of the workspace a dune file describes:
   the build tree, the fetched library (its own requirements are
   `vendor/requirements.opam`'s, not the package's), and the directories the
   editor tooling fills in. *)
let skip = function
  | "_build" | "_opam" | ".git" | "vendor" | "node_modules" | ".vscode-test"
  | ".artifacts" | "coverage" ->
      true
  | _ -> false

(* Libraries every switch has, because the compiler ships them; no opam
   package names them, so a dune file may list them without a line in the
   depends list. `threads.posix` is the POSIX half of the thread library. *)
let from_compiler =
  [
    "unix";
    "threads";
    "str";
    "bytes";
    "bigarray";
    "dynlink";
    "stdlib";
    "ocamlcommon";
    "ocamlbytecomp";
    "ocamloptcomp";
    "compiler-libs";
  ]

(* dune writes this one itself, under the {with-doc} filter, whatever the
   dune-project says. *)
let written_by_dune = [ "odoc" ]

(* A dune file is an s-expression. Comments run from a semicolon to the end of
   the line and strings are double-quoted, so the two are read first and the
   rest is atoms and parentheses. *)
type node = Atom of string | List of node list

exception Syntax of string

let parse text =
  let length = String.length text in
  let position = ref 0 in
  let fail message =
    raise (Syntax (Printf.sprintf "%s (at offset %d)" message !position))
  in
  let is_delimiter = function
    | ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"' -> true
    | _ -> false
  in
  let rec skip_blank () =
    if !position < length then
      match text.[!position] with
      | ' ' | '\t' | '\n' | '\r' ->
          incr position;
          skip_blank ()
      | ';' ->
          while !position < length && text.[!position] <> '\n' do
            incr position
          done;
          skip_blank ()
      | _ -> ()
  and sequence ~inside =
    let items = ref [] in
    let finished = ref false in
    while not !finished do
      skip_blank ();
      if !position >= length then
        if inside then fail "a list is not closed" else finished := true
      else if text.[!position] = ')' then
        if inside then (
          incr position;
          finished := true)
        else fail "a list ends where none began"
      else items := item () :: !items
    done;
    List.rev !items
  and item () =
    skip_blank ();
    if !position >= length then fail "the file ends inside a form";
    match text.[!position] with
    | '(' ->
        incr position;
        List (sequence ~inside:true)
    | '"' ->
        incr position;
        let buffer = Buffer.create 16 in
        let rec more () =
          if !position >= length then fail "a string is not closed";
          match text.[!position] with
          | '"' -> incr position
          | '\\' when !position + 1 < length ->
              Buffer.add_char buffer text.[!position + 1];
              position := !position + 2;
              more ()
          | character ->
              Buffer.add_char buffer character;
              incr position;
              more ()
        in
        more ();
        Atom (Buffer.contents buffer)
    | _ ->
        let start = !position in
        while !position < length && not (is_delimiter text.[!position]) do
          incr position
        done;
        if !position = start then fail "an atom is empty";
        Atom (String.sub text start (!position - start))
  in
  sequence ~inside:false

let children = function Atom _ -> [] | List items -> items
let atom = function Atom name -> Some name | List _ -> None
let field = function List (Atom name :: _) -> Some name | _ -> None

let rec each node ~f =
  f node;
  List.iter (fun child -> each child ~f) (children node)

let failures = ref 0
let cases = ref 0

let case name =
  incr cases;
  Printf.printf "case %s\n%!" name

let fail format =
  Printf.ksprintf
    (fun message ->
      incr failures;
      Printf.printf "FAIL %s\n%!" message)
    format

(* The names in the `libraries` field of every stanza of one dune file. A
   nested form — `(re_export x)`, `(select …)`, `(:standard \ x)` — names a
   library in a shape this test does not read, and answering it with nothing
   would leave that library unchecked by the only case that exists to catch a
   dependency going missing. So it is a failure, not a name. *)
let libraries_of forms =
  let found = ref [] in
  List.iter
    (fun form ->
      each form ~f:(fun node ->
          match field node with
          | Some "libraries" -> (
              match children node with
              | Atom _ :: entries ->
                  List.iter
                    (function
                      | Atom name -> found := name :: !found
                      | List _ ->
                          fail
                            "a `libraries` field carries a nested form, which \
                             this test does not read; name the library in \
                             plain form or teach this test the form")
                    entries
              | _ -> ())
          | _ -> ()))
    forms;
  List.rev !found

(* The libraries one dune file defines: the internal name of each `library`
   stanza and the public name it may also carry. *)
let definitions_of forms =
  let found = ref [] in
  List.iter
    (fun form ->
      each form ~f:(fun node ->
          if field node = Some "library" then
            List.iter
              (fun child ->
                match child with
                | List (Atom ("name" | "public_name") :: rest) -> (
                    match List.filter_map atom rest with
                    | name :: _ -> found := name :: !found
                    | [] -> ())
                | _ -> ())
              (children node)))
    forms;
  List.rev !found

(* The `depends` list of the project's one package, as (name, constraint)
   pairs. A bare name carries no constraint; a list carries the constraint
   forms after it. *)
let project_depends forms =
  match List.filter (fun form -> field form = Some "package") forms with
  | [ package ] -> (
      match
        List.filter (fun form -> field form = Some "depends") (children package)
      with
      | [ depends ] ->
          let entries =
            match children depends with [] -> [] | _ :: entries -> entries
          in
          List.map
            (fun item ->
              match item with
              | Atom name -> (name, [])
              | List (Atom name :: constraint_) -> (name, constraint_)
              | _ ->
                  raise (Syntax "a depends entry is neither a name nor a list"))
            entries
      | _ ->
          raise (Syntax "the package does not carry exactly one depends list"))
  | _ -> raise (Syntax "dune-project does not carry exactly one package")

(* The package names of the `depends:` field of an opam file. The field is a
   bracketed list of quoted names, each optionally followed by a `{...}`
   filter, whose quoted strings are constraints and not names. *)
let opam_depends text =
  let length = String.length text in
  let field_start =
    let rec find index =
      if index >= length then
        raise (Syntax "the opam file has no depends field")
      else
        let at_line_start = index = 0 || text.[index - 1] = '\n' in
        if
          at_line_start
          && index + 8 <= length
          && String.equal (String.sub text index 8) "depends:"
        then index + 8
        else find (index + 1)
    in
    find 0
  in
  let position = ref field_start in
  let names = ref [] in
  let braces = ref 0 in
  let rec skip_to_open () =
    if !position >= length then raise (Syntax "the depends field never opens")
    else
      match text.[!position] with
      | '[' -> incr position
      | _ ->
          incr position;
          skip_to_open ()
  in
  let rec scan () =
    if !position >= length then raise (Syntax "the depends field never closes")
    else
      match text.[!position] with
      | ']' -> ()
      | '{' ->
          incr braces;
          incr position;
          scan ()
      | '}' ->
          decr braces;
          incr position;
          scan ()
      | '"' ->
          incr position;
          let buffer = Buffer.create 16 in
          let rec quoted () =
            if !position >= length then raise (Syntax "a name is not closed")
            else
              match text.[!position] with
              | '"' -> incr position
              | character ->
                  Buffer.add_char buffer character;
                  incr position;
                  quoted ()
          in
          quoted ();
          if !braces = 0 then names := Buffer.contents buffer :: !names;
          scan ()
      | _ ->
          incr position;
          scan ()
  in
  skip_to_open ();
  scan ();
  List.rev !names

(* The `version` line of vendor/jasmin.lock: `key = value` lines with comments
   that start with a hash. *)
let lock_version text =
  let lines = String.split_on_char '\n' text in
  let value_of line =
    match String.index_opt line '=' with
    | None -> None
    | Some equals ->
        let key = String.trim (String.sub line 0 equals) in
        let value =
          String.trim
            (String.sub line (equals + 1) (String.length line - equals - 1))
        in
        if String.equal key "version" then Some value else None
  in
  match List.filter_map value_of lines with
  | [ version ] -> version
  | [] -> raise (Syntax "vendor/jasmin.lock has no version line")
  | _ -> raise (Syntax "vendor/jasmin.lock has more than one version line")

(* A dotted version compared component by component, as numbers where the
   components are numbers. Jasmin releases are `YYYY.MM.PATCH`. *)
let compare_version left right =
  let split version = String.split_on_char '.' version in
  let as_number component =
    match int_of_string_opt component with
    | Some number -> `Number number
    | None -> `Text component
  in
  let rec walk left right =
    match (left, right) with
    | [], [] -> 0
    | [], component :: _ -> compare (as_number "0") (as_number component)
    | component :: _, [] -> compare (as_number component) (as_number "0")
    | left :: lefts, right :: rights -> (
        match compare (as_number left) (as_number right) with
        | 0 -> walk lefts rights
        | different -> different)
  in
  walk (split left) (split right)

let satisfies ~version constraint_ =
  match constraint_ with
  | [ form ] -> (
      match List.filter_map atom (children form) with
      | [ operator; bound ] ->
          let order = compare_version version bound in
          let holds =
            match operator with
            | ">=" -> order >= 0
            | ">" -> order > 0
            | "=" -> order = 0
            | "<=" -> order <= 0
            | "<" -> order < 0
            | _ ->
                raise
                  (Syntax
                     (Printf.sprintf
                        "the jasmin constraint uses %s, which this test does \
                         not read; teach it the operator or write the \
                         constraint with the one the release is checked \
                         against"
                        operator))
          in
          (holds, Printf.sprintf "%s %s" operator bound)
      | _ ->
          raise
            (Syntax "the jasmin dependency carries no operator and version pair")
      )
  | _ ->
      raise
        (Syntax
           "the jasmin dependency does not carry exactly one constraint; this \
            test reads one")

let rec dune_files directory =
  let entries = try Sys.readdir directory with Sys_error _ -> [||] in
  Array.sort compare entries;
  Array.fold_left
    (fun found entry ->
      let path = Filename.concat directory entry in
      let is_directory =
        try Sys.is_directory path with Sys_error _ -> false
      in
      if is_directory then if skip entry then found else found @ dune_files path
      else if String.equal entry "dune" then found @ [ path ]
      else found)
    [] entries

(* The first components of a library name, so `threads.posix` also answers to
   `threads` and `batteries.unthreaded` to `batteries`. *)
let candidates name =
  match String.index_opt name '.' with
  | None -> [ name ]
  | Some dot -> [ name; String.sub name 0 dot ]

let read path =
  try In_channel.with_open_bin path In_channel.input_all
  with Sys_error message ->
    fail "cannot read %s: %s" path message;
    ""

let () =
  let root =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else Filename.concat ".." ".."
  in
  if not (Sys.file_exists (Filename.concat root "dune-project")) then (
    Printf.eprintf "the workspace root %s holds no dune-project\n%!" root;
    exit 2);
  let project_path = Filename.concat root "dune-project" in
  let project = parse (read project_path) in
  let package_depends = project_depends project in
  let package_names = List.map fst package_depends in
  let dune_paths = dune_files root in
  (* A path as it reads from the workspace root, not from the build tree this
     action runs in: `jasmin/dune`, not `../../jasmin/dune`. *)
  let shown path =
    let prefix = root ^ Filename.dir_sep in
    if
      String.length path > String.length prefix
      && String.equal (String.sub path 0 (String.length prefix)) prefix
    then
      String.sub path (String.length prefix)
        (String.length path - String.length prefix)
    else path
  in
  let dune_forms =
    List.map (fun path -> (path, parse (read path))) dune_paths
  in
  let local =
    List.concat_map (fun (_, forms) -> definitions_of forms) dune_forms
  in
  let libraries =
    List.concat_map
      (fun (path, forms) ->
        List.map (fun name -> (path, name)) (libraries_of forms))
      dune_forms
  in

  (* 1. Every library a dune file names is a library of this workspace, one the
     compiler ships, or a package the depends list names. *)
  case "libraries";
  let provided = local @ from_compiler @ package_names in
  let missing =
    List.filter
      (fun (_, name) ->
        not
          (List.exists
             (fun candidate -> List.mem candidate provided)
             (candidates name)))
      libraries
  in
  List.iter
    (fun (path, name) ->
      fail
        "%s names the library %s, which is neither a library of this workspace \
         nor a package of the depends list in dune-project"
        (shown path) name)
    missing;
  Printf.printf
    "  %d dune files, %d library references, %d of them to a package\n%!"
    (List.length dune_paths) (List.length libraries)
    (List.length
       (List.filter (fun (_, name) -> not (List.mem name local)) libraries));

  (* 2. The generated opam file names the packages dune-project names, so a
     dependency added to one file alone is a failure. *)
  case "agreement";
  let opam_path = Filename.concat root "jasmin-lsp.opam" in
  let opam_names = opam_depends (read opam_path) in
  let expected =
    List.filter (fun name -> not (List.mem name written_by_dune)) opam_names
  in
  List.iter
    (fun name ->
      if not (List.mem name package_names) then
        fail
          "%s names %s, which the depends list in dune-project does not; a \
           dependency belongs in dune-project, and the opam file is generated \
           from it"
          (shown opam_path) name)
    expected;
  List.iter
    (fun name ->
      if not (List.mem name expected) then
        fail
          "dune-project names %s, which %s does not; rebuild the opam file \
           with dune build"
          name (shown opam_path))
    package_names;
  Printf.printf "  %d packages in dune-project, %d in the opam file\n%!"
    (List.length package_names)
    (List.length expected);

  (* 3. Every package the opam file names is installed by one of the two files
     `make deps` reads, or is the Jasmin library this repository assembles for
     itself. A package named in dune-project and in neither install file is one
     `make deps` would leave out, and the build would fail on it late.

     The Jasmin library is the second kind and not an installed package. The
     server links the compiler's own parser and type checker, so the package
     that carries them is named; but that package installs the library as
     `jasmin.jasmin`, the dune files here ask for `jasmin`, and a build with
     the tree removed therefore fails with `Library "jasmin" not found`. The
     tree `vendor/fetch.sh` assembles is what supplies the library to every
     build this repository runs, and no opam install of this package builds.
     docs/nix.md records what is measured about that and what is not. *)
  case "installs";
  let installed_paths =
    [
      Filename.concat root "jasmin-lsp.opam.locked";
      Filename.concat root "vendor/requirements.opam";
    ]
  in
  let installed =
    List.concat_map (fun path -> opam_depends (read path)) installed_paths
  in
  let supplied_by_the_tree = [ "jasmin" ] in
  List.iter
    (fun name ->
      if
        (not (List.mem name installed))
        && not (List.mem name supplied_by_the_tree)
      then
        fail
          "%s names %s, which neither %s nor %s installs and which this \
           repository does not build"
          (shown opam_path) name
          (shown (List.nth installed_paths 0))
          (shown (List.nth installed_paths 1)))
    expected;
  List.iter
    (fun name ->
      if List.mem name supplied_by_the_tree then
        Printf.printf
          "  %s is supplied by vendor/fetch.sh, not by an install\n%!" name)
    expected;
  Printf.printf "  %d packages installed by the two files, %d named here\n%!"
    (List.length installed) (List.length expected);

  (* 4. The vendored release satisfies the jasmin constraint the package
     carries. *)
  case "jasmin";
  let lock = Filename.concat root "vendor/jasmin.lock" in
  let version = lock_version (read lock) in
  (match List.assoc_opt "jasmin" package_depends with
  | None -> fail "dune-project does not depend on jasmin"
  | Some constraint_ ->
      let holds, written = satisfies ~version constraint_ in
      if not holds then
        fail
          "the package requires jasmin %s, but %s pins %s, which does not \
           satisfy it"
          written lock version
      else
        Printf.printf "  jasmin %s satisfies the requirement %s\n%!" version
          written);

  Printf.printf "%d cases, %d failures\n%!" !cases !failures;
  if !failures > 0 then exit 1
