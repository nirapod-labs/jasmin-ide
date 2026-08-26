(* The opam contract of the package, checked against the tree it ships.

   Two files describe the same package: `dune-project` carries the `depends`
   list and `jasmin-lsp.opam` is generated from it. This test reads both, plus
   every `dune` file of the workspace, and answers three questions:

   1. is every library a dune file names either a library of this workspace,
      one the compiler ships, or a package the `depends` list names? A
      dependency missing from the list is a defect even when another package
      happens to pull the library in: the next release of that package would
      take it away silently.
   2. do `jasmin-lsp.opam` and `dune-project` name the same packages? A
      dependency added to one file alone is a defect.
   3. does the `jasmin` constraint hold for the release `vendor/jasmin.lock`
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

let atoms_of node = List.filter_map atom (children node)

(* The names in the `libraries` field of every stanza of one dune file. *)
