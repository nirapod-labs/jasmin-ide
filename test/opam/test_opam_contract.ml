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

