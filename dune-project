(lang dune 3.24)
(name jasmin-lsp)
(using menhir 2.1)
(implicit_transitive_deps false)
(generate_opam_files true)

(package
 (name jasmin-lsp)
 (synopsis "Language server for the Jasmin language")
 (description
  "Editor support for the Jasmin language in VS Code and Zed: diagnostics, navigation, hover, completion, signature help and rename, from the Jasmin compiler's own parser and type checker.")
 (depends
  (ocaml (>= 4.14))
  dune
  (jasmin (>= 2026.09.0))
  menhirLib
  lsp
  jsonrpc
  yojson
  zarith))
