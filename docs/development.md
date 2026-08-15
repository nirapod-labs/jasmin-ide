# Development

How to build the tree, run its tests, and compare the server with the Jasmin
compiler over a corpus. Every command runs from the repository root.

## The edge channel

The release channel builds against the tarball `vendor/jasmin.lock` pins. The
edge channel builds against upstream `main`, which has no release tarball, so
you make one from a checkout. That needs Rocq and the libraries the proofs
use; `.github/workflows/edge.yml` installs the versions upstream names.

```
make -C <jasmin checkout>/compiler CIL dist
sh vendor/fetch.sh <jasmin checkout>/compiler/jasmin-compiler.tgz <its sha256>
JIDE_JASMIN_CHANNEL=edge dune build --root . @install
```

Don't use `make build` here: its vendor step puts the release tarball back.
`JIDE_JASMIN_CHANNEL=edge` selects `jasmin/channel_main.ml`, the one place
where the two Jasmin versions' interfaces differ. `jasmin-lsp --version` then
says `built from vendored tarball sha256:<digest>`. Compare an edge build
with the corpus of the same upstream commit, never with the release's.

## Layout

```
lang/                positions, ranges, the line index, diagnostics, URIs
jasmin/              the adapter over the Jasmin library: parse, recovery, typing
engine/              documents, the project file, the index, the resolver,
                     completion, signature help, rename
lsp/                 JSON-RPC framing and LSP message types
controller/          request handlers, capabilities, the debounce
lsp-server/native/   the jasmin-lsp executable
compiler/            jasmin-lsp check
editor/code/         the VS Code extension
editor/zed/          the Zed extension and its queries
test/                cram suites and the corpus comparisons
fuzz/, bench/        the fuzzers and the benchmark
vendor/              the script that assembles the Jasmin library
scripts/             the release scripts (docs/release.md)
```
