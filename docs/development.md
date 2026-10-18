# Development

How to build the tree, run its tests, and compare the server with the Jasmin
compiler over a corpus. Every command runs from the repository root.

## Building

You need opam, and for the extensions pnpm and a Rust toolchain with the
`wasm32-wasip2` target.

```
opam switch create jasmin-ide --empty
make deps
make build
```

`make deps` installs `jasmin-lsp.opam.locked`, which names the compiler too,
and then `vendor/requirements.opam`, into the switch `OPAM_SWITCH`
(`jasmin-ide` by default). `make build` first runs `vendor/fetch.sh`, which
downloads the Jasmin release pinned in `vendor/jasmin.lock`, checks its
SHA-256 and copies the library into `vendor/jasmin/`. That directory isn't
committed. A second run finds it assembled and does nothing.

The server lands in `_build/default/lsp-server/native/main.exe`, and under
its public name in `_build/install/default/bin/jasmin-lsp`.

## Testing

```
make test     # every test in the tree
make fmt      # the formatting check; dune build @fmt --auto-promote fixes it
make ci       # everything below, in order
```

`make test` reruns every test, including the cram tests dune would otherwise
replay from `_build` (it gives them a new `JIDE_TEST_RUN`). The context tests
under `test/context/` run only when `JASMIN_CONTEXT_CORPUS` names a checkout
of [libjade](https://github.com/formosa-crypto/libjade); `make context
CORPUS=<checkout>` runs just those.

`make ci` runs `version-check`, `build`, `bench-build`, `fmt`, `test`,
`oracle`, `editor-code`, `editor-vscode`, `editor-zed`, `zed-queries` and
`workflows`, and stops at the first failure. Three steps print a skip when
their input isn't set: `oracle` without `JASMINC`, `editor-code` without
`JASMIN_TM_CORPUS` (a Jasmin release tree for the TextMate corpus check) and
`zed-queries` without `JASMIN_GRAMMAR_DIR` (a `tree-sitter-jasmin` checkout).
`editor-vscode` downloads a VS Code build and never skips, unless
`JASMIN_ALLOWED_SKIPS=editor-vscode` says so.

## Fuzzing and benchmarks

`make fuzz` runs both fuzzing surfaces for `FUZZ_SECONDS` (120) with
`FUZZ_SEED` (1), over generated text and, with `CORPUS`, over that corpus
too. A failure prints the seed and case that replay it.

`make bench CORPUS=<tests directory>` measures the latency of a whole check,
a keystroke and a request on the largest files. Numbers are worth something
only on an idle machine, so `bench/run.sh` refuses to start while a build
runs and asks for `JASMIN_BENCH_NO_WINDOW=1` when no timing window is held.

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

## Nix and opam

`flake.nix` builds the server from the release tarball, and `default.nix`
does the same without flakes. There's no `flake.lock` yet: `nix flake lock`
writes one on a machine that can fetch the inputs, and that file is what to
commit.

`opam install .` from a clone builds the server against the released `jasmin`
package instead of the vendored tree. The package isn't published in any
opam repository.

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
