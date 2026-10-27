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

## Comparing with the compiler

The compiler is the specification. These targets compare the server with it
over a corpus that isn't in this repository, usually the `compiler/tests`
directory of the Jasmin release the server is built against (a release root
works too). Each target prints a skip when a variable it needs is empty, and
fails when one is set but wrong.

| Target | Needs | Compares |
|---|---|---|
| `parity` | `CORPUS`, `JASMINC` | the verdict and first error of `jasmin-lsp check` with `jasminc -until_cstexp`, for every file on every architecture its path names |
| `parity-tree` | `CORPUS`, `JASMIN2TEX` | the parse tree of the server's driver with the compiler's, and its printing with the release's `jasmin2tex` |
| `parity-uses` | `CORPUS` | the declaration every typed variable use resolves to with the one the compiler's typing names |
| `parity-mutants` | `CORPUS`, `JASMINC` | the first error on one-token mutants of every file; `test/parity/compiler-defects.tsv` lists the mutants on which the compiler itself is wrong |
| `calls` | `CORPUS` | that every typed call has exactly one reference to its callee |
| `completion-scope` | `CORPUS`, `JASMINC` | completion with the resolver's scope, and after `#` with `jasminc -help-intrinsics` |
| `intrinsics` | `JASMINC` | the server's intrinsic table with the compiler's list |
| `intrinsic-arity` | `CORPUS`, `JASMINC` | signature help for every intrinsic with the compiler's argument counts |
| `rename` | `CORPUS`, `JASMINC` | rename and rename back, with the compiler's verdict before and after |
| `mutation` | `CORPUS` | recovery on mutated files: no hang, the outline away from the edit, at most two errors |

For example:

```
make parity CORPUS=~/src/jasmin/compiler/tests JASMINC=$(command -v jasminc)
```

`CHECKS` names a list of checks (label, path, architecture, tab separated) to
use instead of walking the corpus, for `parity`, `parity-tree` and
`parity-uses`. The programs behind the targets live in `test/parity/`,
`test/intrinsic/` and `test/mutation/`, and each prints its usage with
`--help`. Without `--jasminc`, `parity.exe` compares with the release's own
`negative.expected`, which records whole compilations, so it reports the
cases a later pass rejects as differences.

Each comparison has a negative control that plants a defect and must fail.
`make test` runs them all against small corpora the tests write. Some also run
during the corpus run itself: `parity-uses` and `intrinsic-arity` fail unless
their planted control is caught, and `rename_trip.exe --plant skip` is the
rename control.

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
