# Development

How to build the tree, run its tests, and compare the server with the Jasmin
compiler over a corpus. Every command runs from the repository root.

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
