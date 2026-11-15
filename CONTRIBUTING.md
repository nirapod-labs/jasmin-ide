# Contributing to Jasmin IDE

Thanks for your interest in Jasmin IDE. Bug reports, fixes, tests and
documentation are all welcome. This file covers how to propose a change, the
checks it has to pass and the rules the tree follows. The
[development guide](docs/development.md) has the build and every test
command, and its [layout](docs/development.md#layout) section maps the tree.

Everyone taking part is expected to follow the
[code of conduct](CODE_OF_CONDUCT.md).

## Reporting a bug

Open an issue with the server version (`jasmin-lsp --version`), your editor
and its version, and the smallest Jasmin file that shows the problem. If the
server disagrees with `jasminc` on the same file, say which `jasminc` you ran:
the compiler is the reference, and a difference from it is a bug here.

A security problem goes privately, as [SECURITY.md](SECURITY.md) describes,
not into a public issue.

## Fixing a bug or improving a feature

Go ahead and open a pull request. If an issue goes with it, link it in the
description so it's closed when the change lands.

## Adding a feature

Open an issue first and describe what you want to do. That way we can agree
the feature fits before anyone spends time on it.

## Before you send a change

```
make deps build
make ci
```

`make ci` runs the version check, the build, the formatting check, every
test, the editor extensions' checks and the workflow check, and stops at the
first failure. `make fmt` doesn't rewrite anything; `dune build @fmt
--auto-promote` does.

A change to parsing, recovery, typing, the resolver or rename should also run
the corpus comparison it touches (`make parity`, `make parity-uses`,
`make mutation` and the others in `docs/development.md`) against the compiler
of the same Jasmin version. A difference from the compiler is a bug here
until shown otherwise.

## Tests

Unit tests sit beside each library (`lang/`, `jasmin/`, `engine/`, `lsp/`,
`controller/`, `compiler/`). The cram suites under `test/server/` drive the
built server over pipes with the test client in `test/client/` and pin the
protocol: requests before `initialize`, a second `initialize`, `shutdown` and
`exit`, malformed frames, unknown methods, and every request the server
answers. The corpus comparisons under `test/parity/`, `test/intrinsic/` and
`test/mutation/` run their self-tests and negative controls in `make test`.
A fuzz failure prints the seed and case that replay it; add it to `test/` as
a case that fails without the fix.

A test that isn't ready is removed, not skipped.

## Rules the tree follows

- Every library module has an `.mli`, and its comment says what the module
  does.
- Every call into the Jasmin library is wrapped. A failure becomes a
  diagnostic, never an exception that reaches a request handler.
- A check, a parse and a project-file read return a value for any input,
  including a broken file or a huge one.
- The server writes only protocol frames to stdout and logs to stderr.
- Two checks never run at once, because the Jasmin library holds global
  state. Requests are answered in arrival order.
- A new dependency needs a reason, and is pinned in `jasmin-lsp.opam.locked`
  or `vendor/requirements.opam`.
- The root `dune` file sets the warnings; a file doesn't add its own
  suppressions.
- Text for users is British English, plain and short. Diagnostic text is the
  compiler's and isn't reworded.
- A change a user can see adds one file under `changes/`, named
  `<area>-<slug>.md` with the area `server`, `vscode`, `zed` or `grammar`,
  saying in a sentence or two what the user sees. `changes/README.md` has the
  shape.

## Commits

One commit changes one part of the tree, has one subject line, and carries a
`Signed-off-by` trailer (`git commit -s`).
