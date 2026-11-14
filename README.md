# Jasmin IDE

[![License](https://img.shields.io/badge/license-MIT-blue)](LICENSE)
[![Jasmin](https://img.shields.io/badge/jasmin-2026.09.0-orange)](vendor/jasmin.lock)
[![Editors](https://img.shields.io/badge/editors-VS%20Code%20%7C%20Zed-lightgrey)](docs/user-guide.md)

## About

Jasmin IDE is editor support for the [Jasmin](https://github.com/jasmin-lang/jasmin)
language: a language server, `jasmin-lsp`, and an extension each for VS Code
and Zed.

The server parses and types your files with the Jasmin compiler's own library.
So the errors you see are the compiler's errors, at the places the compiler
reports them. Beyond diagnostics it gives you:

- go to definition, find references, hover and the document outline;
- completion of keywords, types and the names in scope, and signature help for
  function and intrinsic calls;
- rename across every file of the project that uses the name, refused with a
  reason when it would clash;
- highlighting, comments and brackets for `.jazz` and `.jinc`, which work even
  with the server stopped.

A `.jinc` file is checked in the context of the entry file that requires it.
That matters, because a file that relies on what came before it would
otherwise drown in errors that aren't real. A `jasmin.project` file sets the
architecture, the calling convention, the include paths and the entry files.

It doesn't check safety, constant-time or speculative constant-time. A clean
file here says nothing about those.

The [user guide](docs/user-guide.md) has the full description, every setting
and the known limits.

## Installing

There's no published release yet, so for now you build the server and point
your editor at it. You need opam.

If you just want a working server, here is a TL;DR:

- build it

  ```sh
  opam switch create jasmin-ide --empty
  make deps
  make build
  ```

  The first build downloads the pinned Jasmin release and checks its digest.
  The server is then `_build/install/default/bin/jasmin-lsp`.

- try it on a file, without an editor

  ```sh
  _build/install/default/bin/jasmin-lsp check path/to/file.jazz
  ```

- or install it into the switch, built against the released `jasmin` opam
  package

  ```sh
  opam install .
  ```

## Editors

Both extensions find the server themselves. Neither needs a setting when
`jasmin-lsp` is on your `PATH`, which `opam install .` gives you in that
switch.

- VS Code tries the `jasminIde.server.path` setting, then a server shipped
  inside the extension, then `PATH`. Release builds ship one VSIX per platform
  with the server inside, so installing one is all it takes.
  `Jasmin: Restart server` and `Jasmin: Show server log` are in the command
  palette.
- Zed tries the `lsp.jasmin-lsp.binary.path` setting, then `PATH`, then
  downloads the server from the release and checks its SHA-256 before running
  it. Until the first release there's nothing to download, so put the server
  on `PATH` or name it:

  ```json
  { "lsp": { "jasmin-lsp": { "binary": { "path": "/path/to/jasmin-lsp" } } } }
  ```

The setting is only for when you want a particular build, a server you've just
compiled for example.

A server understands the Jasmin of the release it was built against,
2026.09.0 here. Code that needs newer syntax needs a server built against
upstream `main`, as [development](docs/development.md) describes.

## Development

```sh
make test   # every test in the tree
make ci     # build, format check, tests and the editor checks, in order
```

[Development](docs/development.md) covers building the extensions, running the
checks and comparing the server with the compiler over a corpus.
[Release](docs/release.md) covers cutting a release. If you'd like to help,
start with [contributing](CONTRIBUTING.md).

## Getting support

Questions and bug reports go to the [issue tracker](../../issues). Security
problems go privately, as [security](SECURITY.md) describes.

## License

Jasmin IDE is free software, licensed under the [MIT license](LICENSE). The
notices that come with the TextMate grammar, the tree-sitter grammar, the
Jasmin library and GMP are in [NOTICE.md](NOTICE.md).
