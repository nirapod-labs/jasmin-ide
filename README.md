# Jasmin IDE

Editor support for the [Jasmin](https://github.com/jasmin-lang/jasmin)
language in VS Code and Zed: a language server, `jasmin-lsp`, and an extension
for each editor.

The server parses and types a file with the Jasmin compiler library it was
built against, so its diagnostics are the compiler's own messages at the
compiler's own places. Beyond diagnostics it answers go to definition,
references, hover, the outline, completion, signature help and rename.

```
opam switch create jasmin-ide --empty
make deps
make build
```

The server is then `_build/install/default/bin/jasmin-lsp`. Point
`jasminIde.server.path` in VS Code, or `lsp.jasmin-lsp.binary.path` in Zed, at
it.

- [docs/user-guide.md](docs/user-guide.md): installing, the editor settings,
  the project file and the known limits.
- [docs/development.md](docs/development.md): building, testing, and comparing
  the server with the compiler over a corpus.
- [docs/release.md](docs/release.md): cutting a release.
- [CONTRIBUTING.md](CONTRIBUTING.md) and [SECURITY.md](SECURITY.md).

The server is built against the Jasmin release pinned in `vendor/jasmin.lock`
(2026.09.0). It can also be built against upstream `main`, which
`docs/development.md` describes.

