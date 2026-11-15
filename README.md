# Jasmin IDE

[![build](https://github.com/nirapod-labs/jasmin-ide/actions/workflows/build.yml/badge.svg)](https://github.com/nirapod-labs/jasmin-ide/actions/workflows/build.yml)

Jasmin IDE is editor support for [Jasmin](https://github.com/jasmin-lang/jasmin),
a language for writing high-assurance and high-speed cryptography. At its core
is `jasmin-lsp`, a server for the
[Language Server Protocol](https://microsoft.github.io/language-server-protocol/),
so it should work with any editor that speaks LSP. VS Code and Zed get an
extension each.

The server parses and types your files with the Jasmin compiler's own library,
so the errors you see are the compiler's errors, at the places the compiler
reports them. On top of that it does go to definition, find references, hover,
the document outline, completion, signature help, and rename across every file
of a project. Highlighting for `.jazz` and `.jinc` comes from each editor's
grammar and works with the server stopped.

It doesn't check safety, constant-time or speculative constant-time. A clean
file here says nothing about those.

Internally the server is a small set of OCaml libraries: positions and
diagnostics, an adapter over the Jasmin library, the analysis engine, the LSP
framing and the request handlers. See [the layout](docs/development.md#layout)
in the development guide.

## Quick start

There's no published release yet, so you build the server yourself. You need
opam.

```sh
opam switch create jasmin-ide --empty
make deps
make build
```

The server is then `_build/install/default/bin/jasmin-lsp`. It understands the
Jasmin of the release it was built against, 2026.09.0 for now. The
[user guide](docs/user-guide.md#installing) covers the rest, including how
each editor finds the server.

## Documentation

If you want to **use** Jasmin IDE, read the [user guide](docs/user-guide.md).
It describes every feature and setting, the `jasmin.project` file, and the
known limits.

If you want to **contribute**, start with [CONTRIBUTING.md](CONTRIBUTING.md).
The [development guide](docs/development.md) covers building, testing and
comparing the server with the compiler over a corpus, and
[docs/release.md](docs/release.md) covers cutting a release.

## Security and privacy

The server runs no code from your files, writes no files, opens no network
connection and sends no telemetry. [SECURITY.md](SECURITY.md) has the details
and says how to report a vulnerability privately.

## Communication

Bug reports and feature requests go to the [issue tracker](../../issues).

For questions about Jasmin itself, and about high-assurance cryptography with
Jasmin and EasyCrypt, there's the
[Formosa-Crypto Zulip](https://formosa-crypto.zulipchat.com/).

Everyone taking part is expected to follow the
[code of conduct](CODE_OF_CONDUCT.md).

## Quick links

* Jasmin: https://github.com/jasmin-lang/jasmin
* Jasmin documentation: https://jasmin-lang.readthedocs.io
* Changelog: [CHANGES.md](CHANGES.md)

## License

Jasmin IDE is distributed under the terms of the [MIT license](LICENSE). The
notices for the TextMate grammar, the tree-sitter grammar, the Jasmin library
and GMP are in [NOTICE.md](NOTICE.md).
