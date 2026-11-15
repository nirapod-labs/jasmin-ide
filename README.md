# Jasmin IDE

Jasmin IDE is a language server that provides IDE functionality for writing
[Jasmin](https://github.com/jasmin-lang/jasmin) programs. You can use it with
any editor that supports the
[Language Server Protocol](https://microsoft.github.io/language-server-protocol/),
and it comes with extensions for VS Code and Zed.

Jasmin IDE features include go-to-definition, find-all-references, hover,
completion, signature help and project-wide rename. Its diagnostics are the
Jasmin compiler's own, because it checks your code with the compiler's
library.

Internally, Jasmin IDE is structured as a set of OCaml libraries for
analysing Jasmin code. See [Layout](docs/development.md#layout) in the
development guide.

[![build](https://github.com/nirapod-labs/jasmin-ide/actions/workflows/build.yml/badge.svg)](https://github.com/nirapod-labs/jasmin-ide/actions/workflows/build.yml)

## Quick Start

See [Installing](docs/user-guide.md#installing) in the user guide.

## Documentation

If you want to **contribute** to Jasmin IDE, check out
[CONTRIBUTING.md](CONTRIBUTING.md), or see the
[development guide](docs/development.md) for how things work under the hood.

If you want to **use** Jasmin IDE with your editor of choice, check
[the user guide](docs/user-guide.md). It also covers the project file and the
known limits.

## Security and Privacy

See [SECURITY.md](SECURITY.md). The server runs no code from your files, uses
no network and sends no telemetry.

## Communication

For bugs and feature requests, use the [issue tracker](../../issues).

For questions about Jasmin, join the Formosa-Crypto Zulip:

https://formosa-crypto.zulipchat.com/

## Quick Links

* Jasmin: https://github.com/jasmin-lang/jasmin
* Jasmin documentation: https://jasmin-lang.readthedocs.io
* Changelog: [CHANGES.md](CHANGES.md)

## License

Jasmin IDE is distributed under the terms of the MIT license.

See [LICENSE](LICENSE) and [NOTICE.md](NOTICE.md) for details.
