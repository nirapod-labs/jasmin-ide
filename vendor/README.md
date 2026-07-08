# Vendored Jasmin library

The language server parses and types with the Jasmin compiler library. This
directory assembles that library from a Jasmin source release, so a build
needs neither Rocq nor the dependencies of the safety checker (apron, PPL)
and does not depend on an installed `jasmin` package.

The library keeps the name the release gives it, `jasmin.jasmin`, so the
repository asks for one name and either source answers it: this tree when it
is assembled, the installed package when it is not.

| Path | Holds |
|---|---|
| `fetch.sh` | assembles the tree from a release archive |
| `jasmin.lock` | the release: upstream URL, version and SHA-256 |
| `dune` | `(vendored_dirs jasmin)` |
| `jasmin/` | the assembled library, git-ignored, never committed |

