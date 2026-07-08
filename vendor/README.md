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

## Fetching

```
vendor/fetch.sh                          # the release named in the lock
vendor/fetch.sh <url-or-path> <sha256>   # any Jasmin source archive
```

The archive may be an `http` or `https` URL, a `file://` URL or a path on
this machine. `fetch.sh` reads the bytes once, checks the SHA-256 before
anything is unpacked, and unpacks into a private temporary directory. It
then insists on the layout upstream publishes:

- exactly one top-level directory, named `jasmin-compiler-v<version>/` (a
  locally made archive may be named `jasmin-compiler/`);
- `compiler/src`, `compiler/uint63`, `compiler/dune-project`,
  `compiler/LICENSE` and `compiler/MANIFEST` inside it.

Only what the library needs is copied out: `compiler/src` becomes
`jasmin/src`, `compiler/uint63` becomes `jasmin/uint63`, `compiler/dune-project`
becomes `jasmin/dune-project`, and the licence and manifest go to
`jasmin/LICENSE` and `jasmin/MANIFEST`. The rest of the release (the entry
points, the linter and the safety library) is dropped with the temporary
directory. An archive that does not match, or whose digest does not match,
leaves `jasmin/` as it was.

The script is idempotent. When `jasmin/` exists, its `.source-sha256` stamp
equals the requested digest and its `.tree-layout` stamp equals the shape this
version of the script writes, it reports that and stops: no download, no
unpacking, no copying. A tree assembled by an older shape is reassembled from
the archive, because two shapes are not interchangeable even when both come
from the same release bytes.

## The library's name

`fetch.sh` copies the release's dune files unchanged, and the library keeps
the name upstream gives it: `jasmin.jasmin`, with `jasmin.uint63`,
`jasmin.uint63-js` and `jasmin.uint63-native` beside it. That is the name the
released `jasmin` package installs, so one `(libraries ... jasmin.jasmin ...)`
in this repository resolves against either source:

- with the tree assembled, dune finds the library in `vendor/jasmin`, whose
  build is what the rest of this section describes;
- without it, dune finds the installed package through findlib, which is the
  opam route: `opam install .` needs the released `jasmin` package, and
  `jasmin-lsp.opam` depends on it.

A public name needs a package, and the release's own `compiler/dune-project`
declares one, `(package (name jasmin))`; `fetch.sh` copies it for that reason.
It also carries the `(using menhir 2.1)` the parser is generated with, and it
makes the assembled tree a dune project of its own. That last part is what
keeps this workspace honest: there is no `jasmin` package here, so dune
neither asks for a `jasmin.opam` at the root of this repository nor installs
a Jasmin library under the name of the released one.

Because the tree is its own project, dune builds it only when a target in this
workspace depends on `jasmin.jasmin`. `dune build @install` alone has nothing
to do until a library in the repository depends on it; every library that does
is enough, and the build log then shows
`vendor/jasmin/src/jasmin.{a,cmxa}`.

