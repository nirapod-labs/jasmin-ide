# User guide

Jasmin IDE is editor support for the [Jasmin](https://github.com/jasmin-lang/jasmin)
language: a language server, `jasmin-lsp`, and an extension each for VS Code
and Zed. The server parses and types your files with the Jasmin compiler's own
library, so its errors are the compiler's, at the compiler's places.

## What you get

- Highlighting, comments and brackets for `.jazz` and `.jinc`, from each
  editor's grammar. These work with the server stopped.
- Diagnostics for every open document: parse errors, typing errors and the
  compiler's warnings, checked again a short delay after you stop typing.
- Go to definition, find references, hover and the document outline.
- Completion of keywords, types and the names in scope, and signature help
  for function and intrinsic calls.
- Rename across every file of the project that uses the name. A rename that
  would clash with another declaration is refused, and the message says
  which one.

Workspace symbols, document highlights and semantic tokens are answered with
an empty result for now, so the editor shows nothing for them.

## Installing

No release has been published, so for now you build the server yourself and
point your editor at it. You need opam.

```
git clone <this repository>
cd jasmin-ide
opam switch create jasmin-ide --empty
make deps
make build
```

The first build downloads the pinned Jasmin release once. The server is then
at `_build/install/default/bin/jasmin-lsp` (the same file as
`_build/default/lsp-server/native/main.exe`). To try it without an editor:

```
jasmin-lsp check path/to/file.jazz
```

`check` takes `--arch NAME`, `--call-conv NAME` and any number of
`-I NAME=PATH`. It prints one line per diagnostic and a summary line per
file, and exits 0 when no file has an error, 1 when one does, 2 when the
command line is wrong.

`opam install .` from a clone builds the server against the released `jasmin`
opam package instead and installs `jasmin-lsp` into the switch.

## VS Code

The extension registers the language `jasmin-crypto` for `.jazz` and `.jinc`.
It looks for the server in `jasminIde.server.path`, then in its own
installation directory (where a platform package will put it), then on
`PATH`.

Two commands: `Jasmin: Restart server` restarts it with the current settings,
and `Jasmin: Show server log` opens the `Jasmin IDE` output channel.

| Setting | Default | Effect |
|---|---|---|
| `jasminIde.server.path` | `""` | The server to run. A path that names no file is an error. |
| `jasminIde.server.args` | `[]` | Extra arguments for the server. |
| `jasminIde.diagnostics.delay` | `250` | Milliseconds to wait after a change before checking again, from 50 to 1000. |
| `jasminIde.trace.server` | `"off"` | Protocol trace in the output channel: `off`, `messages` or `verbose`. |
| `jasminIde.arch` | `"x86-64"` | Sent to the server, which doesn't use it yet: the project file decides. |
| `jasminIde.include` | `{}` | The same, for named include paths. |

The extension warns once when its version and the server's differ. If another
extension claims `.jazz` and nothing is highlighted, add:

```json
"files.associations": { "*.jazz": "jasmin-crypto" }
```

## Zed

The extension `jasmin-ide` registers the tree-sitter grammar `jasmin` and the
language server `jasmin-lsp`. Its highlight, bracket, outline, indent and
text-object queries ship with it. This build can't download a server, so name
yours:

```json
{
  "lsp": {
    "jasmin-lsp": {
      "binary": { "path": "/home/you/.opam/jasmin-ide/bin/jasmin-lsp" },
      "initialization_options": { "diagnostics": { "delay": 250 } }
    }
  }
}
```

`binary.arguments` and `binary.env` are passed as given. Of the
initialization options only `diagnostics.delay` has an effect today; `arch`
and `include` are sent and not used. The server starts with your shell's
environment, so an opam switch or `JASMINPATH` set there is visible to it.

## The project file

A file named `jasmin.project` describes a project. For each document the
server uses the nearest one above it, so one workspace can hold several
projects. It's an INI file; every key is optional:

```ini
[jasmin]
# The target, as jasminc -arch names it.
arch = x86-64
call_conv = linux
include = LIB = src/lib
include = ARCH = src/lib/x86_64
entry = src/main.jazz
```

- `arch` is `x86-64` (the default), `arm-m4`, `armv8a` or `riscv`.
- `call_conv` is `linux` (the default) or `windows`.
- `include` binds one name for `from NAME require`, written `NAME = PATH`.
  Repeat it for each name.
- `entry` names the project's entry file. Without it every `.jazz` file under
  the project file's directory is an entry.

Paths are relative to the project file's directory. Comments are whole lines
starting with `#` or `;`. A bad key or value is left at its default and the
rest of the file is still read; these problems aren't shown in the editor yet.
The names in the `JASMINPATH` environment variable (`NAME=PATH`, separated by
`:`) are bound after the project's own, as the compiler binds them after its
`-I` options.

`jasmin-lsp check` doesn't read these keys. It uses `--arch`, `--call-conv`
and `-I`, and the project file's directory only as the place to look for
entries.

