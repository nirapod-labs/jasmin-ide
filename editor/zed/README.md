# Jasmin IDE for Zed

Zed support for [Jasmin](https://github.com/jasmin-lang/jasmin), the
cryptography language compiled by `jasminc`.

The extension gives you:

- recognition of `.jazz` and `.jinc` files;
- `//` and `/* */` comments, with bracket pairs that stay out of strings and
  comments once the grammar's overrides are in place;
- the Jasmin language server, when one is available: diagnostics, go to
  definition, find references, hover, completion, signature help and rename.

The grammar is the Jasmin project's upstream `tree-sitter-jasmin`, pinned by
commit, so the language is parsed by Zed as soon as the extension is
installed. The pin only ever names an upstream commit, because Zed fetches it
from the declared repository. Grammar fixes not yet upstream are tested with
`JASMIN_GRAMMAR_DIR` pointing at a checkout that has them.

## Highlighting

Zed reads highlight, bracket, outline, indent and text-object rules from query
files in the extension's own language directory, and never from the grammar's
repository. They sit in `languages/jasmin/`, written against the grammar
pinned here, and `test/queries.sh` checks them against that grammar.
`highlights.local.scm` holds the patterns for two statements the pinned
grammar doesn't have yet. Zed doesn't read it; the check runs it against a
grammar that has both nodes.

## Finding the language server

The extension starts `jasmin-lsp` and looks for it in four places, in this
order:

1. **`lsp.jasmin-lsp.binary.path` in your settings**, used exactly as written.
2. **`jasmin-lsp` on your `PATH`**, as the shell that started Zed sees it.
   This covers an `opam install jasmin-lsp` in an active switch.
3. **A copy downloaded earlier**, kept in the extension's own directory. It is
   used only if its SHA-256 still matches the digest compiled into the
   extension.
4. **A release download**, first the release whose tag is the version of the
   extension and then the latest release. Both must match the same compiled-in
   digest before the file is made executable.

A downloaded file whose digest does not match is deleted and the extension
reports which file failed, rather than running it. The digest is compiled into
the extension because the extension API in use, 0.7.0, does not report asset
digests; that arrives in 0.8.0, which stable Zed does not load.

## Pointing the extension at a local build

If you build the server yourself, name it in your settings and the extension
will use it without looking any further:

```json
{
  "lsp": {
    "jasmin-lsp": {
      "binary": {
        "path": "/home/you/.opam/jasmin-ide/bin/jasmin-lsp"
      }
    }
  }
}
```

The same table takes `arguments` (a list, passed to the server as given) and
`env` (an object of environment variables, which replace the inherited ones of
the same name).

The server is started with the shell environment of the worktree, so it finds
`jasminc`, opam and the rest of your toolchain the way a terminal would.

## Settings passed to the server

`lsp.jasmin-lsp.initialization_options` passes three things through, and
ignores the rest of the table:

```json
{
  "lsp": {
    "jasmin-lsp": {
      "initialization_options": {
        "arch": "x86-64",
        "include": { "SECP": "src/jasmin", "ARCH": "src/jasmin/x86_64" },
        "diagnostics": { "delay": 250 }
      }
    }
  }
}
```

- `arch` is the target architecture used when there is no project file. The
  default there is `x86-64`.
- `include` is the set of named include paths used when there is no project
  file, as an object of name to directory.
- `diagnostics.delay` is how long the server waits for changes to stop before
  it checks a document, in milliseconds. It accepts 50 to 1000, and 250 is the
  default.

A `jasmin-project.json` at the workspace root takes precedence over `arch` and
`include`. The table must be a JSON object; anything else is reported as a
setting error.

## Platforms

The server is released for `linux-x86_64`, `linux-aarch64` and
`macos-aarch64`. On Windows and on macOS running on an Intel processor there
is no released server, and the extension says so, naming those three
platforms. A server built on such a machine still works if you name it with
`lsp.jasmin-lsp.binary.path`.

## Downloads

This build records no release digests yet, so it downloads nothing; until a
release records them, name the server with
`lsp.jasmin-lsp.binary.path` as shown above. Once the digests are recorded,
the download step works without any setting.

## Licence

MIT. See the `LICENSE` file at the root of the repository. The grammar comes
from the Jasmin project's `tree-sitter-jasmin`, under the MIT licence.
