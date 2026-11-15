# Jasmin IDE

Jasmin language support for Visual Studio Code: syntax highlighting for the
[Jasmin](https://github.com/jasmin-lang/jasmin) cryptography language, and the
Jasmin language server for files with the `.jazz` and `.jinc` extensions.

The highlighting and the language configuration work on their own. Diagnostics
and the other editor features come from `jasmin-lsp`, which is installed
separately.

## What you get

- Highlighting for Jasmin keywords, types, annotations, intrinsics, operators,
  comments and strings.
- Comment toggling and bracket matching.
- The language id `jasmin-crypto` and the scope `source.jasmin.crypto`, unchanged
  from the syntax-only releases.
- Diagnostics, navigation and the other features the language server supports,
  once it is installed.

## Installing the language server

The extension looks for the server in this order:

1. The `jasminIde.server.path` setting.
2. `server/jasmin-lsp` inside the installed extension, or
   `server/jasmin-lsp.exe` on Windows, for a platform-specific package.
3. `jasmin-lsp` on `PATH`.

When none of them names a file, the extension shows an error that names the
setting and says how to install the server. When the setting is filled in but
does not name a file, the extension reports that path instead of searching
further, because a path that has gone missing is usually a mistake worth
seeing.

The server is either inside the package or installed separately. A platform
package — the one whose file name carries a platform, such as `linux-x64` —
bundles the server for that platform, so step 2 finds it and nothing else is
needed. The universal package carries no server, so install it so that
`jasmin-lsp` is on `PATH`, or set an absolute path:

```json
"jasminIde.server.path": "/usr/local/bin/jasmin-lsp"
```

## Settings

| Setting | Default | Meaning |
|---|---|---|
| `jasminIde.server.path` | `""` | Absolute path to the server executable. Empty means "search". |
| `jasminIde.server.args` | `[]` | Extra arguments for the server. |
| `jasminIde.trace.server` | `off` | Protocol trace: `off`, `messages` or `verbose`. Written to the Jasmin IDE output channel. |
| `jasminIde.diagnostics.delay` | `250` | Delay in milliseconds before the server checks a file again after a change, from 50 to 1000. |
| `jasminIde.arch` | `x86-64` | Target architecture, used when the workspace has no `jasmin-project.json`. |
| `jasminIde.include` | `{}` | Named include paths, as a map from name to directory, used when the workspace has no `jasmin-project.json`. |

The delay, the architecture and the include map are sent in the server's
`initialize` request. The rest of the section is read by the server through
`workspace/configuration`, and the server is told when the section changes.

The protocol trace is captured while the Jasmin IDE output channel's log level
is `Trace`, using the level selector at the right of the Output panel.
`jasminIde.trace.server` chooses how much of it is written: `messages` or
`verbose`.

## Commands

- **Jasmin: Restart server** stops the server and starts it again with the
  current settings.
- **Jasmin: Show server log** opens the Jasmin IDE output channel, which holds
  the server's messages and, when tracing is on, the protocol trace.

## Version check

When the server starts, the extension compares its own version with the version
the server reports in `serverInfo` and shows one warning if they differ. A
server that reports no version is not treated as an error.

## Files from other extensions

Two other extensions also claim `.jazz` files. If you have one of them and this
extension does not highlight your files, add an association:

```json
"files.associations": { "*.jazz": "jasmin-crypto" }
```

## Development

The tests import the TypeScript sources directly, using Node's built-in type
stripping, so development needs Node 22.18 or newer.

From this directory:

```sh
pnpm install --frozen-lockfile
pnpm run validate
```

`validate` runs the manifest and grammar checks, the unit tests on Node's test
runner, `tsc --noEmit`, the esbuild bundle and a packaging check that lists the
files `vsce` would ship and then builds a `.vsix` under `.artifacts/`.

One of the unit tests starts the language server and completes the protocol's
lifecycle with it, so it needs the server built in the repository that holds
this extension. `make build` at the repository root produces it, and the test
looks in `_build/default/lsp-server/native/main.exe`; `JASMIN_LSP` names an
executable elsewhere.

`pnpm run test:vscode` is the editor's own check and is not part of `validate`:
it starts a real VS Code window, so it needs a desktop session, and the first
run downloads a VS Code build into `.vscode-test/`. That window activates the
extension, opens a `.jazz` fixture, and checks that the language client
completes the protocol lifecycle: initialize, initialized, shutdown and exit,
including the restart command's clean stop and second start. Its editing-help
checks — completion, and rename once the server advertises one — are recorded
with the capability each needed, and a check that skips has to be named in the
runner's allowlist or the run fails. The extension's output channel is the
evidence; the runner reads it back after the window has closed. `make
editor-vscode` runs the same check in the local CI; a host that cannot download
VS Code gets a printed skip only when the caller allows that skip by name
(`make editor-vscode JASMIN_ALLOWED_SKIPS=editor-vscode`), and without it the
step fails, so the log of a run cannot claim the editor half was measured when
it was not.

## Packaging

Two kinds of package are built from this directory.

A **platform package** bundles the language server, so a user installs the
extension and has everything. It is built with the server executable for that
platform:

```sh
node scripts/package-vsix.mjs darwin-arm64 ../../_build/default/lsp-server/native/main.exe
pnpm run package:vsix linux-x64 /path/to/jasmin-lsp-linux-x86_64
```

The platforms are `darwin-arm64`, `linux-x64`, `linux-arm64`, `alpine-x64` and
`alpine-arm64`. The script copies the binary into the extension directory as
`server/jasmin-lsp` with mode 0755, writes `server/SHA256SUMS` with its digest,
runs the extension's own server lookup against the directory it is about to
package, and then runs `vsce package --no-dependencies --target <platform>`. It
prints the `.vsix` path, its SHA-256 and its size, and it removes the staged
`server/` again. It refuses a platform it does not build for, a binary that is
not there, a binary without an execute bit, and a `server/` left behind by an
earlier run.

A **universal package** carries no server. It is for every platform, and it
finds `jasmin-lsp` on `PATH`, which is what a user who installed the server
through opam or from a release asset has:

```sh
node scripts/package-vsix.mjs universal
pnpm run test:package
```

`pnpm run test:package` builds the universal package and reads the archive back
to check that it holds the four files the extension needs and no `server/`
directory. It prints why it skipped and passes on a host without `vsce`.

`pnpm run package:check` is part of `validate`. It lists the files `vsce` would
ship and rejects a package that carries a source map, a source, a test, a
packaging script, `node_modules` or `.vscode-test`; it then builds the
universal package and checks that the archive has no `server/` and declares no
target platform. Any platform package already in `.artifacts/` is read back
too: the server has to be at `server/jasmin-lsp` with mode 0755, and
`server/SHA256SUMS` has to name it and record the digest of the bytes in the
archive.

`pnpm run bundle` writes `dist/extension.js` as a CommonJS bundle with `vscode`
left external, because the editor provides it. `pnpm run compile` type-checks
the sources without emitting anything. `src/package.json` marks the TypeScript
sources as ES modules so that the plain Node test runner can import the server
lookup module directly; the bundle itself stays CommonJS for the editor.

## Licence

MIT. The grammar is derived from `dsprenkels/vscode-jasmin`, copyright Daan
Sprenkels, and is credited in `NOTICE.md`.
