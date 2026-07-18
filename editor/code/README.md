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

## Files from other extensions

Two other extensions also claim `.jazz` files. If you have one of them and this
extension does not highlight your files, add an association:

```json
"files.associations": { "*.jazz": "jasmin-crypto" }
```

## Licence

MIT. The grammar is derived from `dsprenkels/vscode-jasmin`, copyright Daan
Sprenkels, and is credited in `NOTICE.md`.
