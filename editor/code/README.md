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

## Files from other extensions

Two other extensions also claim `.jazz` files. If you have one of them and this
extension does not highlight your files, add an association:

```json
"files.associations": { "*.jazz": "jasmin-crypto" }
```

## Licence

MIT. The grammar is derived from `dsprenkels/vscode-jasmin`, copyright Daan
Sprenkels, and is credited in `NOTICE.md`.
