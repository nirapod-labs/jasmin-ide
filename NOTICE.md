# Notices

Jasmin IDE is MIT licensed; the full text is in `LICENSE`.

## TextMate grammar

The TextMate grammar in `editor/code/syntaxes/jasmin.tmLanguage.json`, its
fixtures and its tokenizer tests come from the
[`nirapod-labs/jasmin-crypto-vscode`](https://github.com/nirapod-labs/jasmin-crypto-vscode)
project, under the MIT License, and are imported as files.

That grammar is in turn derived from
[`dsprenkels/vscode-jasmin`](https://github.com/dsprenkels/vscode-jasmin),
copyright Daan Sprenkels, under the MIT License.

## Zed grammar

The Zed extension uses `tree-sitter-jasmin` from the
[`jasmin-lang`](https://github.com/jasmin-lang/tree-sitter-jasmin) project,
under the MIT License, pinned by commit.

## GMP

Release binaries link the GNU Multiple Precision Arithmetic Library (GMP)
statically, through `zarith`. GMP is distributed under the GNU Lesser
General Public License, version 3 or later. Each release states the exact
GMP source it was built from and how to relink the binaries against another
copy, as that licence requires.

## Jasmin

The language server links the Jasmin compiler library, which is MIT
licensed. Jasmin is developed by the Jasmin team at Inria and its
contributors.
