# Changelog fragments

One file per change, named `<area>-<short-slug>.md`, with the area one of
`server`, `vscode`, `zed` or `grammar`. The release script collects the
fragments into `CHANGES.md` and removes them.

Example, `changes/server-diagnostics.md`:

```markdown
- Report diagnostics for a file that has a syntax error, for its other
  items.
```

Fragments are plain sentences, in British English, without identifiers or
tool names. A fragment says what a user sees, not how it was implemented.
