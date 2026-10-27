# Security policy

## What the server is

`jasmin-lsp` is a language server. An editor starts it, it speaks JSON-RPC
over standard input and output, and it stops when the editor closes the
stream.

It parses and types Jasmin source files. That is the same work `jasminc` does
on the same input. It does not compile, does not assemble, does not link and
does not run anything a file asks for. A `#` intrinsic, an annotation string or
a `require` line is data to the parser and nothing more.

The pieces that matter for a threat model:

- **It runs no user code.** There is no evaluation, no plugin loading, no
  macro expansion and no shell command. Nothing in a source file can make the
  server start a process.
- **It writes no files.** The server reads source files the editor tells it
  about, the files a `require` line names, and the project file at the
  workspace root. It writes nothing back: a result goes to the editor as a
  protocol message, and the batch checker writes to standard output only.
- **It uses no network.** The server opens no socket, listens on none and
  fetches nothing. The only channel it has is the standard streams its editor
  gave it.
- **It sends no telemetry.** Nothing is reported anywhere, ever. The log goes
  to standard error, which the editor keeps for the user.

## What is in scope

Source files may come from a repository nobody has read. A file that is
hostile, enormous, deeply nested, full of a million small items or broken in a
way nobody has thought of should be analysed, reported and survived:

- **Memory safety.** No out-of-bounds read, no use after free, no crash that
  is not a caught exception.
- **Bounded work.** A file larger than 8 MiB is refused with one diagnostic and
  not parsed. A malformed input should not make the server use time or memory
  out of proportion to its size.
- **Availability.** A failure inside one analysis is caught, becomes an
  internal diagnostic or a logged line for that document, and leaves the
  server serving other documents.
- **The protocol boundary.** No exception may leave a request handler, and
  stdout carries protocol frames only. A crafted frame must not desynchronise
  the stream or reach the editor as corrupt output.

