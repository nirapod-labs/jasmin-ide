# Security policy

## Reporting a vulnerability

Please report a suspected vulnerability privately, through GitHub's private
vulnerability reporting: the "Report a vulnerability" button on the
repository's Security tab. Don't open a public issue for it.

Include:

- the server version (`jasmin-lsp --version`), which also names the Jasmin
  version it was built against;
- the input that triggers the problem;
- what you saw, and what you expected.

Please don't share a proof of concept with anyone else until there's been a
chance to look at it.

## What to expect

The project is small and maintained in spare time. There's no paid support and
no guaranteed response time, so this file promises no deadline. It does
promise that:

- a report sent through the channel above is read and answered;
- a fix ships in the next release, and the release notes say what was fixed
  without making the problem easier to exploit before users have updated;
- the reporter is credited in the release notes, if they want to be.

## Supported versions

Only the latest release is supported. An older server isn't patched.

## What the server does

`jasmin-lsp` is a language server. An editor starts it, it speaks JSON-RPC over
standard input and output, and it stops when the editor closes the stream. It
parses and types Jasmin source files, the same work `jasminc` does on the same
input. It doesn't compile, assemble, link or run anything a file asks for. A
`#` intrinsic, an annotation string or a `require` line is data to the parser
and nothing more.

- **It runs no user code.** No evaluation, no plugin loading, no macro
  expansion and no shell commands. Nothing in a source file can make the server
  start a process.
- **It writes no files.** It reads the files the editor tells it about, the
  files a `require` line names, and the project file at the workspace root.
  Results go to the editor as protocol messages, and the batch checker writes
  to standard output only.
- **It uses no network.** It opens no socket, listens on none and fetches
  nothing.
- **It sends no telemetry.** Its log goes to standard error, which the editor
  keeps for you.

## In scope

Source files may come from a repository nobody has read. A file that is
hostile, enormous, deeply nested or broken in a way nobody has thought of
should be analysed, reported and survived. So these count as vulnerabilities:

- **Memory safety.** An out-of-bounds read, a use after free, or a crash that
  isn't a caught exception.
- **Unbounded work.** A file larger than 8 MiB is refused with one diagnostic
  and not parsed. A smaller malformed input shouldn't make the server use time
  or memory out of proportion to its size.
- **Availability.** A failure inside one analysis should become a diagnostic
  or a log line for that document and leave the server serving the others.
- **The protocol boundary.** No exception may leave a request handler, and
  stdout carries protocol frames only. A crafted frame mustn't desynchronise
  the stream or reach the editor as corrupt output.

## Out of scope

- The Jasmin compiler library, which the server links. A bug there is a bug in
  Jasmin; please report it to the [Jasmin project](https://github.com/jasmin-lang/jasmin).
  This policy covers what the server does around that library.
- The editors and their extension hosts.
- A binary someone chose to run without checking it.

## Release downloads

No release has been published yet. Once one is:

- every release file carries a SHA-256 file beside it, and a build attestation
  that verifies against the signed tag;
- the Zed extension refuses to run a server it downloaded unless the file's
  SHA-256 equals the digest compiled into the extension. A file that doesn't
  match is deleted and the failure is reported.

A server on `PATH` or named by a setting is outside both checks. That's the
user's own choice.
