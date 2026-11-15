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

## What is out of scope

- The Jasmin compiler library itself, which the server links. A bug there is a
  bug in Jasmin; please report it to the Jasmin project. This policy covers
  what the server does around that library: the recovery driver, the index and
  resolver, the framing and the state machine.
- Either editor and its extension host.
- A binary a user chose to run. Release files are meant to be checked against
  their digests and attestations, and that is the defence.
- Anything that needs the user to run an untrusted binary deliberately.

## Reporting

Report a suspected vulnerability privately, through GitHub's private
vulnerability reporting: the "Report a vulnerability" button on the
repository's Security tab. Include the version of the server, the Jasmin
version it was built against, the input that triggers the problem, and what
you saw.

Please do not open a public issue for a security problem. Do not send a proof
of concept to anyone else before there has been a chance to look at it.

## What to expect

The project is small and maintained in spare time. There is no paid support
and no guaranteed response time, so this file promises no deadline. What it
does promise:

- a report that arrives through the published channel is read and answered;
- a fix ships in the next release, and the release notes say what was fixed,
  in terms that do not make the problem easier to exploit before users have
  updated;
- a reporter is credited in the release notes if they want to be.

## Supported versions

Only the latest release is supported, and an older server is not patched. The
server reports its own version in `serverInfo`, and each extension carries its
own version in its manifest, so the three numbers can differ until a release
sets them to the same one. The VS Code extension warns when its number and the
server's differ.

## Release downloads

A release file is safe only if it is the file the project published. No
release file exists yet. Two checks apply once one does:

- every release file will carry a SHA-256 file beside it, and a build
  attestation that verifies against the signed tag;
- the Zed extension refuses to run a server it downloaded unless the file's
  SHA-256 equals the digest compiled into the extension. A file that does not
  match is deleted and the failure is reported, not run.

A server that is on `PATH` or named by a setting is outside both checks, and
that is the user's own choice.
