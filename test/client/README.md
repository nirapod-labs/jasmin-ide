# Test client

A small client that speaks the language server protocol over the server's
standard input and output, prints one normalised line per reply, and turns
every failure into an exit code. The protocol suites drive a server with it,
so that protocol tests are cram tests (`design/ARCHITECTURE.md` section 13,
layer 2). It is test infrastructure and is never linked into the server: its
only dependencies are `yojson` and the standard library.

## Usage

```
jide-test-client --server <path> [--timeout <ms>] <session-file>
jide-test-client --fake-server <script-file> [--timeout <ms>] <session-file>
```

`--timeout` is the wait for each reply and defaults to 5000 ms. The server is
started as `<path>` with no arguments; its standard error passes through to
the client's standard error. Nothing but the reply lines is written to
standard output. Both modes take exactly one session file: the session is
what the client sends and the script is what the fake server answers with,
so one file cannot stand for both. A usage error exits 2.

## The server's lifetime

The server is the client's to end. When the session is over the client closes
the server's standard input, which is what ends a server that follows the
protocol, and then waits for the process. A server that has not ended after a
short grace period is asked to stop with SIGTERM and then killed with
SIGKILL, and the wait reaps it either way, so the client never returns while
the server it started is still running.

That matters outside `dune runtest`: dune ends the process group of an action
when the action is over, which hides a server nobody reaped, while a client
run by hand has nothing behind it to do the same. The ends of both pipes are
close-on-exec as well, so the server does not inherit a writer for its own
input and does see the end of it.

## Session files

Blocks are separated by one or more blank lines. A block whose first
non-space character is `#` is a directive; any other block is one JSON-RPC
message, sent framed as `Content-Length: N\r\n\r\n` followed by N bytes of
UTF-8. A message may be written over several lines.

```
#sleep <ms>            wait before sending the next message
#close                 close the server's standard input
#expect-timeout <ms>   no message may arrive within <ms>
```

A message with an `id` that is not `null` is a request: the client waits for
its reply. A message without one is a notification, and the client never
waits for a reply to it.

## Output

One line per message, in arrival order:

```
<id> <method-or-kind> <json>
```

The id is printed as the server sent it (a string without its quotes), or
`-` when the message carries no id. For a reply, the kind is `result` or
`error` and the JSON is the `result` or the `error` object; an error
therefore prints its code and message. For a notification sent by the server
while a reply is outstanding, the kind is the method name and the JSON is its
`params`; reading goes on until the reply itself arrives. Every JSON is
printed with object keys sorted lexicographically at every depth and no
spaces. The same session always prints the same bytes. A request sent by the
server is printed the same way and is not answered: the client is a driver
for replies, not an editor.

## Exit codes

The client exits with one of four codes, and with nothing else:

| code | meaning |
| ---- | ------- |
| 0 | every reply arrived |
| 1 | a reply did not arrive within the timeout |
| 2 | a malformed input: a bad `Content-Length`, a header that ends mid-line, a reply that is not JSON-RPC, a `Content-Length` above the body limit, a malformed session or script file, a usage error, or a reply where `#expect-timeout` expected silence |
| 3 | the server left before replying, including a frame whose body ends before its `Content-Length` bytes have arrived |

A frame whose header arrived in full and whose body then ends is the server
leaving before it replied (3), not a malformed frame (2): the header said how
much was coming and the peer stopped. A header cut off before its blank line
is still a malformed frame.

The body of one frame is capped at 8 MiB (`Jide_framing.max_body_bytes`). A
`Content-Length` above the cap is refused before a single body byte is
allocated, so a broken or hostile server cannot make the client allocate an
arbitrary buffer; the run ends with exit 2.

## The fake server

`--fake-server` answers the session from a script file with the same block
format, so the client's own framing and normalisation are tested without a
server. Each request that arrives consumes the next step of the script:

- a JSON message is sent back, framed;
- `#sleep <ms>` delays the answer, which is how a timeout is provoked;
- `#close` closes the fake server's output, which is how "the server exited
  before replying" is provoked;
- a script that runs out leaves the request unanswered.

`#expect-timeout` belongs to the client and is refused in a script.

## Layout

`jide_framing` writes and reads the frames, `jide_session` parses session and
script files, `jide_normalise` sorts keys and formats a reply line,
`jide_client` runs a session over a transport, `jide_fake` is the scripted
server, and `jide_test_client.ml` is the command line.
