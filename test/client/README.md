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

