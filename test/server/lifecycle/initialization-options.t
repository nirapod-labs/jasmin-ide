The debounce reaches the server in one shape: nested inside the initialize
request's initializationOptions, as `diagnostics.delay`. The options below are
`test/fixtures/initialization-options.json`, the same file the extension's own
unit test compares its literal with. That file is the wire line itself, one
line of JSON and nothing else, so `cat` sends it byte for byte and no
rewriting stands between the fixture and the request. A client and the server
therefore cannot part again: were the extension to send a flat
`diagnosticsDelay`, the fixture would carry the flat name, the server would
not read it, and the log line below would vanish.

The session's first message, built from the fixture, is the request the
server receives:

  $ options=$(cat ../../fixtures/initialization-options.json)
  $ printf '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{},"initializationOptions":%s}}\n\n{"jsonrpc":"2.0","id":2,"method":"shutdown"}\n\n{"jsonrpc":"2.0","method":"exit"}\n' "$options" > nested.frames
  $ head -n 1 nested.frames
  {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{},"initializationOptions":{"arch":"x86-64","include":{"SECP":"src/jasmin"},"diagnostics":{"delay":500}}}}

The nested shape is read and applied. The server clamps it and logs the new
debounce when the value changes, which is the line below.

  $ ../../client/jide_test_client.exe --timeout 3000 --server ../../../lsp-server/native/main.exe nested.frames > nested.replies 2> nested.err; echo "status $?"
  status 0
  $ awk '{print $1, $2}' nested.replies
  1 result
  2 result
  $ grep 'diagnostics delay' nested.err
  [jasmin-lsp] diagnostics delay is now 500 ms

No other name is a second name the server takes, and the three requests below
carry the same number under the shapes a reader could accept as well: the flat
`diagnosticsDelay` the extension used to send, a flat `delay` inside the
options, and `diagnostics.delay` under the configuration section's own name,
`jasminIde`. In each the session still initializes and no change is made, so
nothing is logged: the delay stays at its default of 250 ms rather than the
number the request carries. A server that took one of the names would log the
change here. The section's name is not refused anywhere else: a
`workspace/didChangeConfiguration` notification carries it, which is a
different packet.

  $ printf '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{},"initializationOptions":{"arch":"x86-64","include":{"SECP":"src/jasmin"},"diagnosticsDelay":500}}}\n\n{"jsonrpc":"2.0","id":2,"method":"shutdown"}\n\n{"jsonrpc":"2.0","method":"exit"}\n' > flat.frames
  $ ../../client/jide_test_client.exe --timeout 3000 --server ../../../lsp-server/native/main.exe flat.frames > flat.replies 2> flat.err; echo "status $?"
  status 0
  $ awk '{print $1, $2}' flat.replies
  1 result
  2 result
  $ if grep -q 'diagnostics delay' flat.err; then echo "the flat name was read"; else echo "the flat name is not read"; fi
  the flat name is not read

  $ printf '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{},"initializationOptions":{"arch":"x86-64","include":{"SECP":"src/jasmin"},"delay":500}}}\n\n{"jsonrpc":"2.0","id":2,"method":"shutdown"}\n\n{"jsonrpc":"2.0","method":"exit"}\n' > delay.frames
  $ ../../client/jide_test_client.exe --timeout 3000 --server ../../../lsp-server/native/main.exe delay.frames > delay.replies 2> delay.err; echo "status $?"
  status 0
  $ awk '{print $1, $2}' delay.replies
  1 result
  2 result
  $ if grep -q 'diagnostics delay' delay.err; then echo "the flat delay in the options was read"; else echo "the flat delay in the options is not read"; fi
  the flat delay in the options is not read

  $ printf '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{},"initializationOptions":{"arch":"x86-64","include":{"SECP":"src/jasmin"},"jasminIde":{"diagnostics":{"delay":620}}}}}\n\n{"jsonrpc":"2.0","id":2,"method":"shutdown"}\n\n{"jsonrpc":"2.0","method":"exit"}\n' > section.frames
  $ ../../client/jide_test_client.exe --timeout 3000 --server ../../../lsp-server/native/main.exe section.frames > section.replies 2> section.err; echo "status $?"
  status 0
  $ awk '{print $1, $2}' section.replies
  1 result
  2 result
  $ if grep -q 'diagnostics delay' section.err; then echo "the section's name in the options was read"; else echo "the section's name in the options is not read"; fi
  the section's name in the options is not read
