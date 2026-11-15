The seven editing requests are routed to the engine's editing features. Two
of them answer from the snapshot the last check left — completion with the
keywords, the types and the names in scope, signature help with the callee
and the argument the offset is in — and neither costs a check of its own.
The other five are still answered with the empty answer their request has in
the protocol: `null` for prepare rename and semantic tokens, an empty list
for workspace symbols and document highlights. No request is an error, and
none is MethodNotFound, which is what the same session recorded before the
routing landed.

Rename is the one request with no empty answer in the protocol. An empty
edit would tell the client that the rename succeeded and changed nothing, so
a rename the engine will not make is refused with RequestFailed (-32803) and
the engine's own reason instead. The engine has no rename yet, so the reason
here is that it has none.

The document is opened once and every request asks about it, so each answer
is about a document the server holds: the empty answer, not the
never-opened one.

  $ cat > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/editing.jazz","languageId":"jasmin-crypto","version":1,"text":"fn f() -> reg u32 {\n  reg u32 x;\n  x = 0;\n  return x;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{"textDocument":{"uri":"file:///tmp/editing.jazz"},"position":{"line":2,"character":2}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/signatureHelp","params":{"textDocument":{"uri":"file:///tmp/editing.jazz"},"position":{"line":2,"character":2}}}
  > 
  > {"jsonrpc":"2.0","id":4,"method":"textDocument/prepareRename","params":{"textDocument":{"uri":"file:///tmp/editing.jazz"},"position":{"line":2,"character":2}}}
  > 
  > {"jsonrpc":"2.0","id":5,"method":"textDocument/rename","params":{"textDocument":{"uri":"file:///tmp/editing.jazz"},"position":{"line":2,"character":2},"newName":"y"}}
  > 
  > {"jsonrpc":"2.0","id":6,"method":"workspace/symbol","params":{"query":""}}
  > 
  > {"jsonrpc":"2.0","id":7,"method":"textDocument/documentHighlight","params":{"textDocument":{"uri":"file:///tmp/editing.jazz"},"position":{"line":2,"character":2}}}
  > 
  > {"jsonrpc":"2.0","id":8,"method":"textDocument/semanticTokens/full","params":{"textDocument":{"uri":"file:///tmp/editing.jazz"}}}
  > 
  > {"jsonrpc":"2.0","id":9,"method":"shutdown"}
  > EOF

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0

The initialize result is asserted on its own in `lifecycle/` and in the
controller's unit tests, so only the requests' replies are printed here.

  $ grep -v '^1 result' replies.txt | sed 's/"version":"[^"]*"/"version":"<version>"/'
  - textDocument/publishDiagnostics {"diagnostics":[],"uri":"file:///tmp/editing.jazz","version":1}
  2 result [{"kind":14,"label":"ArrayInit"},{"kind":14,"label":"assert"},{"kind":14,"label":"bool"},{"kind":14,"label":"const"},{"kind":14,"label":"downto"},{"kind":14,"label":"else"},{"kind":14,"label":"exec"},{"kind":14,"label":"export"},{"kind":14,"label":"false"},{"kind":14,"label":"fn"},{"kind":14,"label":"for"},{"kind":14,"label":"from"},{"kind":14,"label":"global"},{"kind":14,"label":"if"},{"kind":14,"label":"inline"},{"kind":14,"label":"int"},{"kind":14,"label":"mut"},{"kind":14,"label":"namespace"},{"kind":14,"label":"param"},{"kind":14,"label":"ptr"},{"kind":14,"label":"reg"},{"kind":14,"label":"require"},{"kind":14,"label":"return"},{"kind":14,"label":"si128"},{"kind":14,"label":"si16"},{"kind":14,"label":"si256"},{"kind":14,"label":"si32"},{"kind":14,"label":"si64"},{"kind":14,"label":"si8"},{"kind":14,"label":"sint"},{"kind":14,"label":"stack"},{"kind":14,"label":"to"},{"kind":14,"label":"true"},{"kind":14,"label":"type"},{"kind":14,"label":"u128"},{"kind":14,"label":"u16"},{"kind":14,"label":"u256"},{"kind":14,"label":"u32"},{"kind":14,"label":"u64"},{"kind":14,"label":"u8"},{"kind":14,"label":"ui128"},{"kind":14,"label":"ui16"},{"kind":14,"label":"ui256"},{"kind":14,"label":"ui32"},{"kind":14,"label":"ui64"},{"kind":14,"label":"ui8"},{"kind":14,"label":"uint"},{"kind":14,"label":"while"},{"detail":"reg u32","kind":6,"label":"x"}]
  3 result null
  4 result {"end":{"character":3,"line":2},"start":{"character":2,"line":2}}
  5 result {"changes":{"file:///tmp/editing.jazz":[{"newText":"y","range":{"end":{"character":11,"line":1},"start":{"character":10,"line":1}}},{"newText":"y","range":{"end":{"character":3,"line":2},"start":{"character":2,"line":2}}},{"newText":"y","range":{"end":{"character":10,"line":3},"start":{"character":9,"line":3}}}]}}
  6 result []
  7 result [{"kind":1,"range":{"end":{"character":11,"line":1},"start":{"character":10,"line":1}}},{"kind":1,"range":{"end":{"character":3,"line":2},"start":{"character":2,"line":2}}},{"kind":1,"range":{"end":{"character":10,"line":3},"start":{"character":9,"line":3}}}]
  8 result null
  9 result null
