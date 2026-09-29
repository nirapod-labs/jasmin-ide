A hover request is routed to the engine and answered with a result: the hover
object, or `null` when the engine has none. The engine answers from the index,
so the name at request 2 gives the storage and type its declaration was
written with, as markdown, and the range of the name itself. A position in
whitespace and a position past the end of the file are `null`, and neither is
an error.

  $ cat > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz","languageId":"jasmin-crypto","version":1,"text":"fn f() -> reg u32 {\n  reg u32 x;\n  x = 0;\n  return x;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":2,"character":2}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":2,"character":1}}}
  > 
  > {"jsonrpc":"2.0","id":4,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":99,"character":0}}}
  > 
  > {"jsonrpc":"2.0","id":5,"method":"shutdown"}
  > EOF

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0

  $ sed 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"completionProvider":{"resolveProvider":false,"triggerCharacters":["#",":"]},"definitionProvider":true,"documentHighlightProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"semanticTokensProvider":{"full":true,"legend":{"tokenModifiers":["declaration","inline","export"],"tokenTypes":["namespace","type","function","parameter","variable","property","keyword"]}},"signatureHelpProvider":{"triggerCharacters":["(",","]},"textDocumentSync":1,"workspaceSymbolProvider":true},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  - textDocument/publishDiagnostics {"diagnostics":[],"uri":"file:///tmp/navigation.jazz","version":1}
  2 result {"contents":{"kind":"markdown","value":"```jasmin\nreg u32 x\n```"},"range":{"end":{"character":3,"line":2},"start":{"character":2,"line":2}}}
  3 result null
  4 result null
  5 result null
