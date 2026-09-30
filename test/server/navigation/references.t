A references request is routed to the engine and answered with a list of
locations, an empty list when there are none. The engine answers from the
resolver, so the name at request 2 gives three places: the declaration, the
write below it and the read after it, each in the uri the client opened the
document with and at its own range. A position in whitespace and a position
past the end of the file are `[]`, with no error for any of them. The
`context` member the protocol requires is read, not assumed: the last request
would be refused with -32602 if the parser made `includeDeclaration` optional.

  $ cat > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz","languageId":"jasmin-crypto","version":1,"text":"fn f() -> reg u32 {\n  reg u32 x;\n  x = 0;\n  return x;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/references","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":2,"character":2},"context":{"includeDeclaration":true}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/references","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":2,"character":1},"context":{"includeDeclaration":true}}}
  > 
  > {"jsonrpc":"2.0","id":4,"method":"textDocument/references","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":99,"character":0},"context":{"includeDeclaration":false}}}
  > 
  > {"jsonrpc":"2.0","id":5,"method":"shutdown"}
  > EOF

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0

  $ sed 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"completionProvider":{"resolveProvider":false,"triggerCharacters":["#",":"]},"definitionProvider":true,"documentHighlightProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"semanticTokensProvider":{"full":true,"legend":{"tokenModifiers":["declaration","inline","export"],"tokenTypes":["namespace","type","function","parameter","variable","property","keyword"]}},"signatureHelpProvider":{"triggerCharacters":["(",","]},"textDocumentSync":1,"workspaceSymbolProvider":true},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  - textDocument/publishDiagnostics {"diagnostics":[],"uri":"file:///tmp/navigation.jazz","version":1}
  2 result [{"range":{"end":{"character":11,"line":1},"start":{"character":10,"line":1}},"uri":"file:///tmp/navigation.jazz"},{"range":{"end":{"character":3,"line":2},"start":{"character":2,"line":2}},"uri":"file:///tmp/navigation.jazz"},{"range":{"end":{"character":10,"line":3},"start":{"character":9,"line":3}},"uri":"file:///tmp/navigation.jazz"}]
  3 result []
  4 result []
  5 result null
