A documentSymbol request is routed to the engine and answered with a list of
symbols, an empty list when the document has none. The engine answers from the
index's own outline, so a document with one function gives one node: its name,
the signature the index cut from the source, its kind, the range of the whole
item and the range of the name. A blank document and a document the client
never opened give `[]`, with no error for either. The request takes no
position, so the blank document is the "no item is here" case the other three
requests spell as a position in whitespace.

This test also pins that a second document's publish and reply do not disturb
the first: the publish names the document it is about.

  $ cat > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz","languageId":"jasmin-crypto","version":1,"text":"fn f() -> reg u32 {\n  reg u32 x;\n  x = 0;\n  return x;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/documentSymbol","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/blank.jazz","languageId":"jasmin-crypto","version":1,"text":"\n\n"}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/documentSymbol","params":{"textDocument":{"uri":"file:///tmp/blank.jazz"}}}
  > 
  > {"jsonrpc":"2.0","id":4,"method":"textDocument/documentSymbol","params":{"textDocument":{"uri":"file:///tmp/never-opened.jazz"}}}
  > 
  > {"jsonrpc":"2.0","id":5,"method":"shutdown"}
  > EOF

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0

  $ sed 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"completionProvider":{"resolveProvider":false,"triggerCharacters":["#",":"]},"definitionProvider":true,"documentHighlightProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"semanticTokensProvider":{"full":true,"legend":{"tokenModifiers":["declaration","inline","export"],"tokenTypes":["namespace","type","function","parameter","variable","property","keyword"]}},"signatureHelpProvider":{"triggerCharacters":["(",","]},"textDocumentSync":1,"workspaceSymbolProvider":true},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  - textDocument/publishDiagnostics {"diagnostics":[],"uri":"file:///tmp/navigation.jazz","version":1}
  2 result [{"detail":"fn f() -> reg u32","kind":12,"name":"f","range":{"end":{"character":1,"line":4},"start":{"character":0,"line":0}},"selectionRange":{"end":{"character":4,"line":0},"start":{"character":3,"line":0}}}]
  - textDocument/publishDiagnostics {"diagnostics":[],"uri":"file:///tmp/blank.jazz","version":1}
  3 result []
  4 result []
  5 result null
