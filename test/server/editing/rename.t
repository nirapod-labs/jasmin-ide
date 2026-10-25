A rename is routed to the engine. A prepareRename answers the range of the name
a rename would rewrite, and null where a rename is refused: the keyword at request
5 is refused. A rename answers a WorkspaceEdit with every occurrence of the symbol
in the document, the declaration and its call at request 3. A rename the engine
refuses is an error with the engine's own reason, here the collision with the
parameter at request 4, and a new name that is not one name is InvalidParams,
which the server checks before it asks the engine. The capability is advertised
with prepareProvider, so a client offers the rename.

  $ cat > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/rename.jazz","languageId":"jasmin-crypto","version":1,"text":"fn f(reg u32 x) -> reg u32 {\n  reg u32 y;\n  y = x;\n  return y;\n}\n\nfn g(reg u32 a) -> reg u32 {\n  reg u32 r;\n  r = f(a);\n  return r;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/prepareRename","params":{"textDocument":{"uri":"file:///tmp/rename.jazz"},"position":{"line":2,"character":2}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/rename","params":{"textDocument":{"uri":"file:///tmp/rename.jazz"},"position":{"line":0,"character":3},"newName":"h"}}
  > 
  > {"jsonrpc":"2.0","id":4,"method":"textDocument/rename","params":{"textDocument":{"uri":"file:///tmp/rename.jazz"},"position":{"line":2,"character":2},"newName":"x"}}
  > 
  > {"jsonrpc":"2.0","id":5,"method":"textDocument/prepareRename","params":{"textDocument":{"uri":"file:///tmp/rename.jazz"},"position":{"line":3,"character":3}}}
  > 
  > {"jsonrpc":"2.0","id":6,"method":"textDocument/rename","params":{"textDocument":{"uri":"file:///tmp/rename.jazz"},"position":{"line":2,"character":2},"newName":"a b"}}
  > 
  > {"jsonrpc":"2.0","id":7,"method":"shutdown"}
  > EOF

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0

  $ sed 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"completionProvider":{"resolveProvider":false,"triggerCharacters":["#",":"]},"definitionProvider":true,"documentHighlightProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"renameProvider":{"prepareProvider":true},"semanticTokensProvider":{"full":true,"legend":{"tokenModifiers":["declaration","inline","export"],"tokenTypes":["namespace","type","function","parameter","variable","property","keyword"]}},"signatureHelpProvider":{"triggerCharacters":["(",","]},"textDocumentSync":1,"workspaceSymbolProvider":true},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  - textDocument/publishDiagnostics {"diagnostics":[],"uri":"file:///tmp/rename.jazz","version":1}
  2 result {"end":{"character":3,"line":2},"start":{"character":2,"line":2}}
  3 result {"changes":{"file:///tmp/rename.jazz":[{"newText":"h","range":{"end":{"character":4,"line":0},"start":{"character":3,"line":0}}},{"newText":"h","range":{"end":{"character":7,"line":8},"start":{"character":6,"line":8}}}]}}
  4 error {"code":-32803,"message":"x already names a different declaration (x in /tmp/rename.jazz)"}
  5 result null
  6 error {"code":-32602,"message":"rename: newName must be one name, with nothing blank in it"}
  7 result null
