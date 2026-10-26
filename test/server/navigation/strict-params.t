A navigation request whose parameters do not parse is InvalidParams (-32602),
not MethodNotFound and not a request answered with a default: the server
understood the method and refused the call. The texts are the parser's own and
say which member failed, because that is what a client shows its user.

A refused request costs nothing: no check runs (no diagnostics are published
before it) and the session keeps serving, which the last two requests show.
The six refusals ahead of them parse nothing, so the publish that precedes
request 7 is the check that request runs, and request 7 answers the
declaration the others did not get to ask about.

  $ cat > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz","languageId":"jasmin-crypto","version":1,"text":"fn f() -> reg u32 {\n  reg u32 x;\n  x = 0;\n  return x;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/references","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":2,"character":2}}}
  > 
  > {"jsonrpc":"2.0","id":4,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":"2","character":2}}}
  > 
  > {"jsonrpc":"2.0","id":5,"method":"textDocument/documentSymbol","params":{}}
  > 
  > {"jsonrpc":"2.0","id":6,"method":"textDocument/definition"}
  > 
  > {"jsonrpc":"2.0","id":7,"method":"textDocument/definition","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":2,"character":2}}}
  > 
  > {"jsonrpc":"2.0","id":8,"method":"shutdown"}
  > EOF

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0

  $ sed 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"completionProvider":{"resolveProvider":false,"triggerCharacters":["#",":"]},"definitionProvider":true,"documentHighlightProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"renameProvider":{"prepareProvider":true},"semanticTokensProvider":{"full":true,"legend":{"tokenModifiers":["declaration","inline","export"],"tokenTypes":["namespace","type","function","parameter","variable","property","keyword"]}},"signatureHelpProvider":{"triggerCharacters":["(",","]},"textDocumentSync":1,"workspaceSymbolProvider":true},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  2 error {"code":-32602,"message":"position is missing"}
  3 error {"code":-32602,"message":"context is missing"}
  4 error {"code":-32602,"message":"position.line must be an integer"}
  5 error {"code":-32602,"message":"textDocument is missing"}
  6 error {"code":-32602,"message":"params must be an object"}
  - textDocument/publishDiagnostics {"diagnostics":[],"uri":"file:///tmp/navigation.jazz","version":1}
  7 result [{"range":{"end":{"character":11,"line":1},"start":{"character":10,"line":1}},"uri":"file:///tmp/navigation.jazz"}]
  8 result null
