A request before initialize is refused with ServerNotInitialized (-32002),
and that holds for every request, shutdown included. The refused requests
leave the session uninitialized, so a later initialize still starts it.

  $ cat > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///a.jazz"},"position":{"line":0,"character":0}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"shutdown"}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"initialize","params":{"capabilities":{}}}
  > EOF
  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0
  $ sed 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 error {"code":-32002,"message":"the server is not initialized"}
  2 error {"code":-32002,"message":"the server is not initialized"}
  3 result {"capabilities":{"completionProvider":{"resolveProvider":false,"triggerCharacters":["#",":"]},"definitionProvider":true,"documentHighlightProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"renameProvider":{"prepareProvider":true},"signatureHelpProvider":{"triggerCharacters":["(",","]},"textDocumentSync":1},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
