An unknown notification is logged and ignored: it draws no reply, and the
requests after it are still answered.

  $ cat > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"no/such/notification","params":{}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"shutdown"}
  > 
  > {"jsonrpc":"2.0","method":"exit"}
  > EOF
  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0
  $ sed 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"definitionProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"textDocumentSync":1},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  2 result null
  $ grep 'unknown notification' server.err
  [jasmin-lsp] unknown notification: no/such/notification
