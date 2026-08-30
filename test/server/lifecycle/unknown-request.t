An unknown request is MethodNotFound (-32601). That covers both a method the
protocol does not define and a protocol method this server does not implement:
completion is in the protocol and in the `lsp` package's request type, and this
server answers neither it nor anything outside the lifecycle and the four
navigation requests. The four themselves are implemented, so definition is
answered by definition.t rather than refused here.

  $ cat > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"no/such/method","params":{}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/completion","params":{"textDocument":{"uri":"file:///a.jazz"},"position":{"line":0,"character":0}}}
  > EOF
  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0
  $ sed 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"definitionProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"textDocumentSync":1},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  2 error {"code":-32601,"message":"unknown request: no/such/method"}
  3 error {"code":-32601,"message":"unsupported request: textDocument/completion"}
