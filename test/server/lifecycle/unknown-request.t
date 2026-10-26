An unknown request is MethodNotFound (-32601). That covers both a method the
protocol does not define and a protocol method this server does not implement:
a code lens is in the protocol and in the `lsp` package's request type, and this
server answers nothing for it. The four navigation requests and the seven
editing requests are implemented, so those are answered by their own records,
in `navigation/` and in `editing/`, rather than refused here.

  $ cat > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"no/such/method","params":{}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/codeLens","params":{"textDocument":{"uri":"file:///a.jazz"}}}
  > EOF
  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0
  $ sed 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"completionProvider":{"resolveProvider":false,"triggerCharacters":["#",":"]},"definitionProvider":true,"documentHighlightProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"renameProvider":{"prepareProvider":true},"semanticTokensProvider":{"full":true,"legend":{"tokenModifiers":["declaration","inline","export"],"tokenTypes":["namespace","type","function","parameter","variable","property","keyword"]}},"signatureHelpProvider":{"triggerCharacters":["(",","]},"textDocumentSync":1,"workspaceSymbolProvider":true},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  2 error {"code":-32601,"message":"unknown request: no/such/method"}
  3 error {"code":-32601,"message":"unsupported request: textDocument/codeLens"}
