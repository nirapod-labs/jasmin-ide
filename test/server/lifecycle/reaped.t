A session that ends without exit ends the server, and the client ends with
it. The end of the input is what ends the server, so the server is not
signalled, and the client waits for the process it started, so neither is
left running: a server that outlives its client is a process nobody owns,
holding the pipes of a client that has gone.

The wrapper records the server's pid before it becomes the server, so the
last check is on the process the client started, not on a pid the client
reported.

  $ cp record-pid.sh run-server.sh && chmod +x run-server.sh
  $ cat > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"shutdown"}
  > EOF
  $ ../../client/jide_test_client.exe --server ./run-server.sh --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0
  $ sed 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"definitionProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"textDocumentSync":1},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  2 result null
  $ grep 'input closed' server.err
  [jasmin-lsp] input closed
  $ pid=$(cat server.pid); if kill -0 "$pid" 2>/dev/null; then printf 'the server is still running\n'; kill -9 "$pid" 2>/dev/null; else printf 'the server is gone\n'; fi
  the server is gone
