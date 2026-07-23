A second initialize is an error. The first one starts the session; the
second is refused with InvalidRequest, and it leaves the session running, so
the shutdown that follows still succeeds.

  $ cat > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"shutdown"}
  > EOF
  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0
  $ sed 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"textDocumentSync":1},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  2 error {"code":-32600,"message":"the server is already initialized"}
  3 result null
