A routed request still refuses what the protocol says is not a request: a
position with a negative part cannot exist, a new name with a blank in it is
not one name, parameters that are not the request's own shape are
InvalidParams (-32602), and a rename of a document the server does not hold
is refused rather than answered with an edit that would rename nothing.

  $ cat > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{"textDocument":{"uri":"file:///tmp/never-opened.jazz"},"position":{"line":0,"character":0}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/rename","params":{"textDocument":{"uri":"file:///tmp/never-opened.jazz"},"position":{"line":0,"character":0},"newName":"y"}}
  > 
  > {"jsonrpc":"2.0","id":4,"method":"textDocument/rename","params":{"textDocument":{"uri":"file:///tmp/never-opened.jazz"},"position":{"line":0,"character":0},"newName":"two words"}}
  > 
  > {"jsonrpc":"2.0","id":5,"method":"textDocument/completion","params":{"textDocument":{"uri":"file:///tmp/never-opened.jazz"},"position":{"line":-1,"character":0}}}
  > 
  > {"jsonrpc":"2.0","id":6,"method":"textDocument/completion","params":{}}
  > 
  > {"jsonrpc":"2.0","id":7,"method":"shutdown"}
  > EOF

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0

  $ grep -v '^1 result' replies.txt | sed 's/"version":"[^"]*"/"version":"<version>"/'
  2 result null
  3 error {"code":-32803,"message":"rename: the document is not open: file:///tmp/never-opened.jazz"}
  4 error {"code":-32602,"message":"rename: newName must be one name, with nothing blank in it"}
  5 error {"code":-32602,"message":"position.line and position.character must not be negative"}
  6 error {"code":-32602,"message":"lsp/src/types.ml.CompletionParams.t_of_yojson: the following record elements were undefined: position textDocument"}
  7 result null
