A `params` member of the wrong type is still a malformed frame, dropped
exactly as before: a string is not an object or an array, so the decoder
refuses it and no reply is drawn. A malformed parameter object is a different
case — the reader accepts it, because it is well formed, and the handler
answers InvalidParams. The last two frames show that the session survives
both, because neither poisons the stream: only a frame that cannot be read at
all does that.

This is the control the null-params fix must not weaken. Every frame here is
one whose reading must not change, so the recorded answers hold before and
after it alike.

  $ cat > session.frames <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > {"jsonrpc":"2.0","id":2,"method":"shutdown","params":"oops"}
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/hover","params":"oops"}
  > {"jsonrpc":"2.0","id":4,"method":"textDocument/hover","params":{"textDocument":{},"position":{"line":0,"character":0}}}
  > {"jsonrpc":"2.0","id":5,"method":"shutdown"}
  > {"jsonrpc":"2.0","method":"exit"}
  > EOF
  $ sh frames.sh session.frames | sh bounded.sh 20 ../../../lsp-server/native/main.exe > out.txt 2> server.err; echo "status $?"
  status 0
  $ grep 'malformed frame dropped' server.err
  [jasmin-lsp] malformed frame dropped: invalid packet: invalid structured value
  [jasmin-lsp] malformed frame dropped: invalid packet: invalid structured value
  $ grep -o '"id":[0-9]*' out.txt
  "id":1
  "id":4
  "id":5
  $ grep -o '"code":-32602' out.txt
  "code":-32602
  $ grep -o '"message":"[^"]*"' out.txt
  "message":"textDocument.uri is missing"
  $ grep 'shutdown requested' server.err
  [jasmin-lsp] shutdown requested
