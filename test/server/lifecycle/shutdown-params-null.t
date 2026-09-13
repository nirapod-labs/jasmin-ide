Shutdown with `params` set to JSON null ends the session with status 0, the
same as the absent member and the empty object: null means "no parameters",
which JSON-RPC 2.0 allows for a request, and the method takes none. Before
this was fixed the frame was dropped as `invalid packet: invalid structured
value`, no shutdown was ever answered, and the session ended with status 1.

  $ cat > session.frames <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > {"jsonrpc":"2.0","id":2,"method":"shutdown","params":null}
  > {"jsonrpc":"2.0","method":"exit"}
  > EOF
  $ sh frames.sh session.frames | sh bounded.sh 20 ../../../lsp-server/native/main.exe > out.txt 2> server.err; echo "status $?"
  status 0
  $ grep -o '"jsonrpc":"2.0"' out.txt | wc -l | tr -d ' '
  2
  $ grep -o '"result":null' out.txt | wc -l | tr -d ' '
  1
  $ grep 'shutdown requested' server.err
  [jasmin-lsp] shutdown requested
  $ grep 'malformed frame' server.err; echo "no frame was dropped"
  no frame was dropped
