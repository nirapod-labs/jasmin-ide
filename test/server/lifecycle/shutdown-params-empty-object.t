Shutdown with an empty params object ends the session with status 0: the
request is answered with a null result and the server says it shut down. An
empty object is the third spelling of "no parameters".

  $ cat > session.frames <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > {"jsonrpc":"2.0","id":2,"method":"shutdown","params":{}}
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
