A notification whose `params` is JSON null is handled like one with no params
member: it is read, it draws no reply, and the session goes on. `exit` is the
sharpest case, because a dropped exit frame is a session that never ends, so
the file also carries `initialized`, which the server acknowledges only by not
complaining. (The server logs "params are required" for a notification that
needs parameters, and a null one is not that: it is the absent member.)

  $ cat > session.frames <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > {"jsonrpc":"2.0","method":"initialized","params":null}
  > {"jsonrpc":"2.0","id":2,"method":"shutdown","params":null}
  > {"jsonrpc":"2.0","method":"exit","params":null}
  > EOF
  $ sh frames.sh session.frames | sh bounded.sh 20 ../../../lsp-server/native/main.exe > out.txt 2> server.err; echo "status $?"
  status 0
  $ grep -o '"jsonrpc":"2.0"' out.txt | wc -l | tr -d ' '
  2
  $ grep 'exiting' server.err
  [jasmin-lsp] exiting
  $ grep 'malformed frame' server.err; echo "no frame was dropped"
  no frame was dropped
