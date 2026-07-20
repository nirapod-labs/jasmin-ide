The end of the input ends the server cleanly: with a shutdown before it the
exit status is 0, without one it is 1, and in both cases the only bytes on
standard output are the replies to the requests that were sent.

  $ cat > with-shutdown.frames <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > {"jsonrpc":"2.0","id":2,"method":"shutdown"}
  > EOF
  $ sh frames.sh with-shutdown.frames | sh bounded.sh 20 ../../../lsp-server/native/main.exe > out.txt 2> server.err; echo "status $?"
  status 0
  $ grep -o '"jsonrpc":"2.0"' out.txt | wc -l | tr -d ' '
  2
  $ grep 'input closed' server.err
  [jasmin-lsp] input closed
  $ cat > without-shutdown.frames <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > EOF
  $ sh frames.sh without-shutdown.frames | sh bounded.sh 20 ../../../lsp-server/native/main.exe > out.txt 2> server.err; echo "status $?"
  status 1
  $ grep -o '"jsonrpc":"2.0"' out.txt | wc -l | tr -d ' '
  1
