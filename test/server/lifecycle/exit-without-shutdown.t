Exit without shutdown ends the session with status 1, after the initialize
reply and nothing more.

  $ cat > session.frames <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > {"jsonrpc":"2.0","method":"exit"}
  > EOF
  $ sh frames.sh session.frames | sh bounded.sh 20 ../../../lsp-server/native/main.exe > out.txt 2> server.err; echo "status $?"
  status 1
  $ grep -o '"jsonrpc":"2.0"' out.txt | wc -l | tr -d ' '
  1
  $ grep 'exiting' server.err
  [jasmin-lsp] exiting
