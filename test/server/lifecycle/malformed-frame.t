A malformed frame is dropped and the server keeps serving. The first body is
not JSON and the second is JSON that is not a request, a notification or a
response; neither draws a reply, and the lifecycle after them is answered
and ends with status 0.

  $ cat > session.frames <<'EOF'
  > not json
  > {}
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > {"jsonrpc":"2.0","id":2,"method":"shutdown"}
  > {"jsonrpc":"2.0","method":"exit"}
  > EOF
  $ sh frames.sh session.frames | sh bounded.sh 20 ../../../lsp-server/native/main.exe > out.txt 2> server.err; echo "status $?"
  status 0
  $ grep -o '"jsonrpc":"2.0"' out.txt | wc -l | tr -d ' '
  2
  $ grep -o 'malformed frame dropped: [a-z ]*' server.err
  malformed frame dropped: invalid json
  malformed frame dropped: invalid packet
