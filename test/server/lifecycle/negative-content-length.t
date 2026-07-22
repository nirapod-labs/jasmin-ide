A Content-Length of -1 is refused without a packet: nothing reaches standard
output, the valid frame written after it is not read, and the server ends
with status 1.

  $ cat > session.frames <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > EOF
  $ { printf 'Content-Length: -1\r\n\r\n'; sh frames.sh session.frames; } | sh bounded.sh 20 ../../../lsp-server/native/main.exe > out.txt 2> server.err; echo "status $?"
  status 1
  $ wc -c < out.txt | tr -d ' '
  0
  $ grep 'negative Content-Length' server.err
  [jasmin-lsp] malformed frame dropped: a negative Content-Length
