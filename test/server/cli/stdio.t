A language client starts a server with `--stdio`: it appends that word to the
arguments it was given, to name the transport it will speak. The transport
this server has is standard input and output and no other, so the word asks
for what the server does anyway — but until it was accepted, the client got a
refusal (`jasmin-lsp: unknown option: --stdio`, status 2) and never an
`initialize` answer. This file is the session that says otherwise.

The session is the one the protocol always runs: `initialize` is answered by
this server, `shutdown` is answered with null, `exit` ends it, and the status
is 0.

  $ cat > session.frames <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > {"jsonrpc":"2.0","id":2,"method":"shutdown"}
  > {"jsonrpc":"2.0","method":"exit"}
  > EOF
  $ sh ../lifecycle/frames.sh session.frames | sh ../lifecycle/bounded.sh 20 ../../../lsp-server/native/main.exe --stdio > out.txt 2> server.err; echo "status $?"
  status 0
  $ grep -o '"jsonrpc":"2.0"' out.txt | wc -l | tr -d ' '
  2
  $ grep -o '"result":null' out.txt | wc -l | tr -d ' '
  1

The `serverInfo` is the one the binary prints, not a word this test knows in
advance: the version in it comes from `controller/version.ml`.

  $ server_version=$(sed -n 's/^let version = "\(.*\)"$/\1/p' ../../../controller/version.ml)
  $ grep -o "\"serverInfo\":{\"name\":\"jasmin-lsp\",\"version\":\"$server_version\"}" out.txt
  "serverInfo":{"name":"jasmin-lsp","version":"0.0.0"}

The log goes to standard error, which is the stream the protocol does not
use, and a served session says the three things it did:

  $ cat server.err
  [jasmin-lsp] initialized
  [jasmin-lsp] shutdown requested
  [jasmin-lsp] exiting

The word changes nothing, which is the whole contract. The same session
started with no argument at all writes the same bytes on both streams:

  $ sh ../lifecycle/frames.sh session.frames | sh ../lifecycle/bounded.sh 20 ../../../lsp-server/native/main.exe > plain.txt 2> plain.err; echo "status $?"
  status 0
  $ cmp -s out.txt plain.txt && echo "same session"
  same session
  $ cmp -s server.err plain.err && echo "same log"
  same log

`--stdio` alone is a session and not a refusal, and the difference is
visible from outside: a server whose client sends nothing ends at the end of
input with status 1 and one line on standard error, where a refused command
line is status 2 and never reaches the protocol stream.

  $ ../../../lsp-server/native/main.exe --stdio < /dev/null > out.txt 2> err.txt; echo "status $?"
  status 1
  $ wc -c < out.txt | tr -d ' '
  0
  $ cat err.txt
  [jasmin-lsp] input closed
