Completion after a hash answers with the intrinsics of the architecture the
document is analysed under. A project file in the document's directory names
that architecture; a document with none gets the default, x86-64. The two
documents hold the same text, so only the project file differs. Each name
is printed as a count and by its presence, never as the whole list, and the
paths are the test's own, so nothing machine-specific is recorded.

  $ mkdir -p arm x86
  $ printf '[jasmin]\narch = arm-m4\n' > arm/jasmin.project
  $ printf 'fn f(reg u32 a) -> reg u32 {\n  reg u32 r;\n  r = #\n  return r;\n}\n' > arm/intrinsics.jazz
  $ cp arm/intrinsics.jazz x86/intrinsics.jazz
  $ cat > session.txt <<EOF
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file://$PWD/arm/intrinsics.jazz","languageId":"jasmin-crypto","version":1,"text":"fn f(reg u32 a) -> reg u32 {\n  reg u32 r;\n  r = #\n  return r;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file://$PWD/x86/intrinsics.jazz","languageId":"jasmin-crypto","version":1,"text":"fn f(reg u32 a) -> reg u32 {\n  reg u32 r;\n  r = #\n  return r;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{"textDocument":{"uri":"file://$PWD/arm/intrinsics.jazz"},"position":{"line":2,"character":7}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/completion","params":{"textDocument":{"uri":"file://$PWD/x86/intrinsics.jazz"},"position":{"line":2,"character":7}}}
  > 
  > {"jsonrpc":"2.0","id":9,"method":"shutdown"}
  > EOF

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0
  $ grep '^2 result' replies.txt | grep -o '"label":"[^"]*"' | wc -l | tr -d ' '
  72
  $ grep '^2 result' replies.txt | grep -o '"label":"CLZ"' | wc -l | tr -d ' '
  1
  $ grep '^2 result' replies.txt | grep -o '"label":"AESDEC"' | wc -l | tr -d ' '
  0
  $ grep '^2 result' replies.txt | grep -o '"label":"fn"' | wc -l | tr -d ' '
  0
  $ grep '^3 result' replies.txt | grep -o '"label":"[^"]*"' | wc -l | tr -d ' '
  167
  $ grep '^3 result' replies.txt | grep -o '"label":"AESDEC"' | wc -l | tr -d ' '
  1
  $ grep '^3 result' replies.txt | grep -o '"label":"CLZ"' | wc -l | tr -d ' '
  0
  $ grep '^3 result' replies.txt | grep -o '"label":"fn"' | wc -l | tr -d ' '
  0
