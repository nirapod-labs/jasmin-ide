Signature help inside an intrinsic call answers from the compiler's
instruction table for the architecture the document is analysed under. A
project file in the document's directory names arm-m4; a document with none
gets the default, x86-64. The parameters are the types the compiler's
description gives the instruction's inputs, and the active parameter is the
one the cursor is in. A name only the other architecture's table holds has
no signature.

  $ mkdir -p arm x86
  $ printf '[jasmin]\narch = arm-m4\n' > arm/jasmin.project
  $ cat > session.txt <<EOF
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file://$PWD/arm/sig.jazz","languageId":"jasmin-crypto","version":1,"text":"fn f(reg u32 a) -> reg u32 {\n  reg u32 r;\n  r = #ADD(a, a);\n  return r;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file://$PWD/x86/sig.jazz","languageId":"jasmin-crypto","version":1,"text":"fn f(reg u64 a) -> reg u64 {\n  reg u64 r;\n  r = #ADD_64(a, a);\n  return r;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file://$PWD/arm/other.jazz","languageId":"jasmin-crypto","version":1,"text":"fn f(reg u32 a) -> reg u32 {\n  reg u32 r;\n  r = #ADD_64(a, a);\n  return r;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/signatureHelp","params":{"textDocument":{"uri":"file://$PWD/arm/sig.jazz"},"position":{"line":2,"character":14}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/signatureHelp","params":{"textDocument":{"uri":"file://$PWD/x86/sig.jazz"},"position":{"line":2,"character":17}}}
  > 
  > {"jsonrpc":"2.0","id":4,"method":"textDocument/signatureHelp","params":{"textDocument":{"uri":"file://$PWD/x86/sig.jazz"},"position":{"line":2,"character":14}}}
  > 
  > {"jsonrpc":"2.0","id":5,"method":"textDocument/signatureHelp","params":{"textDocument":{"uri":"file://$PWD/arm/other.jazz"},"position":{"line":2,"character":17}}}
  > 
  > {"jsonrpc":"2.0","id":9,"method":"shutdown"}
  > EOF

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0
  $ grep -o 'signatureHelpProvider[^}]*}' replies.txt
  signatureHelpProvider":{"triggerCharacters":["(",","]}
  $ grep '^2 result' replies.txt
  2 result {"activeParameter":1,"activeSignature":0,"signatures":[{"label":"ADD(u32, u32) -> u32","parameters":[{"label":"u32"},{"label":"u32"}]}]}
  $ grep '^3 result' replies.txt
  3 result {"activeParameter":1,"activeSignature":0,"signatures":[{"label":"ADD_64(u64, u64) -> bool, bool, bool, bool, bool, u64","parameters":[{"label":"u64"},{"label":"u64"}]}]}
  $ grep '^4 result' replies.txt
  4 result {"activeParameter":0,"activeSignature":0,"signatures":[{"label":"ADD_64(u64, u64) -> bool, bool, bool, bool, bool, u64","parameters":[{"label":"u64"},{"label":"u64"}]}]}
  $ grep '^5 result' replies.txt
  5 result null
