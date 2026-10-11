A hover request is routed to the engine and answered with a result: the hover
object, or `null` when the engine has none. The engine answers from the index,
so the name at request 2 gives the storage and type its declaration was
written with, as markdown, and the range of the name itself. A position in
whitespace and a position past the end of the file are `null`, and neither is
an error.

  $ cat > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz","languageId":"jasmin-crypto","version":1,"text":"fn f() -> reg u32 {\n  reg u32 x;\n  x = 0;\n  return x;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":2,"character":2}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":2,"character":1}}}
  > 
  > {"jsonrpc":"2.0","id":4,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":99,"character":0}}}
  > 
  > {"jsonrpc":"2.0","id":5,"method":"shutdown"}
  > EOF

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0

  $ sed 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"completionProvider":{"resolveProvider":false,"triggerCharacters":["#",":"]},"definitionProvider":true,"documentHighlightProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"semanticTokensProvider":{"full":true,"legend":{"tokenModifiers":["declaration","inline","export"],"tokenTypes":["namespace","type","function","parameter","variable","property","keyword"]}},"signatureHelpProvider":{"triggerCharacters":["(",","]},"textDocumentSync":1,"workspaceSymbolProvider":true},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  - textDocument/publishDiagnostics {"diagnostics":[],"uri":"file:///tmp/navigation.jazz","version":1}
  2 result {"contents":{"kind":"markdown","value":"```jasmin\nreg u32 x\n```"},"range":{"end":{"character":3,"line":2},"start":{"character":2,"line":2}}}
  3 result null
  4 result null
  5 result null

The same question across a `require` and across a namespace. The document is
the one `definition.t` describes: it is opened at `fixtures/entry.jazz`, it
requires `fixtures/lib.jinc`, and it declares `namespace A`, whose `f` is
named by the qualified `A::f` in `caller` and by the unqualified `f(x)` in
`twice`. Request 2 asks the function the `require` brought in: the answer is
the signature the required file writes, cut from its own source, and the range
is the use here. Requests 3 and 4 ask the one function `f` from outside the
namespace and from inside it; both show the compiler's own signature for it,
`fn A::f(u32) -> u32`, which is the qualified name and the types the compiler
typed it with, and each range is the name that was asked — the whole `A::f`
in the qualified case. Request 5 asks the parameter `x` where `f` returns it,
which shows the storage and the type it was declared with. Request 6 asks the
namespace itself.

The directory the test runs in is dune's sandbox, whose name changes on every
run, so the record prints it as `$ROOT`; every other byte is the server's own.

  $ uri="file://$PWD/fixtures/entry.jazz"
  $ sed "s|@URI@|$uri|g" > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"@URI@","languageId":"jasmin-crypto","version":1,"text":"require \"lib.jinc\"\n\nnamespace A {\n  fn f(reg u32 x) -> reg u32 {\n    return x;\n  }\n\n  fn twice(reg u32 x) -> reg u32 {\n    reg u32 r;\n    r = f(x);\n    return r;\n  }\n}\n\nfn caller(reg u32 a) -> reg u32 {\n  reg u32 r;\n  r = A::f(a);\n  r = lib_f(r);\n  return r;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/hover","params":{"textDocument":{"uri":"@URI@"},"position":{"line":17,"character":6}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/hover","params":{"textDocument":{"uri":"@URI@"},"position":{"line":16,"character":9}}}
  > 
  > {"jsonrpc":"2.0","id":4,"method":"textDocument/hover","params":{"textDocument":{"uri":"@URI@"},"position":{"line":9,"character":8}}}
  > 
  > {"jsonrpc":"2.0","id":5,"method":"textDocument/hover","params":{"textDocument":{"uri":"@URI@"},"position":{"line":4,"character":11}}}
  > 
  > {"jsonrpc":"2.0","id":6,"method":"textDocument/hover","params":{"textDocument":{"uri":"@URI@"},"position":{"line":2,"character":10}}}
  > 
  > {"jsonrpc":"2.0","id":7,"method":"shutdown"}
  > EOF

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0

  $ sed -e "s|$PWD|\$ROOT|g" -e 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"completionProvider":{"resolveProvider":false,"triggerCharacters":["#",":"]},"definitionProvider":true,"documentHighlightProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"semanticTokensProvider":{"full":true,"legend":{"tokenModifiers":["declaration","inline","export"],"tokenTypes":["namespace","type","function","parameter","variable","property","keyword"]}},"signatureHelpProvider":{"triggerCharacters":["(",","]},"textDocumentSync":1,"workspaceSymbolProvider":true},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  - textDocument/publishDiagnostics {"diagnostics":[],"uri":"file://$ROOT/fixtures/entry.jazz","version":1}
  2 result {"contents":{"kind":"markdown","value":"```jasmin\nfn lib_f(reg u32 x) -> reg u32\n```"},"range":{"end":{"character":11,"line":17},"start":{"character":6,"line":17}}}
  3 result {"contents":{"kind":"markdown","value":"```jasmin\nfn A::f(u32) -> u32\n```"},"range":{"end":{"character":10,"line":16},"start":{"character":6,"line":16}}}
  4 result {"contents":{"kind":"markdown","value":"```jasmin\nfn A::f(u32) -> u32\n```"},"range":{"end":{"character":9,"line":9},"start":{"character":8,"line":9}}}
  5 result {"contents":{"kind":"markdown","value":"```jasmin\nreg u32 x\n```"},"range":{"end":{"character":12,"line":4},"start":{"character":11,"line":4}}}
  6 result {"contents":{"kind":"markdown","value":"```jasmin\nnamespace A\n```"},"range":{"end":{"character":11,"line":2},"start":{"character":10,"line":2}}}
  7 result null
