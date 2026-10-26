A references request is routed to the engine and answered with a list of
locations, an empty list when there are none. The engine answers from the
resolver, so the name at request 2 gives three places: the declaration, the
write below it and the read after it, each in the uri the client opened the
document with and at its own range. A position in whitespace and a position
past the end of the file are `[]`, with no error for any of them. The
`context` member the protocol requires is read, not assumed: the last request
would be refused with -32602 if the parser made `includeDeclaration` optional.

  $ cat > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz","languageId":"jasmin-crypto","version":1,"text":"fn f() -> reg u32 {\n  reg u32 x;\n  x = 0;\n  return x;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/references","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":2,"character":2},"context":{"includeDeclaration":true}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/references","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":2,"character":1},"context":{"includeDeclaration":true}}}
  > 
  > {"jsonrpc":"2.0","id":4,"method":"textDocument/references","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":99,"character":0},"context":{"includeDeclaration":false}}}
  > 
  > {"jsonrpc":"2.0","id":5,"method":"shutdown"}
  > EOF

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0

  $ sed 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"completionProvider":{"resolveProvider":false,"triggerCharacters":["#",":"]},"definitionProvider":true,"documentHighlightProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"renameProvider":{"prepareProvider":true},"semanticTokensProvider":{"full":true,"legend":{"tokenModifiers":["declaration","inline","export"],"tokenTypes":["namespace","type","function","parameter","variable","property","keyword"]}},"signatureHelpProvider":{"triggerCharacters":["(",","]},"textDocumentSync":1,"workspaceSymbolProvider":true},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  - textDocument/publishDiagnostics {"diagnostics":[],"uri":"file:///tmp/navigation.jazz","version":1}
  2 result [{"range":{"end":{"character":11,"line":1},"start":{"character":10,"line":1}},"uri":"file:///tmp/navigation.jazz"},{"range":{"end":{"character":3,"line":2},"start":{"character":2,"line":2}},"uri":"file:///tmp/navigation.jazz"},{"range":{"end":{"character":10,"line":3},"start":{"character":9,"line":3}},"uri":"file:///tmp/navigation.jazz"}]
  3 result []
  4 result []
  5 result null

The same question across a `require` and across a namespace. The document is
the one `definition.t` describes: it is opened at `fixtures/entry.jazz`, it
requires `fixtures/lib.jinc`, and it declares `namespace A`, whose `f` is
named by the qualified `A::f` in `caller` and by the unqualified `f(x)` in
`twice`. Request 2 asks `lib_f` with the declaration included, and the answer
is one list of two places in two files: the use here, then the declaration in
the required file, because the answer is the entry first and the files it
required after it. Request 3 asks the same name without the declaration and
gets only the use, which is what `includeDeclaration` selects. Requests 4 and
5 ask the one declaration of `f`, from outside the namespace and from inside
it, and both give the same three places: the declaration, the unqualified use
inside `twice`, and the qualified use, whose place is its whole `A::f` range.
Request 6 asks the file name inside the `require` itself, which names a file
and declares nothing, so the answer is the empty list.

The directory the test runs in is dune's sandbox, whose name changes on every
run, so the record prints it as `$ROOT`; every other byte is the server's own.

  $ uri="file://$PWD/fixtures/entry.jazz"
  $ sed "s|@URI@|$uri|g" > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"@URI@","languageId":"jasmin-crypto","version":1,"text":"require \"lib.jinc\"\n\nnamespace A {\n  fn f(reg u32 x) -> reg u32 {\n    return x;\n  }\n\n  fn twice(reg u32 x) -> reg u32 {\n    reg u32 r;\n    r = f(x);\n    return r;\n  }\n}\n\nfn caller(reg u32 a) -> reg u32 {\n  reg u32 r;\n  r = A::f(a);\n  r = lib_f(r);\n  return r;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/references","params":{"textDocument":{"uri":"@URI@"},"position":{"line":17,"character":6},"context":{"includeDeclaration":true}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/references","params":{"textDocument":{"uri":"@URI@"},"position":{"line":17,"character":6},"context":{"includeDeclaration":false}}}
  > 
  > {"jsonrpc":"2.0","id":4,"method":"textDocument/references","params":{"textDocument":{"uri":"@URI@"},"position":{"line":16,"character":9},"context":{"includeDeclaration":true}}}
  > 
  > {"jsonrpc":"2.0","id":5,"method":"textDocument/references","params":{"textDocument":{"uri":"@URI@"},"position":{"line":9,"character":8},"context":{"includeDeclaration":true}}}
  > 
  > {"jsonrpc":"2.0","id":6,"method":"textDocument/references","params":{"textDocument":{"uri":"@URI@"},"position":{"line":0,"character":9},"context":{"includeDeclaration":true}}}
  > 
  > {"jsonrpc":"2.0","id":7,"method":"shutdown"}
  > EOF

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0

  $ sed -e "s|$PWD|\$ROOT|g" -e 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"completionProvider":{"resolveProvider":false,"triggerCharacters":["#",":"]},"definitionProvider":true,"documentHighlightProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"renameProvider":{"prepareProvider":true},"semanticTokensProvider":{"full":true,"legend":{"tokenModifiers":["declaration","inline","export"],"tokenTypes":["namespace","type","function","parameter","variable","property","keyword"]}},"signatureHelpProvider":{"triggerCharacters":["(",","]},"textDocumentSync":1,"workspaceSymbolProvider":true},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  - textDocument/publishDiagnostics {"diagnostics":[],"uri":"file://$ROOT/fixtures/entry.jazz","version":1}
  2 result [{"range":{"end":{"character":11,"line":17},"start":{"character":6,"line":17}},"uri":"file://$ROOT/fixtures/entry.jazz"},{"range":{"end":{"character":8,"line":0},"start":{"character":3,"line":0}},"uri":"file://$ROOT/fixtures/lib.jinc"}]
  3 result [{"range":{"end":{"character":11,"line":17},"start":{"character":6,"line":17}},"uri":"file://$ROOT/fixtures/entry.jazz"}]
  4 result [{"range":{"end":{"character":6,"line":3},"start":{"character":5,"line":3}},"uri":"file://$ROOT/fixtures/entry.jazz"},{"range":{"end":{"character":9,"line":9},"start":{"character":8,"line":9}},"uri":"file://$ROOT/fixtures/entry.jazz"},{"range":{"end":{"character":10,"line":16},"start":{"character":6,"line":16}},"uri":"file://$ROOT/fixtures/entry.jazz"}]
  5 result [{"range":{"end":{"character":6,"line":3},"start":{"character":5,"line":3}},"uri":"file://$ROOT/fixtures/entry.jazz"},{"range":{"end":{"character":9,"line":9},"start":{"character":8,"line":9}},"uri":"file://$ROOT/fixtures/entry.jazz"},{"range":{"end":{"character":10,"line":16},"start":{"character":6,"line":16}},"uri":"file://$ROOT/fixtures/entry.jazz"}]
  6 result []
  7 result null
