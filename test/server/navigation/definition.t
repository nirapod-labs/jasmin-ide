A definition request is routed to the engine and answered with a result: the
engine's locations, or `null` when it has none. The engine answers from the
index and the resolver, so the name at request 2 gives its declaration — the
uri the client opened the document with, and the range of `x` in the
`reg u32 x` that declares it. A position in whitespace and a position past the
end of the file are `null`, and neither is an error. The initialize result
shows the capability the routing advertises.

  $ cat > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz","languageId":"jasmin-crypto","version":1,"text":"fn f() -> reg u32 {\n  reg u32 x;\n  x = 0;\n  return x;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":2,"character":2}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/definition","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":2,"character":1}}}
  > 
  > {"jsonrpc":"2.0","id":4,"method":"textDocument/definition","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"},"position":{"line":99,"character":0}}}
  > 
  > {"jsonrpc":"2.0","id":5,"method":"shutdown"}
  > EOF

The request forces the check the debounce had not run yet, so the diagnostics
are published before the first reply: the reply and the publish describe the
same text.

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0

  $ sed 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"completionProvider":{"resolveProvider":false,"triggerCharacters":["#",":"]},"definitionProvider":true,"documentHighlightProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"renameProvider":{"prepareProvider":true},"signatureHelpProvider":{"triggerCharacters":["(",","]},"textDocumentSync":1},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  - textDocument/publishDiagnostics {"diagnostics":[],"uri":"file:///tmp/navigation.jazz","version":1}
  2 result [{"range":{"end":{"character":11,"line":1},"start":{"character":10,"line":1}},"uri":"file:///tmp/navigation.jazz"}]
  3 result null
  4 result null
  5 result null

The same question across a `require` and across a namespace. The document is
opened at `fixtures/entry.jazz`, beside `fixtures/lib.jinc`, and requires that
file; it also declares `namespace A` with a function `f` and two callers of
it, one inside the namespace and one outside. Request 2 asks the name the
`require` brought in, `lib_f`: the answer is a location in the required file,
whose uri is that file's and whose range is the `lib_f` it writes, so the
answer crosses the file boundary. Requests 3 and 4 ask the one declaration of
`f` from outside the namespace, as the qualified `A::f`, and from inside it,
as the call `f(x)` in `twice`: both answer the range of `f`'s own name inside
the namespace, which is what the qualified spelling and the unqualified one
both name. Request 5 asks the file name inside the `require` itself, which
names a file and not a declaration, so it is `null` as a position in
whitespace is. Request 6 asks the namespace's own declaration.

The directory the test runs in is dune's sandbox, whose name changes on every
run, so the record prints it as `$ROOT`; every other byte is the server's own.

  $ uri="file://$PWD/fixtures/entry.jazz"
  $ sed "s|@URI@|$uri|g" > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"@URI@","languageId":"jasmin-crypto","version":1,"text":"require \"lib.jinc\"\n\nnamespace A {\n  fn f(reg u32 x) -> reg u32 {\n    return x;\n  }\n\n  fn twice(reg u32 x) -> reg u32 {\n    reg u32 r;\n    r = f(x);\n    return r;\n  }\n}\n\nfn caller(reg u32 a) -> reg u32 {\n  reg u32 r;\n  r = A::f(a);\n  r = lib_f(r);\n  return r;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{"textDocument":{"uri":"@URI@"},"position":{"line":17,"character":6}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/definition","params":{"textDocument":{"uri":"@URI@"},"position":{"line":16,"character":9}}}
  > 
  > {"jsonrpc":"2.0","id":4,"method":"textDocument/definition","params":{"textDocument":{"uri":"@URI@"},"position":{"line":9,"character":8}}}
  > 
  > {"jsonrpc":"2.0","id":5,"method":"textDocument/definition","params":{"textDocument":{"uri":"@URI@"},"position":{"line":0,"character":9}}}
  > 
  > {"jsonrpc":"2.0","id":6,"method":"textDocument/definition","params":{"textDocument":{"uri":"@URI@"},"position":{"line":2,"character":10}}}
  > 
  > {"jsonrpc":"2.0","id":7,"method":"shutdown"}
  > EOF

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0

  $ sed -e "s|$PWD|\$ROOT|g" -e 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"completionProvider":{"resolveProvider":false,"triggerCharacters":["#",":"]},"definitionProvider":true,"documentHighlightProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"renameProvider":{"prepareProvider":true},"signatureHelpProvider":{"triggerCharacters":["(",","]},"textDocumentSync":1},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  - textDocument/publishDiagnostics {"diagnostics":[],"uri":"file://$ROOT/fixtures/entry.jazz","version":1}
  2 result [{"range":{"end":{"character":8,"line":0},"start":{"character":3,"line":0}},"uri":"file://$ROOT/fixtures/lib.jinc"}]
  3 result [{"range":{"end":{"character":6,"line":3},"start":{"character":5,"line":3}},"uri":"file://$ROOT/fixtures/entry.jazz"}]
  4 result [{"range":{"end":{"character":6,"line":3},"start":{"character":5,"line":3}},"uri":"file://$ROOT/fixtures/entry.jazz"}]
  5 result null
  6 result [{"range":{"end":{"character":11,"line":2},"start":{"character":10,"line":2}},"uri":"file://$ROOT/fixtures/entry.jazz"}]
  7 result null
