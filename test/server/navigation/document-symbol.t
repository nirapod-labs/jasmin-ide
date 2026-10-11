A documentSymbol request is routed to the engine and answered with a list of
symbols, an empty list when the document has none. The engine answers from the
index's own outline, so a document with one function gives one node: its name,
the signature the index cut from the source, its kind, the range of the whole
item and the range of the name. A blank document and a document the client
never opened give `[]`, with no error for either. The request takes no
position, so the blank document is the "no item is here" case the other three
requests spell as a position in whitespace.

This test also pins that a second document's publish and reply do not disturb
the first: the publish names the document it is about.

  $ cat > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz","languageId":"jasmin-crypto","version":1,"text":"fn f() -> reg u32 {\n  reg u32 x;\n  x = 0;\n  return x;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/documentSymbol","params":{"textDocument":{"uri":"file:///tmp/navigation.jazz"}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/blank.jazz","languageId":"jasmin-crypto","version":1,"text":"\n\n"}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/documentSymbol","params":{"textDocument":{"uri":"file:///tmp/blank.jazz"}}}
  > 
  > {"jsonrpc":"2.0","id":4,"method":"textDocument/documentSymbol","params":{"textDocument":{"uri":"file:///tmp/never-opened.jazz"}}}
  > 
  > {"jsonrpc":"2.0","id":5,"method":"shutdown"}
  > EOF

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0

  $ sed 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"completionProvider":{"resolveProvider":false,"triggerCharacters":["#",":"]},"definitionProvider":true,"documentHighlightProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"semanticTokensProvider":{"full":true,"legend":{"tokenModifiers":["declaration","inline","export"],"tokenTypes":["namespace","type","function","parameter","variable","property","keyword"]}},"signatureHelpProvider":{"triggerCharacters":["(",","]},"textDocumentSync":1,"workspaceSymbolProvider":true},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  - textDocument/publishDiagnostics {"diagnostics":[],"uri":"file:///tmp/navigation.jazz","version":1}
  2 result [{"detail":"fn f() -> reg u32","kind":12,"name":"f","range":{"end":{"character":1,"line":4},"start":{"character":0,"line":0}},"selectionRange":{"end":{"character":4,"line":0},"start":{"character":3,"line":0}}}]
  - textDocument/publishDiagnostics {"diagnostics":[],"uri":"file:///tmp/blank.jazz","version":1}
  3 result []
  4 result []
  5 result null

The same question across a `require` and across a namespace. The document is
the one `definition.t` describes: it is opened at `fixtures/entry.jazz` and it
requires `fixtures/lib.jinc`. The outline is the index's own nesting, so the
`require` is a node of its own — kind 2, named by the keyword that introduced
it, whose child is the file it names — and the `namespace A` is one node of
kind 3 whose children are the functions it declares, in the order it declares
them, each with the signature cut from its source. A `require` has no name of
its own, so its selection range is the whole item, while the file it names is
selected on the string literal with its quotes: an editor that jumps to either
lands on the file name. The namespace's own selection range is its name, and
the two functions after it are its children of kind 12, which is what an
outline collapses.

The directory the test runs in is dune's sandbox, whose name changes on every
run, so the record prints it as `$ROOT`; every other byte is the server's own.

  $ uri="file://$PWD/fixtures/entry.jazz"
  $ sed "s|@URI@|$uri|g" > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"@URI@","languageId":"jasmin-crypto","version":1,"text":"require \"lib.jinc\"\n\nnamespace A {\n  fn f(reg u32 x) -> reg u32 {\n    return x;\n  }\n\n  fn twice(reg u32 x) -> reg u32 {\n    reg u32 r;\n    r = f(x);\n    return r;\n  }\n}\n\nfn caller(reg u32 a) -> reg u32 {\n  reg u32 r;\n  r = A::f(a);\n  r = lib_f(r);\n  return r;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/documentSymbol","params":{"textDocument":{"uri":"@URI@"}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"shutdown"}
  > EOF

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0

  $ sed -e "s|$PWD|\$ROOT|g" -e 's/"version":"[^"]*"/"version":"<version>"/' replies.txt
  1 result {"capabilities":{"completionProvider":{"resolveProvider":false,"triggerCharacters":["#",":"]},"definitionProvider":true,"documentHighlightProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"semanticTokensProvider":{"full":true,"legend":{"tokenModifiers":["declaration","inline","export"],"tokenTypes":["namespace","type","function","parameter","variable","property","keyword"]}},"signatureHelpProvider":{"triggerCharacters":["(",","]},"textDocumentSync":1,"workspaceSymbolProvider":true},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  - textDocument/publishDiagnostics {"diagnostics":[],"uri":"file://$ROOT/fixtures/entry.jazz","version":1}
  2 result [{"children":[{"kind":2,"name":"lib.jinc","range":{"end":{"character":18,"line":0},"start":{"character":0,"line":0}},"selectionRange":{"end":{"character":18,"line":0},"start":{"character":8,"line":0}}}],"kind":2,"name":"require","range":{"end":{"character":18,"line":0},"start":{"character":0,"line":0}},"selectionRange":{"end":{"character":18,"line":0},"start":{"character":0,"line":0}}},{"children":[{"detail":"fn f(reg u32 x) -> reg u32","kind":12,"name":"f","range":{"end":{"character":3,"line":5},"start":{"character":13,"line":2}},"selectionRange":{"end":{"character":6,"line":3},"start":{"character":5,"line":3}}},{"detail":"fn twice(reg u32 x) -> reg u32","kind":12,"name":"twice","range":{"end":{"character":3,"line":11},"start":{"character":3,"line":5}},"selectionRange":{"end":{"character":10,"line":7},"start":{"character":5,"line":7}}}],"kind":3,"name":"A","range":{"end":{"character":1,"line":12},"start":{"character":0,"line":2}},"selectionRange":{"end":{"character":11,"line":2},"start":{"character":10,"line":2}}},{"detail":"fn caller(reg u32 a) -> reg u32","kind":12,"name":"caller","range":{"end":{"character":1,"line":19},"start":{"character":1,"line":12}},"selectionRange":{"end":{"character":9,"line":14},"start":{"character":3,"line":14}}}]
  3 result null
