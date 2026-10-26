A rename is routed to the engine, and it reaches every file of the project
that uses the symbol. The project is a directory with a library, `lib.jinc`,
and two entries that require it, `main.jazz` and `other.jazz`; the client
names the directory as its root and opens the library only. The entries are
read from the disk.

Request 2 is a prepareRename on the library's function: it answers the range
of the name a rename would rewrite. Request 3 renames the function: the
WorkspaceEdit rewrites its declaration in the library and its call in each
entry, although neither entry is open. Request 4 is a prepareRename on a
keyword, which is refused with the engine's reason as an error, so a client
can show why. Request 5 renames the parameter `a` to `b`, which the function
declares as a local: the renamed use would mean the local, so the rename is
refused with the declaration it collides with. Request 6 names a new name that
is not one name, which is InvalidParams and is checked before the engine is
asked. The capability is advertised with prepareProvider, so a client offers
the rename.

The directory the test runs in is dune's sandbox, whose name changes on every
run, so the record prints it as `$ROOT`; every other byte is the server's own.

  $ mkdir project
  $ printf 'fn lib_fn(reg u32 a) -> reg u32 {\n  reg u32 b;\n  b = a;\n  return b;\n}\n' > project/lib.jinc
  $ printf 'require "lib.jinc"\nexport fn main(reg u32 x) -> reg u32 {\n  x = lib_fn(x);\n  return x;\n}\n' > project/main.jazz
  $ printf 'require "lib.jinc"\nexport fn other(reg u32 y) -> reg u32 {\n  y = lib_fn(y);\n  return y;\n}\n' > project/other.jazz
  $ root="file://$PWD/project"
  $ sed "s|@ROOT@|$root|g" > session.txt <<'EOF'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"rootUri":"@ROOT@","capabilities":{}}}
  > 
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"@ROOT@/lib.jinc","languageId":"jasmin-crypto","version":1,"text":"fn lib_fn(reg u32 a) -> reg u32 {\n  reg u32 b;\n  b = a;\n  return b;\n}\n"}}}
  > 
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/prepareRename","params":{"textDocument":{"uri":"@ROOT@/lib.jinc"},"position":{"line":0,"character":3}}}
  > 
  > {"jsonrpc":"2.0","id":3,"method":"textDocument/rename","params":{"textDocument":{"uri":"@ROOT@/lib.jinc"},"position":{"line":0,"character":3},"newName":"lib_call"}}
  > 
  > {"jsonrpc":"2.0","id":4,"method":"textDocument/prepareRename","params":{"textDocument":{"uri":"@ROOT@/lib.jinc"},"position":{"line":3,"character":3}}}
  > 
  > {"jsonrpc":"2.0","id":5,"method":"textDocument/rename","params":{"textDocument":{"uri":"@ROOT@/lib.jinc"},"position":{"line":2,"character":6},"newName":"b"}}
  > 
  > {"jsonrpc":"2.0","id":6,"method":"textDocument/rename","params":{"textDocument":{"uri":"@ROOT@/lib.jinc"},"position":{"line":2,"character":6},"newName":"a b"}}
  > 
  > {"jsonrpc":"2.0","id":7,"method":"shutdown"}
  > EOF

  $ ../../client/jide_test_client.exe --server ../../../lsp-server/native/main.exe --timeout 3000 session.txt > replies.txt 2> server.err; echo "status $?"
  status 0

  $ sed -e 's/"version":"[^"]*"/"version":"<version>"/' -e "s|$PWD|\$ROOT|g" replies.txt
  1 result {"capabilities":{"completionProvider":{"resolveProvider":false,"triggerCharacters":["#",":"]},"definitionProvider":true,"documentHighlightProvider":true,"documentSymbolProvider":true,"hoverProvider":true,"referencesProvider":true,"renameProvider":{"prepareProvider":true},"semanticTokensProvider":{"full":true,"legend":{"tokenModifiers":["declaration","inline","export"],"tokenTypes":["namespace","type","function","parameter","variable","property","keyword"]}},"signatureHelpProvider":{"triggerCharacters":["(",","]},"textDocumentSync":1,"workspaceSymbolProvider":true},"serverInfo":{"name":"jasmin-lsp","version":"<version>"}}
  - textDocument/publishDiagnostics {"diagnostics":[],"uri":"file://$ROOT/project/lib.jinc","version":1}
  2 result {"end":{"character":9,"line":0},"start":{"character":3,"line":0}}
  3 result {"changes":{"file://$ROOT/project/lib.jinc":[{"newText":"lib_call","range":{"end":{"character":9,"line":0},"start":{"character":3,"line":0}}}],"file://$ROOT/project/main.jazz":[{"newText":"lib_call","range":{"end":{"character":12,"line":2},"start":{"character":6,"line":2}}}],"file://$ROOT/project/other.jazz":[{"newText":"lib_call","range":{"end":{"character":12,"line":2},"start":{"character":6,"line":2}}}]}}
  4 error {"code":-32803,"message":"a keyword is not a name a rename can change"}
  5 error {"code":-32803,"message":"b already names a different declaration (b in $ROOT/project/lib.jinc)"}
  6 error {"code":-32602,"message":"rename: newName must be one name, with nothing blank in it"}
  7 result null
