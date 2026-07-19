An entry from a public corpus that requires .jinc files.

kem.jazz requires kem.jinc, which requires indcpa.jinc and verify.jinc;
indcpa.jinc requires gen_matrix.jinc, params.jinc, poly.jinc and
polyvec.jinc, and those reach the rest of the unit: nineteen files in all,
eighteen of them .jinc, each resolved from the file that requires it.
Typing threads one environment through them, so an entry can only be
reported clean when every require resolved and every included item typed in
the environment the file before it left.

The corpus is not in this repository; docs/context-tests.md says which one
the rows are written against and how to run them.

  $ sh corpus.sh ../../lsp-server/native/main.exe "$JASMIN_CONTEXT_CORPUS" entry
  kem.jazz: ok
  status 0
