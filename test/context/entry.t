An entry from a public corpus that requires .jinc files.

kem.jazz requires kem.jinc, which requires indcpa.jinc and verify.jinc;
indcpa.jinc requires gen_matrix.jinc, params.jinc, poly.jinc and
polyvec.jinc, and those reach the rest of the unit: nineteen files in all,
eighteen of them .jinc, each resolved from the file that requires it.
Typing threads one environment through them: a require adds the items of
the file it names to the environment the rest of the entry is typed in, and
an item that fails leaves the environment it found, so a require that does
not resolve is reported at the require itself. A diagnostic whose span
indexes a required file is not reported on the entry
(jasmin/adapter.mli), so this verdict is the entry's own; it does not say
that the required files hold no error of their own, and required-file.t
records that gap.

The corpus is not in this repository; docs/context-tests.md says which one
the rows are written against and how to run them.

  $ sh corpus.sh ../../lsp-server/native/main.exe "$JASMIN_CONTEXT_CORPUS" entry
  kem.jazz: ok
  status 0
