An entry from a public corpus that requires .jinc files.

kem.jazz requires kem.jinc, which requires indcpa.jinc and verify.jinc;
indcpa.jinc requires gen_matrix.jinc, params.jinc, poly.jinc and
polyvec.jinc, and those reach the rest of the unit: nineteen files in all,
eighteen of them .jinc, each resolved from the file that requires it.
Typing threads one environment through them: a require adds the items of
the file it names to the environment the rest of the entry is typed in, and
an item that fails leaves the environment it found, so a require that does
not resolve is reported at the require itself. A diagnostic whose span
indexes a required file is not reported on the entry (jasmin/adapter.mli):
it is reported against the file it is in, so this verdict is the entry's
own, and required-file.t records where such a diagnostic goes. A file that
two requires reach is entered once, as the compiler enters it once, so a
diamond in the closure does not report its declarations as already
declared.

The corpus is not in this repository; the rows are written against libjade; `make
context CORPUS=<libjade checkout>` runs them.

  $ unit=$JASMIN_CONTEXT_CORPUS/oldsrc-should-delete/crypto_kem/mlkem/mlkem768/amd64/avx2
  $ lsp=$PWD/../../lsp-server/native/main.exe
  $ (cd $unit && $lsp check kem.jazz); echo "status $?"
  kem.jazz: ok
  status 0
