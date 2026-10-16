A .jinc checked on its own.

consts.jinc uses the parameters MLKEM_Q and MLKEM_N, which params.jinc
declares before requiring it. Checked as a document of its own there is no
such parameter in scope, so the declaration of jqx16 fails and the file
does not type. One diagnostic is reported for the item, at its first use of
MLKEM_Q; the column is the byte column the checker prints, counted from 1.
The row checks a copy of the file in a directory that holds no entry, which
is a file no entry requires; checked beside
the entry that requires it, the file is clean, which is what
jinc-in-context.t records.

  $ unit=$JASMIN_CONTEXT_CORPUS/oldsrc-should-delete/crypto_kem/mlkem/mlkem768/amd64/avx2
  $ lsp=$PWD/../../lsp-server/native/main.exe
  $ mkdir alone && cp $unit/consts.jinc alone/
  $ (cd alone && $lsp check consts.jinc); echo "status $?"
  consts.jinc:73:18: typing: unknown variable: `MLKEM_Q'
  consts.jinc: 1 error
  status 1
