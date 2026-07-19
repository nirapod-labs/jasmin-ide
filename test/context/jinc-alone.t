A .jinc checked on its own.

consts.jinc uses the parameters MLKEM_Q and MLKEM_N, which params.jinc
declares before requiring it. Checked as a document of its own there is no
such parameter in scope, so the declaration of jqx16 fails and the file
does not type. One diagnostic is reported for the item, at its first use of
MLKEM_Q; the column is the byte column the checker prints, counted from 1.

  $ sh corpus.sh ../../lsp-server/native/main.exe "$JASMIN_CONTEXT_CORPUS" alone
  consts.jinc:73:18: typing: unknown variable: `MLKEM_Q'
  consts.jinc: 1 error
  status 1
