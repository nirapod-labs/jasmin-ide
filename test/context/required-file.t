A diagnostic that belongs to a required file.

This entry requires consts.jinc without params.jinc before it, so the file
holds exactly the error jinc-alone.t records, at consts.jinc:73:18. The
copy is pinned by its digest, the bytes jinc-alone.t and the other rows
check, so the row goes red when the corpus stops holding them instead of
staying green on a premise that is no longer true. A check of an entry
reports a diagnostic with the span of the file it is in, so that an error
in an included file is shown where it is: the line below names consts.jinc,
and its column counts that file's own bytes. The compiler reports the same
file at the same place, and exits 1. The document's own summary stays
`entry.jazz: ok`, because the entry has no error of its own, and the status
is 1 because the check did not come out clean.

The last line counts the diagnostic lines the check placed in
consts.jinc, those that begin `consts.jinc:<line>:`, and not the per-file
summary `consts.jinc: <n> error`, which carries the file name too.

  $ sh corpus.sh ../../lsp-server/native/main.exe "$JASMIN_CONTEXT_CORPUS" required
  consts.jinc sha256 d45ddbcc45708a950c8443056bd28706bc783b5f34ba347401d4daea0bdbbb8b
  consts.jinc:73:18: typing: unknown variable: `MLKEM_Q'
  entry.jazz: ok
  status 1
  diagnostics in consts.jinc: 1
