A diagnostic that belongs to a required file.

This entry requires consts.jinc without params.jinc before it, so the file
holds exactly the error jinc-alone.t records, at consts.jinc:73:18. The
copy is pinned by its digest, the bytes jinc-alone.t and the other rows
check, so the row goes red when the corpus stops holding them instead of
staying green on a premise that is no longer true. A check of an entry is
expected to report a diagnostic with the span of the file it is in, so
that an error in an included file is shown where it is. The checker as it
stands keeps only the diagnostics whose span indexes the document it was
asked about, and drops the others (jasmin/check.ml, through
`Error.belongs_to`; jasmin/adapter.mli says so), so the entry is reported
clean and the error never reaches the user.

The last line counts the diagnostic lines the check placed in
consts.jinc, those that begin `consts.jinc:<line>:`, and not the per-file
summary `consts.jinc: <n> error`, which carries the file name too. It is
0 today, and 1 when the attribution lands; the row is recorded as observed
today and changes then.

  $ sh corpus.sh ../../lsp-server/native/main.exe "$JASMIN_CONTEXT_CORPUS" required
  consts.jinc sha256 d45ddbcc45708a950c8443056bd28706bc783b5f34ba347401d4daea0bdbbb8b
  entry.jazz: ok
  status 0
  diagnostics in consts.jinc: 0
