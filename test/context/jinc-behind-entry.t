The same .jinc behind an entry that declares the parameters first.

This is the context the rule of ARCHITECTURE.md section 8.4 builds,
supplied by hand: an entry written for the test requires params.jinc and
then consts.jinc, the order poly.jinc puts them in inside kem.jazz.
consts.jinc is the corpus file, copied unchanged and pinned by its digest,
so the row cannot drift into checking a different file. Typed in that
environment the file is clean, which is the verdict the rule should make
true of the file on its own; jinc-in-context.t records the file on its own
as the engine reports it today.

  $ sh corpus.sh ../../lsp-server/native/main.exe "$JASMIN_CONTEXT_CORPUS" scoped
  consts.jinc sha256 d45ddbcc45708a950c8443056bd28706bc783b5f34ba347401d4daea0bdbbb8b
  entry.jazz: ok
  status 0
