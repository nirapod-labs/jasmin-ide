The same .jinc behind an entry that declares the parameters first.

This is the context the rule of ARCHITECTURE.md section 8.4 builds,
supplied by hand: an entry written for the test requires params.jinc and
then consts.jinc, the order poly.jinc puts them in inside kem.jazz, and
then uses jdmontx16, the last array consts.jinc declares. consts.jinc is
the corpus file, copied unchanged and pinned by its digest, so the row
cannot drift into checking a different file.

The use is what makes the verdict an observation of the file and not only
of the entry. Typing is item by item in the environment the requires leave
behind, and a require whose items fail leaves the environment it found, so
a failure anywhere in consts.jinc leaves none of its declarations in
scope. The clean line below is therefore the entry typed with the whole of
consts.jinc in scope, and the second check is the control that shows it:
the same entry over a copy of the file whose first MLKEM_Q at 73:18, the
token jinc-alone.t reports, is MLKEM_NOPE, and the entry then reports the
symbol it uses as unknown.

What the control prints is the entry's own diagnostic, not the file's: a
diagnostic whose span indexes a required file is dropped from a check of
the entry (jasmin/adapter.mli), and required-file.t records that gap. So
this row observes the context being built and used, up to the last item of
the included file; the file's own diagnostics in context are what
jinc-in-context.t records once the context rule lands.

  $ sh corpus.sh ../../lsp-server/native/main.exe "$JASMIN_CONTEXT_CORPUS" scoped
  consts.jinc sha256 d45ddbcc45708a950c8443056bd28706bc783b5f34ba347401d4daea0bdbbb8b
  entry.jazz: ok
  status 0
  control: consts.jinc:73:18 is MLKEM_NOPE, not MLKEM_Q
  consts.jinc sha256 a1aa837a2f96fc52a29a9fe1f9f7f19be29f2c2ba5ddb62e57b193cc6713e315
  entry.jazz:6:7: typing: unknown variable: `jdmontx16'
  entry.jazz: 1 error
  status 1
