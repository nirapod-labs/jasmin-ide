The same .jinc behind an entry that declares the parameters first.

This is the context the engine builds for a .jinc an entry requires,
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
token jinc-alone.t reports, is MLKEM_NOPE, and the check then reports the
file's own error at that token and exits 1.

What the control prints is the file's diagnostic, at its own place, and
not one of the entry's: the entry has nothing wrong with it, it just
cannot use a declaration the file failed to make. The `entry.jazz: ok`
above the status is that reading — the document is clean, and the check
failed because a file it requires did not type. The compiler reports the
same place for the same bytes, an unknown variable at consts.jinc line 73,
and reports nothing on entry.jazz, because it stops at the file's error.

  $ unit=$JASMIN_CONTEXT_CORPUS/oldsrc-should-delete/crypto_kem/mlkem/mlkem768/amd64/avx2
  $ lsp=$PWD/../../lsp-server/native/main.exe
  $ digest() { (sha256sum "$1" 2>/dev/null || shasum -a 256 "$1") | cut -d' ' -f1; }
  $ mkdir -p scoped/control
  $ cp $unit/params.jinc $unit/consts.jinc scoped/
  $ cat > scoped/entry.jazz <<'EOF'
  > require "params.jinc"
  > require "consts.jinc"
  > fn probe() -> reg u16 {
  >   reg u16 x;
  >   x = jdmontx16[0];
  >   return x;
  > }
  > EOF
  $ cp scoped/params.jinc scoped/entry.jazz scoped/control/
  $ sed '73s/MLKEM_Q/MLKEM_NOPE/' scoped/consts.jinc > scoped/control/consts.jinc
  $ digest scoped/consts.jinc
  d45ddbcc45708a950c8443056bd28706bc783b5f34ba347401d4daea0bdbbb8b
  $ (cd scoped && $lsp check entry.jazz); echo "status $?"
  entry.jazz: ok
  status 0
  $ digest scoped/control/consts.jinc
  a1aa837a2f96fc52a29a9fe1f9f7f19be29f2c2ba5ddb62e57b193cc6713e315
  $ (cd scoped/control && $lsp check entry.jazz); echo "status $?"
  consts.jinc:73:18: typing: unknown variable: `MLKEM_NOPE'
  entry.jazz: ok
  status 1
