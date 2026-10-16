The same .jinc named as a document in the directory of its entry.

consts.jinc uses the parameters MLKEM_Q and MLKEM_N, which params.jinc
declares. Nothing in consts.jinc requires params.jinc: the environment
arrives through the chain of files that does. kem.jazz requires kem.jinc,
kem.jinc requires indcpa.jinc, indcpa.jinc requires poly.jinc, and
poly.jinc requires params.jinc, shuffle.jinc, consts.jinc, reduce.jinc and
the two fips202 files, in that order. The engine checks such a file in
the context of the first entry that requires it, typed up to that
require.

The checker reads that chain: it types the entry up to the require that
names the file, so consts.jinc is typed in the environment params.jinc
leaves behind and is clean, while the same bytes checked where no entry
requires them are the error jinc-alone.t records. kem.jazz in the same
directory, which requires the file through that chain, is clean too. Both
lines are the verdict of the rule, not an observation of a missing part.

  $ unit=$JASMIN_CONTEXT_CORPUS/oldsrc-should-delete/crypto_kem/mlkem/mlkem768/amd64/avx2
  $ lsp=$PWD/../../lsp-server/native/main.exe
  $ digest() { (sha256sum "$1" 2>/dev/null || shasum -a 256 "$1") | cut -d' ' -f1; }
  $ digest $unit/consts.jinc
  d45ddbcc45708a950c8443056bd28706bc783b5f34ba347401d4daea0bdbbb8b
  $ (cd $unit && $lsp check consts.jinc); echo "status $?"
  consts.jinc: ok
  status 0
  $ (cd $unit && $lsp check kem.jazz); echo "status $?"
  kem.jazz: ok
  status 0
