The same .jinc named as a document in the directory of its entry.

consts.jinc uses the parameters MLKEM_Q and MLKEM_N, which params.jinc
declares. Nothing in consts.jinc requires params.jinc: the environment
arrives through the chain of files that does. kem.jazz requires kem.jinc,
kem.jinc requires indcpa.jinc, indcpa.jinc requires poly.jinc, and
poly.jinc requires params.jinc, shuffle.jinc, consts.jinc, reduce.jinc and
the two fips202 files, in that order. Section 8.4 of ARCHITECTURE.md has
the engine check the file in the context of the first entry that requires
it, typed up to that require.

The engine as it stands reads no requiring entry: a named file is checked
as an entry of its own. The row therefore records what the check does
today. consts.jinc sits beside params.jinc and is still reported with the
unknown parameter jinc-alone.t records, while kem.jazz in the same
directory, which requires the file through that chain, is clean. The row
changes to the clean verdict the rule promises when the context rule
lands; until then it is the observation, not the expectation.

  $ sh corpus.sh ../../lsp-server/native/main.exe "$JASMIN_CONTEXT_CORPUS" context
  consts.jinc sha256 d45ddbcc45708a950c8443056bd28706bc783b5f34ba347401d4daea0bdbbb8b
  consts.jinc:73:18: typing: unknown variable: `MLKEM_Q'
  consts.jinc: 1 error
  status 1
  kem.jazz: ok
  status 0
