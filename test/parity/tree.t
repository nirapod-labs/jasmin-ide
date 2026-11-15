The tree half of the parser parity check over a corpus this test writes itself.
The release's parse-and-print is not here, so a stand-in plays it: the same
rendering this program prints for one file, which is what the release's
binary prints for a file the two parsers read alike.

  $ mkdir -p corpus/success/x86-64
  $ printf 'export fn f(reg u64 x) -> reg u64 {\n  x += 1;\n  return x;\n}\n' > corpus/success/x86-64/a.jazz
  $ printf 'param int N = 4;\n' > corpus/success/b.jazz
  $ tree=../../test/parity/parity_tree.exe
  $ cat > same <<EOF
  > #!/bin/sh
  > exec $PWD/$tree "\$3" 2>/dev/null
  > EOF
  $ cat > wrong <<EOF
  > #!/bin/sh
  > $PWD/$tree "\$3" 2>/dev/null; echo planted
  > EOF
  $ printf '#!/bin/sh\necho no >&2\nexit 1\n' > refuses
  $ chmod +x same wrong refuses

Both files print alike, and the run passes.

  $ $tree --corpus corpus --jasmin2tex ./same --jobs 2 | tail -n 1; echo "status $?"
  tree: every file the compiler parses passes
  status 0

The negative control: a printing that differs in one line fails the run.

  $ $tree --corpus corpus --jasmin2tex ./wrong --jobs 2 | grep -E 'byte for byte|different|agree'
  tree: the same printing as the compiler, external, byte for byte: 0 of 2 compared; 0 not compared (0 refused, 0 answered nothing)
  tree: by group: checks, files, the compiler's parser answered, no tree, a different tree
  tree: different success/b.jazz: printing: ours: ; compiler: planted
  tree: different success/x86-64/a.jazz: printing: ours: ; compiler: planted
  tree: the corpus does not agree
  $ $tree --corpus corpus --jasmin2tex ./wrong > /dev/null; echo "status $?"
  status 1

A printer that refuses every file is not an oracle, so the run cannot be made.

  $ $tree --corpus corpus --jasmin2tex ./refuses > /dev/null; echo "status $?"
  status 2
