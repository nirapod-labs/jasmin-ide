`--stdio` names the transport and not a mode, so the dispatcher drops it
before it reads the mode. That is the precedence this file pins: a form after
`--stdio` is read as if it were the first word, so `--stdio --version` prints
the version and does not serve. The other order needs no rule of its own,
because `--version` and `--help` already ignore every word after them; it is
pinned here all the same, so that a later reader cannot narrow either word to
the front of the command line without a test saying so.

The line is the `--version` line itself, one line, with the shape
`design/INFRA.md` section 2 asks for:

  $ ../../../lsp-server/native/main.exe --stdio --version > out.txt 2> err.txt; echo "status $?"
  status 0
  $ wc -l < out.txt | tr -d ' '
  1
  $ wc -c < err.txt | tr -d ' '
  0
  $ grep -cE '^jasmin-lsp [0-9]+\.[0-9]+\.[0-9]+ \(Jasmin Compiler [0-9]{4}\.[0-9]{2}\.[0-9]+, built from (vendored release [0-9]{4}\.[0-9]{2}\.[0-9]+|installed library)\)$' out.txt
  1

It is compared byte for byte with what `--version` writes, in both orders, so
this test cannot pass by printing a line of its own:

  $ ../../../lsp-server/native/main.exe --version > plain.txt
  $ cmp -s out.txt plain.txt && echo "same line"
  same line
  $ ../../../lsp-server/native/main.exe --version --stdio > back.txt 2> err.txt; echo "status $?"
  status 0
  $ cmp -s back.txt plain.txt && echo "same line"
  same line
  $ wc -c < err.txt | tr -d ' '
  0

`--help` is preceded the same way, and the block is the same block:

  $ ../../../lsp-server/native/main.exe --stdio --help > out.txt 2> err.txt; echo "status $?"
  status 0
  $ ../../../lsp-server/native/main.exe --help > plain.txt
  $ cmp -s out.txt plain.txt && echo "same block"
  same block

The block is where the compatibility is stated, in the prose and in the
options, and it says which transport the word names:

  $ grep -c '^so `--stdio`, which asks for them, is accepted and changes nothing\.$' out.txt
  1
  $ grep -c '^  --stdio           serve on standard input and output, the only$' out.txt
  1
  $ grep -c '^                    transport there is: accepted for compatibility,$' out.txt
  1
