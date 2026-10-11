An option the binary does not take is a command line it refuses: a message
and a usage line on stderr, exit 2, and nothing at all on stdout. The clean
stdout is the part worth a session of its own. Standard output is the
protocol stream, so a byte written there by a bad command line would be read
as the start of a frame header by whatever reads that stream next; and the
exit code and the silence on stdout together are what tells a calling script
that no session ran.

Standard input is closed, and that is the assertion: before the option was
read, an unknown option fell through to `serve`, which waited on this stream
for a client that was never going to send anything, then printed `input
closed` on stderr and exited 1. A run that reached the loop cannot pass the
status and stderr checks below.

  $ for option in --nonsense -x --versionn -V; do
  >   ../../../lsp-server/native/main.exe "$option" < /dev/null > out.txt 2> err.txt
  >   echo "$option: status $?, stdout $(wc -c < out.txt | tr -d ' ') bytes"
  > done
  --nonsense: status 2, stdout 0 bytes
  -x: status 2, stdout 0 bytes
  --versionn: status 2, stdout 0 bytes
  -V: status 2, stdout 0 bytes

The detail names the option, and the second line is the usage line the help
block starts with, so a reader of the error learns the forms that do work.

  $ ../../../lsp-server/native/main.exe --nonsense < /dev/null > out.txt 2> err.txt; echo "status $?"
  status 2
  $ cat err.txt
  jasmin-lsp: unknown option: --nonsense
  usage: jasmin-lsp [--stdio] [--version | --help | check [options] <file>...]

The one option that is accepted is `--stdio`, and accepting it widens
nothing: it is dropped and the word after it is refused as unknown, with the
same two lines. One dash is a different word, and only the spelling a
language client passes is the accepted one.

  $ ../../../lsp-server/native/main.exe --stdio --nonsense < /dev/null > out.txt 2> err.txt; echo "status $?"
  status 2
  $ wc -c < out.txt | tr -d ' '
  0
  $ cat err.txt
  jasmin-lsp: unknown option: --nonsense
  usage: jasmin-lsp [--stdio] [--version | --help | check [options] <file>...]
  $ ../../../lsp-server/native/main.exe -stdio < /dev/null > out.txt 2> err.txt; echo "status $?"
  status 2
  $ head -n 1 err.txt
  jasmin-lsp: unknown option: -stdio

An argument that is not an option is refused too, rather than served: this
program reads a file only in the check mode, where the mode has to come
first, so `jasmin-lsp foo.jazz` is a mistake and not a request to check
`foo.jazz`. It says which form to use.

  $ ../../../lsp-server/native/main.exe foo.jazz < /dev/null > out.txt 2> err.txt; echo "status $?"
  status 2
  $ wc -c < out.txt | tr -d ' '
  0
  $ cat err.txt
  jasmin-lsp: unexpected argument: foo.jazz
  to check a file, name the mode: jasmin-lsp check <file>...
  usage: jasmin-lsp [--stdio] [--version | --help | check [options] <file>...]

The refusal is not fatal to the binary and does not depend on it: no
argument at all still serves, which is the mode an editor starts, and the
end of its input is the exit status the protocol gives (1 without a
shutdown). That is asserted here so a refusal added later cannot have been
made the default.

  $ ../../../lsp-server/native/main.exe < /dev/null > out.txt 2> err.txt; echo "status $?"
  status 1
  $ wc -c < out.txt | tr -d ' '
  0
  $ cat err.txt
  [jasmin-lsp] input closed
