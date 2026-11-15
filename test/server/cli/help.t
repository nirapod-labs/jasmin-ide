`--help` prints the usage block on stdout and exits 0, and the modes it lists
are the ones the binary has: the default serve mode and `check`. Those are
the only two, so the test asks for both and for no third — a mode named in
the help and not implemented would be a lie in the one place a reader looks,
and a mode implemented and not named would be hidden from that reader.

The options listed are `--version`, `--help` and `--stdio`; the check mode's
own options stay `jasmin-lsp check --help`, which the block says, and which
is the checker's text rather than the server's.

The transport is stated twice, because this block is where a reader looks
when a client will not start: once in the prose, that standard input and
output are the only transport the server has, and once in the options, that
`--stdio` asks for exactly those two streams and is accepted for
compatibility. The reference in `controller/cli.mli` says the same.

  $ ../../../lsp-server/native/main.exe --help > out.txt 2> err.txt; echo "status $?"
  status 0
  $ cat out.txt
  usage: jasmin-lsp [--stdio] [--version | --help | check [options] <file>...]
  
  A language server for Jasmin. With no argument it serves the language
  server protocol on standard input and output, which is how an editor
  starts it. Those two streams are the only transport this server has,
  so `--stdio`, which asks for them, is accepted and changes nothing.
  
  modes:
    (none)            serve the language server protocol on stdin/stdout
    check <file>...   parse and type each file and print a verdict for it
  
  options:
    --stdio           serve on standard input and output, the only
                      transport there is: accepted for compatibility,
                      because a language client passes it
    -v, --version     print the server version, the Jasmin version and
                      the Jasmin source this build uses, then exit
    -h, --help        print this text, then exit
  
  Run `jasmin-lsp check --help` for the options of the check mode.
  
  exit status: 0 for a session that ended normally or a check with no
  error, 1 for a check with an error or a session without shutdown, and
  2 for a command line this program refuses.
  $ wc -c < err.txt | tr -d ' '
  0
  $ grep -c 'serve the language server protocol on stdin/stdout' out.txt
  1
  $ grep -c '^  check <file>\.\.\.' out.txt
  1

The block starts with the same line a refusal ends with, so the program has
one synopsis and not two.

  $ ../../../lsp-server/native/main.exe --nonsense > /dev/null 2> err.txt; echo "status $?"
  status 2
  $ tail -n 1 err.txt > synopsis.txt
  $ head -n 1 out.txt > first.txt
  $ cmp -s synopsis.txt first.txt && echo "same synopsis"
  same synopsis

The two modes that exist are the two named, and the exit codes the block
gives are the ones the binary returns: `check` on a file with no error is 0,
on a file with an error is 1, and a refused command line is 2, which the
sessions of `refused.t` and `check.t` pin.

  $ ../../../lsp-server/native/main.exe -h > out.txt 2> err.txt; echo "status $?"
  status 0
  $ ../../../lsp-server/native/main.exe --help > long.txt
  $ cmp -s out.txt long.txt && echo "same block"
  same block

`check --help` stays the checker's own text, so one mode's options are not
read as the server's.

  $ ../../../lsp-server/native/main.exe check --help | head -n 1
  usage: jasmin-lsp check [--arch NAME] [--call-conv NAME] [-I name=path]... <file>...
