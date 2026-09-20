`check <file>` prints a verdict per file, `ok` and 0 when the file has no
error, the diagnostics and the summary and 1 when it has one, and nothing but
its own lines on stdout. The last part is the one the reader could have
broken: `check` is read before the server is created so the batch mode never
touches the protocol (`compiler/cli.ml`), and the `--version` line this card
adds must not change that.

  $ printf 'export fn main() -> reg u64 {\n  reg u64 x;\n  x = 1;\n  return x;\n}\n' > ok.jazz
  $ ../../../lsp-server/native/main.exe check ok.jazz > out.txt 2> err.txt; echo "status $?"
  status 0
  $ cat out.txt
  ok.jazz: ok
  $ wc -c < err.txt | tr -d ' '
  0

A file with an error is still the checker's verdict and the checker's exit
status. The line and column are the compiler's own, which is why this test
does not restate them: the numbers are the compiler's oracle, and
`compiler/test/compare.sh` compares them against it over a corpus.

  $ printf 'fn broken( -> {\n' > bad.jazz
  $ ../../../lsp-server/native/main.exe check bad.jazz > out.txt 2> err.txt; echo "status $?"
  status 1
  $ wc -l < out.txt | tr -d ' '
  3
  $ tail -n 1 out.txt
  bad.jazz: 2 errors
  $ wc -c < err.txt | tr -d ' '
  0

The checker's own refusals are still its own: an unknown check option is the
check mode's error text and its exit 2, not the server's usage line, because
the reader hands `check`'s arguments over unread.

  $ ../../../lsp-server/native/main.exe check --nonsense > out.txt 2> err.txt; echo "status $?"
  status 2
  $ wc -c < out.txt | tr -d ' '
  0
  $ cat err.txt
  jasmin-lsp check: unknown option: --nonsense
  usage: jasmin-lsp check [--arch NAME] [--call-conv NAME] [-I name=path]... <file>...
  
  Parse and type each file and print a verdict for it.
  One line per diagnostic, then one summary line per file.
  
  options:
    --arch NAME       target architecture (default: x86-64)
    --call-conv NAME  calling convention (default: linux)
    -I NAME=PATH      named include path, repeatable
    -h, --help        print this text
  
  exit status: 0 when no file has an error, 1 when one does, 2 when the
  command line itself is wrong.
