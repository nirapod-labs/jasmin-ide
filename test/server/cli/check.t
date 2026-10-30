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
does not restate them: the numbers are the compiler's, and `make parity`
compares them with the compiler over a corpus.

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

The errors are printed in the order the compiler meets them, so the first
line is the compiler's first error. It types the items in order and reads a
required file when it reaches the `require`: a required file's errors come
before the document's own errors that follow the `require`, and after one
that precedes it. A syntax error in the document comes before everything,
because the compiler parses the whole document before it reads any required
file. A require at or before the syntax error is never read, so its file's
errors are not printed at all; one after it is still typed by the recovered
tree, and its errors follow the document's.

  $ printf 'fn lib(reg u32 a) -> reg u64 {\n  reg u64 r;\n  r = a;\n  return r;\n}\n' > broken.jinc
  $ printf 'require "broken.jinc"\nfn f() {}\n' > clean.jazz
  $ printf 'int N = 1;\nrequire "broken.jinc"\n' > typed.jazz
  $ printf 'param int N = 1\nrequire "broken.jinc"\n' > before.jazz
  $ printf 'fn g( {}\n\nrequire "broken.jinc"\n' > after.jazz
  $ ../../../lsp-server/native/main.exe check clean.jazz typed.jazz before.jazz after.jazz > order.txt; echo "status $?"
  status 1
  $ cut -d: -f1-4 order.txt
  broken.jinc:3:7: typing
  clean.jazz: ok
  typed.jazz:1:1: typing
  broken.jinc:3:7: typing
  typed.jazz: 1 error
  before.jazz:2:1: parse
  before.jazz:2:1: parse
  before.jazz: 2 errors
  after.jazz:1:7: parse
  after.jazz:1:7: parse
  broken.jinc:3:7: typing
  after.jazz: 2 errors
