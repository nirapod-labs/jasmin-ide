The named include paths of the JASMINPATH environment variable are bound
after the command line's `-I`, as the compiler binds them: `NAME=PATH`
entries separated by `:`. The check reads a `from NAME require` through
them, and so does the context walk, so a file an entry reaches through such
a name is checked in that entry's context.

  $ server="$(pwd)/../../../lsp-server/native/main.exe"
  $ mkdir -p app/lib
  $ printf '[jasmin]\n' > app/jasmin.project
  $ cat > app/entry.jazz <<'SRC'
  > param int W = 4;
  > from P require "w.jinc"
  > export
  > fn main(reg u32 x) -> reg u32 {
  >   reg u32 y;
  >   y = widen(x);
  >   return y;
  > }
  > SRC
  $ cat > app/lib/w.jinc <<'SRC'
  > fn widen(reg u32 v) -> reg u32 {
  >   reg u32 r;
  >   r = v + W;
  >   return r;
  > }
  > SRC

With the name bound, the entry is clean and the library is checked in its
context, where the parameter is declared.

  $ JASMINPATH=Q=elsewhere:P=app/lib "$server" check app/entry.jazz app/lib/w.jinc; echo "status $?"
  app/entry.jazz: ok
  app/lib/w.jinc: ok
  status 0

Without it the name is unknown to both, and the library is checked alone.

  $ (unset JASMINPATH; "$server" check app/entry.jazz app/lib/w.jinc); echo "status $?"
  app/entry.jazz:2:6: typing: unknown name P
  app/entry.jazz:6:7: typing: unknown function: `widen'
  app/entry.jazz: 2 errors
  app/lib/w.jinc:3:11: typing: unknown variable: `W'
  app/lib/w.jinc: 1 error
  status 1

An entry with no `=` makes the whole value ill-formed: the compiler warns and
binds nothing from it.

  $ JASMINPATH=P "$server" check app/entry.jazz; echo "status $?"
  app/entry.jazz:2:6: typing: unknown name P
  app/entry.jazz:6:7: typing: unknown function: `widen'
  app/entry.jazz:1:1: warning: ill-formed value for the JASMINPATH environment variable
  app/entry.jazz: 2 errors
  status 1
