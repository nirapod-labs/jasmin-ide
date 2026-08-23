A file reached through a named include path is checked in the context of the
entry that reaches it.

An entry in a subdirectory of an include root requires two files by name.
`from Inc require "params.jinc"` names a file beside the directory the include
path Inc is bound to, not beside the entry. The checker reads the name the way
the compiler does, so the entry types params.jinc first, and widen.jinc, which
uses the parameter that file declares, is checked in that environment and is
clean. The control is the same two files reached by plain relative requires
from a second tree, which gives the same verdict.

  $ server="$(pwd)/../../../lsp-server/native/main.exe"
  $ mkdir -p named/inc/sub plain/inc/sub
  $ printf 'param int WIDTH = 4;\n' > named/inc/params.jinc
  $ cat > named/inc/widen.jinc <<'EOF'
  > fn widen(reg u32 value) -> reg u32 {
  >   reg u32 result;
  >   result = value + WIDTH;
  >   return result;
  > }
  > EOF
  $ cat > named/inc/sub/entry.jazz <<'EOF'
  > from Inc require "params.jinc"
  > from Inc require "widen.jinc"
  > export
  > fn main(reg u32 x) -> reg u32 {
  >   reg u32 y;
  >   y = widen(x);
  >   return y;
  > }
  > EOF
  $ cp -R named/inc/. plain/inc/
  $ cat > plain/inc/sub/entry.jazz <<'EOF'
  > require "../params.jinc"
  > require "../widen.jinc"
  > export
  > fn main(reg u32 x) -> reg u32 {
  >   reg u32 y;
  >   y = widen(x);
  >   return y;
  > }
  > EOF

With the include path given on the command line, the file is checked in the
context of the entry that reaches it, and the entry itself is clean.

  $ (cd named && "$server" check -I Inc=inc inc/widen.jinc); echo "status $?"
  inc/widen.jinc: ok
  status 0
  $ (cd named && "$server" check -I Inc=inc inc/sub/entry.jazz); echo "status $?"
  inc/sub/entry.jazz: ok
  status 0
  $ (cd plain && "$server" check inc/widen.jinc); echo "status $?"
  inc/widen.jinc: ok
  status 0

Without the name the requirement binds nothing, so the entry gives no context
and the file is checked alone. That is the verdict the compiler gives the file
on its own: the parameter is not in scope.

  $ (cd named && "$server" check inc/widen.jinc); echo "status $?"
  inc/widen.jinc:3:20: typing: unknown variable: `WIDTH'
  inc/widen.jinc: 1 error
  status 1
