A file an entry reaches only inside a namespace is checked with that entry as
its context. The walk reaches the file inside the namespace, where the
entry's own items stop, and the file is then typed on its own items: it is
the file being checked, not one the namespace requires of itself, so no
diagnostic says it requires itself.

  $ server="$(pwd)/../../../lsp-server/native/main.exe"
  $ mkdir lib
  $ printf 'namespace A {\n  require "t.jinc"\n}\n' > lib/entry.jazz
  $ printf 'fn f(reg u32 x) -> reg u32 {\n  reg u32 y;\n  y = x;\n  return y;\n}\n' > lib/t.jinc
  $ "$server" check lib/entry.jazz lib/t.jinc; echo "status $?"
  lib/entry.jazz: ok
  lib/t.jinc: ok
  status 0
