An error in a required file is printed with that file's path as the compiler
prints it. A bare `require` is resolved beside the file that writes it and
printed relative when that file's path is, keeping the `..` that climb out of
the working directory. A file reached through `from NAME require` is printed
with an absolute path, because the compiler takes the directory `-I` binds
from its working directory and keeps it absolute. A file checked in the
context of an entry prints the entry's other files the same way.

  $ mkdir -p lib sub ctx
  $ printf 'fn lib(reg u32 a) -> reg u64 {\n  reg u64 r;\n  r = a;\n  return r;\n}\n' > lib/broken.jinc
  $ printf 'require "broken.jinc"\n' > lib/inner.jinc
  $ printf 'require "lib/inner.jinc"\n' > chain.jazz
  $ printf 'from L require "broken.jinc"\n' > named.jazz
  $ printf 'require "./lib/../lib/broken.jinc"\n' > dots.jazz
  $ cp lib/broken.jinc ctx/bad.jinc
  $ printf 'param int A = 1;\n' > ctx/a.jinc
  $ printf 'param int C = A;\n' > ctx/c.jinc
  $ printf 'require "a.jinc"\nrequire "bad.jinc"\nrequire "c.jinc"\n' > ctx/entry.jazz
  $ server=$(pwd)/../../../lsp-server/native/main.exe
  $ here=$(pwd -P)

  $ "$server" check chain.jazz dots.jazz ctx/c.jinc | grep broken; "$server" check ctx/c.jinc | grep bad
  lib/broken.jinc:3:7: typing: can not implicitly cast u32 into u64
  lib/broken.jinc:3:7: typing: can not implicitly cast u32 into u64
  ctx/bad.jinc:3:7: typing: can not implicitly cast u32 into u64
  $ (cd sub && "$server" check ../chain.jazz | head -n 1)
  ../lib/broken.jinc:3:7: typing: can not implicitly cast u32 into u64
  $ "$server" check -I L=lib named.jazz | head -n 1 | sed "s#^$here/#<cwd>/#"
  <cwd>/lib/broken.jinc:3:7: typing: can not implicitly cast u32 into u64
  $ (cd sub && "$server" check -I L=../lib ../named.jazz | head -n 1 | sed "s#^$here/#<cwd>/#")
  <cwd>/lib/broken.jinc:3:7: typing: can not implicitly cast u32 into u64
