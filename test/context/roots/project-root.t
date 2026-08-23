A file checked from a subdirectory of a project is checked in the context of
the entry at the project root, which the project file above it marks.

The entry sits at the project root and requires the parameter file and then
lib/util.jinc, which uses the parameter. Checked on its own, util.jinc has no
parameter in scope, and in the context of the entry it is clean. The search
for entries starts at the project file above the file, not at the file's own
directory, so the check gives the same verdict from the project root and from
lib/. The last check is the bound: an entry outside the project root requires
lib/only.jinc, but that entry is not searched, so only.jinc is checked alone.

  $ server="$(pwd)/../../../lsp-server/native/main.exe"
  $ mkdir -p project/lib outside
  $ printf '[jasmin]\n' > project/jasmin.project
  $ printf 'param int WIDTH = 4;\n' > project/lib/params.jinc
  $ cat > project/lib/util.jinc <<'EOF'
  > fn widen(reg u32 value) -> reg u32 {
  >   reg u32 result;
  >   result = value + WIDTH;
  >   return result;
  > }
  > EOF
  $ cat > project/lib/only.jinc <<'EOF'
  > fn only(reg u32 value) -> reg u32 {
  >   reg u32 result;
  >   result = value + WIDTH;
  >   return result;
  > }
  > EOF
  $ cat > project/entry.jazz <<'EOF'
  > require "lib/params.jinc"
  > require "lib/util.jinc"
  > export
  > fn main(reg u32 x) -> reg u32 {
  >   reg u32 y;
  >   y = widen(x);
  >   return y;
  > }
  > EOF
  $ cat > outside/entry.jazz <<'EOF'
  > require "../project/lib/params.jinc"
  > require "../project/lib/only.jinc"
  > export
  > fn other(reg u32 x) -> reg u32 {
  >   reg u32 y;
  >   y = only(x);
  >   return y;
  > }
  > EOF

  $ (cd project && "$server" check lib/util.jinc); echo "status $?"
  lib/util.jinc: ok
  status 0
  $ (cd project/lib && "$server" check util.jinc); echo "status $?"
  util.jinc: ok
  status 0
  $ (cd project && "$server" check lib/only.jinc); echo "status $?"
  lib/only.jinc:3:20: typing: unknown variable: `WIDTH'
  lib/only.jinc: 1 error
  status 1
