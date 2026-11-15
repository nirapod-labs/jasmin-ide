The workflow check over workflows this test writes, read against the
repository's own Makefile and scripts.

  $ mkdir wf
  $ cat > wf/ci.yml <<'EOF'
  > name: ci
  > on: push
  > permissions:
  >   contents: read
  > jobs:
  >   build:
  >     runs-on: ubuntu-24.04
  >     steps:
  >       - uses: actions/checkout@0123456789abcdef0123456789abcdef01234567
  >       - run: make build
  >       - run: sh scripts/check-version.sh
  > EOF
  $ check() { sh ../../scripts/check-workflows.sh "$PWD/wf" > out 2>/dev/null; s=$?; sed "s|$PWD/||" out; return $s; }
  $ check
  check-workflows: 1 workflows, every rule holds

A pin to a commit SHA passes; a tag or a branch fails.

  $ printf '      - uses: actions/setup-node@v4\n      - uses: actions/cache@main\n' >> wf/ci.yml
  $ check
  pins: wf/ci.yml:12: not pinned to a commit: v4
  pins: wf/ci.yml:13: not pinned to a commit: main
  check-workflows: 2 violation(s)
  [1]

A missing target, a missing script, a write for every job, a missing
read-only default and a local path each fail.

  $ head -n 11 wf/ci.yml > wf/b.yml; mv wf/b.yml wf/ci.yml
  $ printf '      - run: make bulid\n      - run: sh scripts/nothing.sh\n      - run: ls /Users/someone\n' >> wf/ci.yml
  $ sed 's/  contents: read/  contents: write/' wf/ci.yml > wf/b.yml; mv wf/b.yml wf/ci.yml
  $ check; echo "status $?"
  permissions: wf/ci.yml:4: a write granted to every job
  make: wf/ci.yml:12: no Makefile target bulid
  files: wf/ci.yml:13: no file scripts/nothing.sh
  content: wf/ci.yml:14: a local path or a tool name
  permissions: wf/ci.yml:1: no top-level contents: read
  check-workflows: 5 violation(s)
  status 1
