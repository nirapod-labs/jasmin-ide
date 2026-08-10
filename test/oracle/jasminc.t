The oracle script prints the compiler's own version and nothing else on
stdout. A stand-in compiler here is any executable that answers -version.

  $ cat > jasminc <<'EOF'
  > #!/bin/sh
  > echo "Jasmin Compiler 2026.09.0"
  > EOF
  $ chmod +x jasminc
  $ sh jasminc.sh ./jasminc
  Jasmin Compiler 2026.09.0

A compiler that also writes to stderr leaves stdout to the version alone.

  $ cat > chatty <<'EOF'
  > #!/bin/sh
  > echo "loading libraries" >&2
  > echo "Jasmin Compiler 2026.09.0"
  > EOF
  $ chmod +x chatty
  $ sh jasminc.sh ./chatty 2>/dev/null
  Jasmin Compiler 2026.09.0

Every refusal is exit 2, and the reason is on stderr.

  $ sh jasminc.sh /nonexistent/jasminc
  test/oracle/jasminc.sh: error: no such file: /nonexistent/jasminc
  [2]

  $ sh jasminc.sh .
  test/oracle/jasminc.sh: error: no such file: .
  [2]

  $ printf 'not a program\n' > not-executable
  $ sh jasminc.sh ./not-executable
  test/oracle/jasminc.sh: error: not executable: ./not-executable
  [2]

  $ cat > failing <<'EOF'
  > #!/bin/sh
  > exit 3
  > EOF
  $ chmod +x failing
  $ sh jasminc.sh ./failing
  test/oracle/jasminc.sh: error: ./failing -version failed
  [2]

  $ cat > silent <<'EOF'
  > #!/bin/sh
  > exit 0
  > EOF
  $ chmod +x silent
  $ sh jasminc.sh ./silent
  test/oracle/jasminc.sh: error: the compiler printed no version
  [2]

  $ sh jasminc.sh a b
  test/oracle/jasminc.sh: usage: test/oracle/jasminc.sh [<path-to-jasminc>]
  [2]

With no argument the script asks a machine, and refuses without asking when
colima is not on PATH.

  $ env PATH=/nonexistent /bin/sh jasminc.sh
  test/oracle/jasminc.sh: error: no path was given and colima is not on PATH, so there is no compiler to run
  [2]

A machine that stopped answering is a skip with a message, not a wait: the
call is bounded by JASMINC_TIMEOUT.

  $ mkdir -p fakebin
  $ cat > fakebin/colima <<'EOF'
  > #!/bin/sh
  > sleep 30
  > EOF
  $ chmod +x fakebin/colima
  $ env PATH="$PWD/fakebin:$PATH" JASMINC_TIMEOUT=1 /bin/sh jasminc.sh
  test/oracle/jasminc.sh: error: no path was given and the colima profile 'jide' did not answer within 1s; start it, or pass the compiler's path
  [2]

The same bound covers the `colima ssh` call: a machine that answers `status`
and then stops answering is a message, not a wait.

  $ mkdir -p sshbin
  $ cat > sshbin/colima <<'EOF'
  > #!/bin/sh
  > case $1 in
  > status) exit 0 ;;
  > esac
  > sleep 30
  > EOF
  $ chmod +x sshbin/colima
  $ env PATH="$PWD/sshbin:$PATH" JASMINC_TIMEOUT=1 /bin/sh jasminc.sh
  test/oracle/jasminc.sh: error: the colima profile 'jide' did not answer within 1s; start it, or pass the compiler's path
  [2]

A machine that answers is asked through colima ssh, and its compiler's
version is what the script prints.

  $ mkdir -p answerbin
  $ cat > answerbin/colima <<'EOF'
  > #!/bin/sh
  > case $1 in
  > status) exit 0 ;;
  > ssh) echo "Jasmin Compiler 2026.09.0" ;;
  > esac
  > EOF
  $ chmod +x answerbin/colima
  $ env PATH="$PWD/answerbin:$PATH" /bin/sh jasminc.sh
  Jasmin Compiler 2026.09.0

The Makefile reads the same exit as a skip when no compiler was named, and as
a failure when one was: a typo'd path must not turn the parity gate into a
silent skip.

  $ env MAKEFLAGS= make -f ../../Makefile oracle JASMINC=none
  skip oracle (none)
  $ env MAKEFLAGS= make -f ../../Makefile oracle JASMINC=/nonexistent/jasminc ORACLE=jasminc.sh 2>/dev/null; echo "exit: $?"
  fail oracle no such file: /nonexistent/jasminc (exit 2)
  exit: 2
  $ env MAKEFLAGS= make -f ../../Makefile oracle JASMINC=./jasminc ORACLE=jasminc.sh
  ok oracle Jasmin Compiler 2026.09.0

A path with a space in it is a path, not an absence: make splices JASMINC into
the command in single quotes, so the shell sees one word.

  $ mkdir -p "oracle dir"
  $ cp jasminc "oracle dir/jasminc"
  $ env MAKEFLAGS= make -f ../../Makefile oracle JASMINC="oracle dir/jasminc" ORACLE=jasminc.sh
  ok oracle Jasmin Compiler 2026.09.0
