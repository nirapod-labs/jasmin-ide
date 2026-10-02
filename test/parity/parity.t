The parity run over a corpus this test writes itself, with stand-ins for the
checker and the compiler, so its verdicts and its negative controls run on
every tree. One file is accepted, one is rejected at 2:4 with a typing error,
and the release's record names that error.

  $ mkdir -p tests/success tests/fail
  $ printf 'fn f() {}\n' > tests/success/a.jazz
  $ printf 'fn g() {\n    x = 1;\n}\n' > tests/fail/b.jazz
  $ printf 'fail/b.jazz:\n\n"fail/b.jazz", line 2 (4-5):\ntyping error: unknown variable x\n\n' > tests/negative.expected
  $ cat > jasminc <<'EOF'
  > #!/bin/sh
  > [ "$1" = -version ] && { echo 'Jasmin Compiler 0.0'; exit 0; }
  > case $4 in
  > */fail/b.jazz) printf '"%s", line 2 (4-5):\ntyping error: unknown variable x\n' "$4"; exit 1 ;;
  > esac
  > exit 0
  > EOF
  $ cat > checker <<'EOF'
  > #!/bin/sh
  > file=$5
  > case $PLANT:$file in
  > verdict:success/a.jazz) printf '%s:1:1: typing: planted\n%s: 1 error\n' "$file" "$file"; exit 1 ;;
  > message:fail/b.jazz) printf '%s:2:5: typing: unknown variable y\n%s: 1 error\n' "$file" "$file"; exit 1 ;;
  > location:fail/b.jazz) printf '%s:2:6: typing: unknown variable x\n%s: 1 error\n' "$file" "$file"; exit 1 ;;
  > *:fail/b.jazz) printf '%s:2:5: typing: unknown variable x\n%s: 1 error\n' "$file" "$file"; exit 1 ;;
  > esac
  > printf '%s: ok\n' "$file"
  > EOF
  $ chmod +x jasminc checker
  $ parity() { ../../test/parity/parity.exe --corpus tests --checker ./checker --jobs 2 "$@" > out.txt; status=$?; grep -E '^(verdicts|negative checks|parity):' out.txt | grep -v ' with '; echo "status $status"; }

Both sides agree, against the compiler and against the record.

  $ parity --jasminc ./jasminc
  negative checks: 1; the same line and column 1; a different one 0; the checker accepts 0
  verdicts: 2 checks; the same verdict 2; a different one 0
  parity: every check agrees
  status 0
  $ parity
  negative checks: 1; the same line and column 1; a different one 0; the checker accepts 0
  verdicts: 2 checks; the same verdict 2; a different one 0
  parity: every check agrees
  status 0

The negative controls: a planted verdict, message or location each fails the
run.

  $ PLANT=verdict parity --jasminc ./jasminc
  negative checks: 1; the same line and column 1; a different one 0; the checker accepts 0
  verdicts: 2 checks; the same verdict 1; a different one 1
  parity: 1 differing checks, 0 that did not complete
  status 1
  $ PLANT=message parity --jasminc ./jasminc
  negative checks: 1; the same line and column 1; a different one 0; the checker accepts 0
  verdicts: 2 checks; the same verdict 2; a different one 0
  parity: 1 differing checks, 0 that did not complete
  status 1
  $ PLANT=location parity --jasminc ./jasminc
  negative checks: 1; the same line and column 0; a different one 1; the checker accepts 0
  verdicts: 2 checks; the same verdict 2; a different one 0
  parity: 1 differing checks, 0 that did not complete
  status 1

The record is a reference too: a planted message fails the run against it.

  $ PLANT=message parity
  negative checks: 1; the same line and column 1; a different one 0; the checker accepts 0
  verdicts: 2 checks; the same verdict 2; a different one 0
  parity: 1 differing checks, 0 that did not complete
  status 1

A compiler that does not run is not a skip: the run cannot be made.

  $ parity --jasminc ./missing
  parity: the compiler is not an executable program: ./missing
  status 2
