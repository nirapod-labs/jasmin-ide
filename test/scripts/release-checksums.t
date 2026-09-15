The checksum list is complete, sorted, in the layout `sha256sum -c` reads, and
one changed byte breaks it. Every case works in a directory the test makes, so
none of it needs a network, a build or a release; the digits are checked with
the platform's own tool, not with the script's own loop, because a list that
only the script can verify is not a list a user can check.

  $ mkdir assets
  $ printf 'alpha\n' > assets/alpha.txt
  $ printf 'beta\n' > assets/beta.txt
  $ printf 'gamma\n' > assets/gamma.txt
  $ printf 'stale\n' > assets/SHA256SUMS
  $ printf 'a sidecar\n' > assets/alpha.txt.sha256

A list left over from an earlier run and the per-file sidecars are not release
files, so neither is listed nor counted: three files, whatever else is in the
directory.

  $ bash ../../scripts/release-checksums.sh assets
  release-checksums.sh: wrote assets/SHA256SUMS: 3 files
    alpha.txt
    beta.txt
    gamma.txt

Each line is one digest, two spaces and the file's name, sorted by name.

  $ sed 's/^[0-9a-f]\{64\}  //' assets/SHA256SUMS
  alpha.txt
  beta.txt
  gamma.txt
  $ sed 's/  [^ ]*$//' assets/SHA256SUMS | while read -r d; do
  >   echo "${#d}"
  > done | sort -u
  64

The tool a user runs agrees — here that is whichever of the two the host has.

  $ check() {
  >   if command -v sha256sum >/dev/null 2>&1; then
  >     ( cd "$1" && sha256sum -c SHA256SUMS )
  >   else
  >     ( cd "$1" && shasum -a 256 -c SHA256SUMS )
  >   fi
  > }
  $ check assets
  alpha.txt: OK
  beta.txt: OK
  gamma.txt: OK

One changed byte, in a scratch copy of the directory, is what the list exists
to catch: the file is named and the tool's exit is 1. The tool's own warning
line is not printed, because its spelling is the tool's and not ours.

  $ cp -R assets changed
  $ printf 'alphA\n' > changed/alpha.txt
  $ check changed 2>/dev/null | grep FAILED
  alpha.txt: FAILED
  $ check changed >/dev/null 2>&1; echo "exit: $?"
  exit: 1

A list the script could not verify is not written. The tools here are
stand-ins that answer the digest question and refuse the list, so this
measures the refusal and not the digest arithmetic.

  $ mkdir -p fakebin
  $ cat > fakebin/sha256sum <<'EOF'
  > #!/bin/sh
  > # A stand-in: it answers the digest question with a fixed value and says
  > # every list it is asked to check is wrong.
  > case $1 in
  > -c) exit 1 ;;
  > *) printf '%064d  %s\n' 0 "$1" ;;
  > esac
  > EOF
  $ chmod +x fakebin/sha256sum
  $ cp -R assets unverified
  $ PATH=$PWD/fakebin:$PATH bash ../../scripts/release-checksums.sh unverified
  release-checksums.sh: error: the written list does not verify against unverified
  [1]
  $ cmp -s unverified/SHA256SUMS assets/SHA256SUMS; echo "list left as it was: $?"
  list left as it was: 0

A directory named relative to the caller is entered once, and the fallback
tool is reached from there. When the first tool could not answer, the second
half of the check used to run inside the directory the first half had already
entered; the second `cd` then failed, and a correct list was reported as one
that does not verify. Both tools are stand-ins here, so the fallback is the
one that runs on every host, and it records the directory it was entered in.

  $ mkdir -p relbin
  $ cat > relbin/sha256sum <<'EOF'
  > #!/bin/sh
  > # The first tool the script tries: it digests, but it says every list is
  > # wrong, so the fallback below is the one that has to verify.
  > case $1 in
  > -c) exit 1 ;;
  > *) printf '%064d  %s\n' 0 "$1" ;;
  > esac
  > EOF
  $ cat > relbin/shasum <<'EOF'
  > #!/bin/sh
  > # The fallback: it records the name of the directory it was entered in and
  > # says the list is good. Only its status is read by the script.
  > case $1 in
  > -a) shift 2 ;;
  > esac
  > case $1 in
  > -c) basename "$PWD" > "$PWD/fallback-ran" ;;
  > *) printf '%064d  %s\n' 0 "$1" ;;
  > esac
  > EOF
  $ chmod +x relbin/sha256sum relbin/shasum
  $ mkdir rel
  $ printf 'delta\n' > rel/delta.txt
  $ PATH=$PWD/relbin:$PATH bash ../../scripts/release-checksums.sh rel
  release-checksums.sh: wrote rel/SHA256SUMS: 1 files
    delta.txt
  $ cat rel/fallback-ran
  rel

A directory with no release file is a refusal: an empty list verifies nothing
while looking like a list, and a subdirectory would fall out of a flat list
without a word.

  $ mkdir empty
  $ bash ../../scripts/release-checksums.sh empty
  release-checksums.sh: error: empty holds no release file, so a checksum list would be empty
  [1]
  $ mkdir -p nested/sub && printf 'one\n' > nested/one
  $ bash ../../scripts/release-checksums.sh nested
  release-checksums.sh: error: nested holds a subdirectory (sub), which a flat checksum list cannot name
  [1]

The refusals that are not a measurement are 2: no argument, and a directory
that is not there.

  $ bash ../../scripts/release-checksums.sh
  usage: release-checksums.sh <directory>
  [2]
  $ bash ../../scripts/release-checksums.sh no-such-directory
  release-checksums.sh: error: no such directory: no-such-directory
  [2]
