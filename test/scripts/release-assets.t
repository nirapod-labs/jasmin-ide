A release publishes each asset "as raw executables with `.sha256` files"
(design/INFRA.md section 6), so one download and one line are enough to check
one asset. The sidecar is a line of the same form as a line of SHA256SUMS, and
the same tool reads both; every case here works in a directory the test makes,
so none of it needs a network, a build or a release.

  $ mkdir assets
  $ printf 'the linux binary\n' > assets/jasmin-lsp-0.1.0-linux-x86_64
  $ printf 'the macos binary\n' > assets/jasmin-lsp-0.1.0-macos-aarch64

One sidecar per asset, named after it, in a directory whose every file is an
asset.

  $ bash ../../scripts/release-assets.sh assets
  release-assets.sh: wrote 2 sidecars in assets
    jasmin-lsp-0.1.0-linux-x86_64
    jasmin-lsp-0.1.0-macos-aarch64

A sidecar holds the digest, two spaces and the file's name, and nothing else.

  $ sed 's/^[0-9a-f]\{64\}  //' assets/jasmin-lsp-0.1.0-linux-x86_64.sha256
  jasmin-lsp-0.1.0-linux-x86_64

A user who downloads that one asset and its sidecar runs the check the script
ran, and it agrees.

  $ check() {
  >   if command -v sha256sum >/dev/null 2>&1; then
  >     ( cd "$1" && sha256sum -c "$2" )
  >   else
  >     ( cd "$1" && shasum -a 256 -c "$2" )
  >   fi
  > }
  $ check assets jasmin-lsp-0.1.0-linux-x86_64.sha256
  jasmin-lsp-0.1.0-linux-x86_64: OK

A changed asset is what the sidecar is for.

  $ printf 'something else\n' > assets/jasmin-lsp-0.1.0-linux-x86_64
  $ check assets jasmin-lsp-0.1.0-linux-x86_64.sha256 2>/dev/null | grep FAILED
  jasmin-lsp-0.1.0-linux-x86_64: FAILED

A second run replaces the sidecars and does not make sidecars of them: the
directory still holds two assets, and the two scripts agree on which files a
release is.

  $ bash ../../scripts/release-assets.sh assets
  release-assets.sh: wrote 2 sidecars in assets
    jasmin-lsp-0.1.0-linux-x86_64
    jasmin-lsp-0.1.0-macos-aarch64
  $ check assets jasmin-lsp-0.1.0-linux-x86_64.sha256
  jasmin-lsp-0.1.0-linux-x86_64: OK
  $ bash ../../scripts/release-checksums.sh assets
  release-checksums.sh: wrote assets/SHA256SUMS: 2 files
    jasmin-lsp-0.1.0-linux-x86_64
    jasmin-lsp-0.1.0-macos-aarch64

The two refusals that are not a measurement are 1 — a directory with no asset
in it, and a subdirectory a flat sidecar cannot name — and both say why.

  $ mkdir empty
  $ bash ../../scripts/release-assets.sh empty
  release-assets.sh: error: empty holds no release file, so there is no sidecar to write
  [1]
  $ mkdir -p nested/sub && printf 'one\n' > nested/one
  $ bash ../../scripts/release-assets.sh nested
  release-assets.sh: error: nested holds a subdirectory (sub), which a flat sidecar cannot name
  [1]

The usage errors are 2: no argument, and a directory that is not there.

  $ bash ../../scripts/release-assets.sh
  usage: release-assets.sh <directory>
  [2]
  $ bash ../../scripts/release-assets.sh no-such-directory
  release-assets.sh: error: no such directory: no-such-directory
  [2]
