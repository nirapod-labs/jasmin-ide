The build records which Jasmin source it used. The script reads
`../vendor/jasmin/` and `../vendor/jasmin.lock` from the directory it runs in,
so each layout below is a small tree with a `controller` directory beside a
`vendor` one.

  $ script=$PWD/../../build-route.sh
  $ layout() {
  >   rm -rf tree && mkdir -p tree/controller tree/vendor
  >   printf 'upstream = https://example.org/jasmin.tar.bz2\nversion = 2026.09.0\nsha256 = %s\n' \
  >     1111111111111111111111111111111111111111111111111111111111111111 > tree/vendor/jasmin.lock
  > }
  $ route() { (cd tree/controller && sh "$script"); }

No vendored tree: the library came from the switch.

  $ layout
  $ route
  let vendored_release : string option = None
  let vendored_tarball : string option = None

The vendored tree is the release the lock pins: its stamp is the lock's
digest.

  $ layout
  $ mkdir tree/vendor/jasmin
  $ echo 1111111111111111111111111111111111111111111111111111111111111111 > tree/vendor/jasmin/.source-sha256
  $ route
  let vendored_release = Some "2026.09.0"
  let vendored_tarball : string option = None

A tree with no stamp is read as the lock's release.

  $ rm tree/vendor/jasmin/.source-sha256
  $ route
  let vendored_release = Some "2026.09.0"
  let vendored_tarball : string option = None

A tree assembled from another tarball, as the edge channel does, is not that
release: the record names the tarball's digest and no release.

  $ echo 93f4f1c3de5bbd341bc6bb35568cad8a13ef9aa88c2c74598d7cb2ff8b57fd7b > tree/vendor/jasmin/.source-sha256
  $ route
  let vendored_release : string option = None
  let vendored_tarball = Some "93f4f1c3de5b"
