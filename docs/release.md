# Cutting a release

A release is one version of the server and both editor extensions, built
from one signed tag, published with the SHA-256 of every file.

Nothing has been released yet, and the GitHub workflows have never run: the
repository is private. What follows works locally today. Publishing,
attestations and the registries wait for the repository to be public.

## The version

One version, `0.MINOR.PATCH` until 1.0, is written in four files:
`controller/version.ml`, `editor/code/package.json`,
`editor/zed/extension.toml` and `editor/zed/Cargo.toml` (and
`editor/zed/Cargo.lock` beside it). They must agree. The server reports its
version in every `initialize` reply and the VS Code extension warns when the
two differ.

```
sh scripts/set-version.sh 0.2.0     # writes all five and checks them
sh scripts/check-version.sh 0.2.0   # or make version-check, the first step of make ci
```

## The files

| File | Made by |
|---|---|
| `jasmin-lsp-<version>-linux-x86_64`, `-linux-aarch64` | `scripts/build-static.sh` in an Alpine container: musl, linked static |
| `jasmin-lsp-<version>-macos-aarch64` | `scripts/build-static.sh` on an arm64 Mac, with `libgmp.a` linked in |
| `jasmin-lsp-<version>-src.tar.gz` | `git archive` of the tag |
| `jasmin-ide-<version>-<target>.vsix` and `-universal.vsix` | `editor/code/scripts/package-vsix.mjs` |
| the Zed extension's `.wasm` and `extension.toml` | `make editor-zed` |
| `<file>.sha256` beside each file, and `SHA256SUMS` | `scripts/checksums.sh write` |

`build-static.sh` builds into a temporary name, runs `scripts/check-linked.sh`
on the result, and moves it into place only when the check passes. The check
asks `otool -L` or `ldd` what the binary links and allows only `libSystem` on
macOS and musl on Linux. A host that can't build a platform (no container
runtime, no image, no `libgmp.a`) is refused with the reason, never given a
dynamic binary under a static name. `NOTICE.md` travels with the release,
because GMP is linked statically under the LGPL.

## Steps

1. Set the version and commit it.
2. On each build host, build its platform into one directory:
   `make release-static RELEASE_DIR=<dir>`, or
   `scripts/build-static.sh <platform> <dir>/jasmin-lsp-<version>-<platform>`.
3. Add the source: `git archive --format=tar.gz --prefix=jasmin-lsp-<version>/ -o <dir>/jasmin-lsp-<version>-src.tar.gz HEAD`.
4. Write and check the sums: `make release-sums release-verify-sums RELEASE_DIR=<dir>`.
5. Build twice and compare (below), and keep the record with the release.
6. Tag `v<version>`, signed, on the commit the files came from, and publish
   the files with the tag.

`make release` does steps 2 and 4 for the host it runs on.

## Checksums

`SHA256SUMS` has one line per file, sorted, in the form `sha256sum -c` and
`shasum -a 256 -c` read, so a user needs nothing of ours to check a
download. Each `<file>.sha256` is the same line for one file.

`sh scripts/checksums.sh verify <dir>` reads the list back. Every line must be
well formed, every file it names present and matching, no name listed twice,
and no file in the directory left out. It prints `ok <name>` per file and
exits 1 on any problem.

A digest says the bytes didn't change. It doesn't say who built them: that's
the attestation's job, which needs CI.

## The double build

Whether two builds of one commit give the same bytes is measured, not
assumed. Check out the tag twice, assemble `vendor/jasmin` in both, then:

```
make release-double-build TREE_A=../tree-a TREE_B=../tree-b
```

Both trees must be clean and at the same commit. Each builds the server in
the release profile with `SOURCE_DATE_EPOCH` set to the commit's time, and a
source tarball; `JIDE_LINK_FLAGS` adds link flags and `JIDE_SRC_PREFIX` sets
the tarball's prefix. `.artifacts/double-build/double-build.txt` records the
digest and size of each file in both passes. When a file differs it also
records how many bytes differ, the first offset, and whether either tree's
path appears inside, since an embedded path is the usual cause. The target
fails on any difference.

Until a double build shows identical bytes, a release promises pinned inputs
and an attested build, not identical bytes.
