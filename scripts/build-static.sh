#!/bin/sh
# Build the server as a static release binary for one platform.
#
#   scripts/build-static.sh <linux-x86_64|linux-aarch64|macos-aarch64> [<output>]
#
# Linux: musl, linked `-static -no-pie`, on an Alpine host or in the image of
# scripts/static-build.Dockerfile (JIDE_STATIC_IMAGE; it is never pulled).
# macOS arm64: `libgmp.a` (JIDE_LIBGMP_A, else pkg-config, else Homebrew)
# linked into the image, so only libSystem remains.
#
# The binary is built under a temporary name and moved onto <output> only
# after scripts/check-linked.sh passes, so a failure leaves no release file.
# A host that cannot build the platform is refused, never given a dynamic
# binary under a static name. Exit status: 0 built and checked, 1 the build
# or the check failed, 2 a usage error or a refusal.

set -eu
here=$(cd "$(dirname "$0")/.." && pwd)
name=build-static.sh
refuse() { echo "$name: refusing: $*" >&2; exit 2; }

[ $# -ge 1 ] && [ $# -le 2 ] || { echo "usage: $name <linux-x86_64|linux-aarch64|macos-aarch64> [<output>]" >&2; exit 2; }
platform=$1
out=${2:-$here/.artifacts/static/jasmin-lsp-$platform}
case $out in /*) ;; *) out=$(pwd)/$out ;; esac
[ -d "$here/vendor/jasmin" ] || refuse "vendor/jasmin is not assembled; run vendor/fetch.sh first"

# The commit's own time, so two builds of one commit can be compared.
epoch=${SOURCE_DATE_EPOCH:-$(git -C "$here" log -1 --format=%ct 2>/dev/null || echo 0)}
jobs=${DUNE_JOBS:-4}
if [ -n "${DUNE:-}" ]; then dune=$DUNE
elif command -v dune >/dev/null 2>&1; then dune=dune
else dune="opam exec --switch ${OPAM_SWITCH:-jasmin-ide} -- dune"
fi

stage=$(mktemp -d "${TMPDIR:-/tmp}/jasmin-static.XXXXXX")
trap 'rm -rf "$stage"' EXIT

# dune has no option for link flags, so they reach the link through a
# workspace file given with --workspace.
workspace() { printf '(lang dune 3.24)\n(env (_ (link_flags (%s))))\n' "$*" >"$stage/dune-workspace"; }
build() {
	SOURCE_DATE_EPOCH=$epoch ${HEAVY:-} $dune build --root "$here" --profile release \
		--build-dir "$stage/build" --workspace "$stage/dune-workspace" -j "$jobs" \
		lsp-server/native/main.exe
	cp "$stage/build/default/lsp-server/native/main.exe" "$stage/server"
}

case $platform in
macos-aarch64)
	[ "$(uname -sm)" = "Darwin arm64" ] || refuse "macos-aarch64 needs an arm64 macOS host"
	gmp=${JIDE_LIBGMP_A:-}
	[ -n "$gmp" ] || gmp="$(pkg-config --variable=libdir gmp 2>/dev/null)/libgmp.a"
	[ -f "$gmp" ] || gmp=/opt/homebrew/opt/gmp/lib/libgmp.a
	[ -f "$gmp" ] || refuse "no libgmp.a on this host; set JIDE_LIBGMP_A to a static GMP archive"
	# A directory holding only the archive, ahead of zarith's own -L, makes
	# -lgmp resolve to it rather than to the dylib.
	mkdir "$stage/gmp" && ln -s "$gmp" "$stage/gmp/libgmp.a"
	workspace -ccopt "-L$stage/gmp" -cclib -lgmp
	echo "$name: building macos-aarch64 against $gmp"
	build
	;;
linux-x86_64 | linux-aarch64)
	arch=${platform#linux-}
	workspace -ccopt -static -ccopt -no-pie
	if [ "$(uname -s)" = Linux ] && ls /lib/ld-musl-*.so.1 >/dev/null 2>&1; then
		[ "$(uname -m)" = "$arch" ] || refuse "$platform needs an $arch host"
		echo "$name: building $platform on this musl host"
		build
	else
		runtime=
		for candidate in ${JIDE_CONTAINER:-docker podman}; do
			if command -v "$candidate" >/dev/null 2>&1 && "$candidate" info >/dev/null 2>&1; then
				runtime=$candidate
				break
			fi
		done
		[ -n "$runtime" ] || refuse "no musl host and no container runtime that answers (docker, podman)"
		image=${JIDE_STATIC_IMAGE:-jasmin-ide-static-build:latest}
		"$runtime" image inspect "$image" >/dev/null 2>&1 ||
			refuse "no image $image; build it from scripts/static-build.Dockerfile"
		[ "$arch" = x86_64 ] && docker_arch=amd64 || docker_arch=arm64
		mkdir "$stage/out"
		echo "$name: building $platform in $image ($runtime, linux/$docker_arch)"
		# The tree is mounted read-only, and the workspace file beside it: a
		# mount point cannot be made inside a read-only mount.
		"$runtime" run --rm --platform "linux/$docker_arch" \
			-v "$here:/src:ro" -v "$stage/dune-workspace:/dune-workspace:ro" -v "$stage/out:/out" \
			-w /src -e "SOURCE_DATE_EPOCH=$epoch" "$image" sh -c "
				/opt/static/run-dune build --root /src --profile release --build-dir /tmp/build \
					--workspace /dune-workspace -j $jobs lsp-server/native/main.exe &&
				cp /tmp/build/default/lsp-server/native/main.exe /out/server"
		cp "$stage/out/server" "$stage/server"
	fi
	;;
*) echo "usage: $name <linux-x86_64|linux-aarch64|macos-aarch64> [<output>]" >&2; exit 2 ;;
esac

sh "$here/scripts/check-linked.sh" "$stage/server" "${platform%%-*}" ||
	{ echo "$name: $platform links a library it may not; nothing was written" >&2; exit 1; }
mkdir -p "$(dirname "$out")"
mv "$stage/server" "$out"
chmod 755 "$out"
echo "$name: wrote $out"
