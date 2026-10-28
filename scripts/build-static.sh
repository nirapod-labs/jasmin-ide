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

jobs=${DUNE_JOBS:-4}
epoch=${SOURCE_DATE_EPOCH:-}
if [ -z "$epoch" ]; then
	epoch=$(git -C "$here" log -1 --format=%ct 2>/dev/null) || epoch=
	epoch=${epoch:-0}
fi

stage=

remove_stage() {
	if [ -n "$stage" ] && [ -d "$stage" ]; then
		rm -rf "$stage"
	fi
	stage=
}
trap remove_stage EXIT
trap 'remove_stage; exit 130' INT TERM

dune_command() {
	if [ -n "${DUNE:-}" ]; then
		printf '%s\n' "$DUNE"
	elif command -v dune >/dev/null 2>&1; then
		printf 'dune\n'
	elif command -v opam >/dev/null 2>&1; then
		printf 'opam exec --switch %s -- dune\n' "${OPAM_SWITCH:-jasmin-ide}"
	else
		refuse "no dune and no opam on PATH, so the server cannot be built"
	fi
}

run_dune() {
	workspace=$1
	build_dir=$2
	shift 2
	if [ -n "${HEAVY:-}" ]; then
		SOURCE_DATE_EPOCH=$epoch $HEAVY $(dune_command) build --root "$here" \
			--profile release --build-dir "$build_dir" \
			--workspace "$workspace" -j "$jobs" "$@"
	else
		SOURCE_DATE_EPOCH=$epoch $(dune_command) build --root "$here" \
			--profile release --build-dir "$build_dir" \
			--workspace "$workspace" -j "$jobs" "$@"
	fi
}

#
write_flags_workspace() {
	path=$1
	shift
	{
		printf '(lang dune 3.24)\n'
		printf '(env\n (_\n  (link_flags\n   ('
		for flag in "$@"; do
			printf '%s ' "$flag"
		done
		printf '))))\n'
	} >"$path"
}

find_libgmp_a() {
	if [ -n "${JIDE_LIBGMP_A:-}" ]; then
		[ -f "$JIDE_LIBGMP_A" ] ||
			refuse "JIDE_LIBGMP_A names $JIDE_LIBGMP_A, which is not a file"
		printf '%s\n' "$JIDE_LIBGMP_A"
		return 0
	fi
	if command -v pkg-config >/dev/null 2>&1; then
		libdir=$(pkg-config --variable=libdir gmp 2>/dev/null) || libdir=
		if [ -n "$libdir" ] && [ -f "$libdir/libgmp.a" ]; then
			printf '%s\n' "$libdir/libgmp.a"
			return 0
		fi
	fi
	for candidate in \
		/opt/homebrew/opt/gmp/lib/libgmp.a \
		/opt/homebrew/lib/libgmp.a \
		/usr/local/opt/gmp/lib/libgmp.a \
		/usr/local/lib/libgmp.a \
		/usr/lib/libgmp.a \
		/usr/lib/x86_64-linux-musl/libgmp.a \
		/usr/lib/aarch64-linux-musl/libgmp.a; do
		if [ -f "$candidate" ]; then
			printf '%s\n' "$candidate"
			return 0
		fi
	done
	refuse "no libgmp.a on this host; set JIDE_LIBGMP_A to a static GMP archive"
}

container_runtime() {
	for candidate in ${JIDE_CONTAINER:-docker podman}; do
		command -v "$candidate" >/dev/null 2>&1 || continue
		if "$candidate" info >/dev/null 2>&1; then
			printf '%s\n' "$candidate"
			return 0
		fi
	done
	return 1
}

host_is_musl() {
	[ "$(uname -s)" = Linux ] || return 1
	if command -v ldd >/dev/null 2>&1; then
		ldd --version 2>&1 | grep -q musl && return 0
	fi
	ls /lib/ld-musl-*.so.1 >/dev/null 2>&1
}

host_arch() {
	case $(uname -m) in
	x86_64 | amd64) printf 'x86_64\n' ;;
	aarch64 | arm64) printf 'aarch64\n' ;;
	*) printf '%s\n' "$(uname -m)" ;;
	esac
}

build_linux() {
	local platform=$1
	local out=$2
	local want_arch flags runtime image
	case $platform in
	linux-x86_64) want_arch=x86_64 ;;
	linux-aarch64) want_arch=aarch64 ;;
	esac
	flags="-ccopt -static -ccopt -no-pie"

	[ -d "$here/vendor/jasmin" ] ||
		refuse "vendor/jasmin is not assembled; run vendor/fetch.sh first, so the build links the Jasmin library from the tree"

	if host_is_musl; then
		[ "$(host_arch)" = "$want_arch" ] ||
			refuse "$platform needs a $want_arch host; this one is $(uname -m)"
		say "building $platform natively on this musl host"
		stage=$(mktemp -d "${TMPDIR:-/tmp}/jasmin-static.XXXXXX")
		write_flags_workspace "$stage/dune-workspace" $flags
		if ! run_dune "$stage/dune-workspace" "$stage/build" \
			lsp-server/native/main.exe; then
			printf '%s: the build failed; nothing was written to %s\n' \
				"${0##*/}" "$out" >&2
			return 1
		fi
		cp "$stage/build/default/lsp-server/native/main.exe" "$out"
		chmod 755 "$out"
		return 0
	fi

	runtime=$(container_runtime) ||
		refuse "this host is $(uname -s)/$(uname -m) with a non-musl C library and no container runtime whose daemon answers (tried docker and podman). A musl static link cannot be made here, and a glibc or libSystem link is not this release file; start a runtime and build the image of scripts/static-build.Dockerfile, or run this on an Alpine host"
	image=${JIDE_STATIC_IMAGE:-jasmin-ide-static-build:latest}
	if ! "$runtime" image inspect "$image" >/dev/null 2>&1; then
		refuse "the image $image is not present, and this script never pulls one: build it with scripts/static-build.Dockerfile, or set JIDE_STATIC_IMAGE to an image that is already there"
	fi

	stage=$(mktemp -d "${TMPDIR:-/tmp}/jasmin-static.XXXXXX")
	mkdir "$stage/out"
	write_flags_workspace "$stage/dune-workspace" $flags

	say "building $platform in $image ($runtime, linux/$([ "$want_arch" = x86_64 ] && printf amd64 || printf arm64))"
	if ! "$runtime" run --rm \
		--platform "linux/$([ "$want_arch" = x86_64 ] && printf amd64 || printf arm64)" \
		-v "$here:/src:ro" \
		-v "$stage/dune-workspace:/dune-workspace:ro" \
		-v "$stage/out:/out" \
		-w /src \
		-e "SOURCE_DATE_EPOCH=$epoch" \
		"$image" sh -c '
			set -eu
			/opt/static/run-dune build --root /src --profile release \
				--build-dir /tmp/build --workspace /dune-workspace \
				-j '"$jobs"' lsp-server/native/main.exe
			cp /tmp/build/default/lsp-server/native/main.exe /out/server
		'; then
		printf '%s: the container build failed; nothing was written to %s\n' \
			"${0##*/}" "$out" >&2
		return 1
	fi
	[ -f "$stage/out/server" ] ||
		refuse "the container reported success but wrote no binary"
	cp "$stage/out/server" "$out"
	chmod 755 "$out"
}

build_macos() {
	local out=$1
	local libgmp_a

	[ "$(uname -s)" = Darwin ] ||
		refuse "the macOS build needs a macOS host; this one is $(uname -s)"
	case $(uname -m) in
	arm64) ;;
	*) refuse "the macOS release target is arm64; this host is $(uname -m)" ;;
	esac
	[ -d "$here/vendor/jasmin" ] ||
		refuse "vendor/jasmin is not assembled; run vendor/fetch.sh first, so the build links the Jasmin library from the tree"

	libgmp_a=$(find_libgmp_a)
	say "static GMP: $libgmp_a"

	stage=$(mktemp -d "${TMPDIR:-/tmp}/jasmin-static.XXXXXX")

	mkdir "$stage/gmp"
	ln -s "$libgmp_a" "$stage/gmp/libgmp.a"

	write_flags_workspace "$stage/dune-workspace" \
		"-ccopt" "-L$stage/gmp" "-cclib" "-lgmp"
	say "building macos-aarch64 against $libgmp_a"
	if ! run_dune "$stage/dune-workspace" "$stage/build" \
		lsp-server/native/main.exe; then
		printf '%s: the build failed; nothing was written to %s\n' \
			"${0##*/}" "$out" >&2
		return 1
	fi
	cp "$stage/build/default/lsp-server/native/main.exe" "$out"
	chmod 755 "$out"
}

main() {
	local platform out staged
	[ $# -ge 1 ] && [ $# -le 2 ] || usage
	platform=$1
	case $platform in
	linux-x86_64 | linux-aarch64 | macos-aarch64) ;;
	*) usage ;;
	esac

	if [ $# -eq 2 ] && [ -n "$2" ]; then
		out=$2
	else
		out=$here/.artifacts/static/jasmin-lsp-$platform
	fi
	case $out in
	/*) ;;
	*) out=$here/$out ;;
	esac

	mkdir -p "$(dirname -- "$out")"
	staged=$out.partial
	rm -f "$staged"

	case $platform in
	linux-*) build_linux "$platform" "$staged" || return 1 ;;
	macos-*) build_macos "$staged" || return 1 ;;
	esac

	[ -f "$staged" ] || refuse "the build reported success but wrote no binary"

	say "checking the linked libraries of $platform"
	if ! "$here/scripts/check-linked.sh" "$staged" "${platform%%-*}"; then
		rm -f "$staged"
		printf '%s: %s links a library it may not, so no release binary was written\n' \
			"${0##*/}" "$platform" >&2
		return 1
	fi

	mv "$staged" "$out"
	say "wrote $out"
}

main "$@"
