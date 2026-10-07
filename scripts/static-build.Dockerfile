# The image scripts/build-static.sh builds the Linux release binaries in.
#
# Alpine is the point: the release server is linked statically against musl,
# never against glibc. The dynamic Ubuntu build is not a release file — it
# needs a newer glibc than the oldest system the editors support — so the
# container this image describes is the only place the Linux binaries come
# from.
#
# Build it once on a host with a container runtime whose daemon answers:
#
#   docker build -t jasmin-ide-static-build:latest -f scripts/static-build.Dockerfile .
#
# The last layer writes /opt/static/run-dune, a one-line wrapper around dune,
# with the project switch named. The script calls it rather than `dune`
# directly so the image decides where the OCaml switch lives.
#
# The tag is the default of JIDE_STATIC_IMAGE in scripts/build-static.sh. This
# image is never pushed anywhere: it is built on the release host and used
# there.

FROM alpine:3.21

# gmp-dev is the archive the release links: on Alpine 3.21 it installs
# /usr/lib/libgmp.a as well as the shared library and the headers, which is
# what `-static` needs to resolve zarith's `-lgmp`. (`gmp-static` is not a
# package on either alpine 3.21/x86_64 or 3.21/aarch64 — `apk add gmp-static`
# fails with "no such package", measured on 2026-11-12 — so naming it would
# stop the image at its second layer.) `musl-dev` and `build-base` supply the
# C toolchain and musl's own headers, which is what makes `-ccopt -static`
# produce a musl binary rather than a glibc one.
RUN apk add --no-cache \
	build-base \
	musl-dev \
	gmp-dev \
	ocaml \
	ocaml-findlib \
	dune \
	opam \
	tar \
	bzip2 \
	ca-certificates

ENV OPAMROOT=/opt/opam \
	OPAMYES=1 \
	OPAMCOLOR=never

# One switch for the release build, with the packages the lock file names. It
# is created here so the container starts with a usable switch and the build
# step never installs anything (it has no network: the script mounts the tree
# read-only and the container is started without a network policy of its own,
# but nothing in the build needs one).
#
# menhir, menhirLib, angstrom and batteries are the vendored library's own
# build requirements (vendor/requirements.opam): the tree's dune files ask for
# the parser generator and for `angstrom` and `batteries.unthreaded`, so a
# switch without them fails to build the release binary at all.
#
# The libraries the server links are pinned to the versions the repository
# pins (jasmin-lsp.opam.locked and vendor/requirements.opam), so the Linux
# binaries are built from the same sources as the macOS one. Left unpinned,
# `lsp` and `jsonrpc` resolve to 1.28.1, whose `Client_notification.Initialized`
# takes an argument, and controller/server.ml does not compile against it.
#
# `--no-depexts` is deliberate: the system packages are the `apk add` layer
# above, and opam's depext check does not see them on alpine 3.21 — it stops
# on conf-gmp with "Package conf-gmp depends on the unavailable system package
# 'gmp-dev'" (measured on 2026-11-12, aarch64) even though gmp-dev is installed
# and /usr/lib/libgmp.a is there. Nothing is installed by the check that the
# layer has not already installed.
RUN opam init --bare --disable-sandboxing --no-setup && \
	opam switch create jasmin-ide ocaml-system && \
	opam install --switch jasmin-ide --yes --no-depexts \
	dune.3.24.2 \
	ocamlfind.1.9.8 \
	conf-gmp \
	zarith.1.14 \
	lsp.1.27.0 \
	jsonrpc.1.27.0 \
	yojson.3.0.0 \
	cmdliner \
	crowbar \
	qcheck-core \
	menhir.20260209 \
	menhirLib.20260209 \
	angstrom.0.16.1 \
	batteries.3.11.0 && \
	opam exec --switch jasmin-ide -- dune --version

# /opt/static does not exist in the base image: making it is part of writing the
# wrapper. (Without the mkdir the layer that follows the 155 s opam install
# fails with "/bin/sh: can't create /opt/static/run-dune: nonexistent
# directory", measured on 2026-11-12.)
RUN mkdir -p /opt/static && \
	printf '%s\n' \
	'#!/bin/sh' \
	'set -eu' \
	'exec opam exec --switch jasmin-ide -- dune "$@"' \
	>/opt/static/run-dune && \
	chmod 755 /opt/static/run-dune

WORKDIR /src

# The tree is mounted at build time, read-only, and the build directory lives
# in the container's own filesystem; nothing is written back into the checkout.
CMD ["/opt/static/run-dune", "--version"]
