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

RUN apk add --no-cache \
	build-base \
	musl-dev \
	gmp-static \
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
RUN opam init --bare --disable-sandboxing --no-setup && \
	opam switch create jasmin-ide ocaml-system && \
	opam install --switch jasmin-ide --yes \
	dune \
	ocamlfind \
	conf-gmp \
	zarith \
	lsp \
	jsonrpc \
	yojson \
	cmdliner \
	crowbar \
	qcheck-core && \
	opam exec --switch jasmin-ide -- dune --version

RUN printf '%s\n' \
	'#!/bin/sh' \
	'set -eu' \
	'exec opam exec --switch jasmin-ide -- dune "$@"' \
	>/opt/static/run-dune && \
	chmod 755 /opt/static/run-dune

WORKDIR /src

# The tree is mounted at build time, read-only, and the build directory lives
# in the container's own filesystem; nothing is written back into the checkout.
CMD ["/opt/static/run-dune", "--version"]
