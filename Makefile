DUNE ?= dune
PNPM ?= pnpm
CARGO ?= $(HOME)/.cargo/bin/cargo

.PHONY: all build test ci fmt editor-code editor-zed check gens

all: build

build:
	$(DUNE) build @install

test:
	$(DUNE) runtest

ci: build test editor-code

editor-code:
	cd editor/code && $(PNPM) install --frozen-lockfile && $(PNPM) run validate

editor-zed:
	cd editor/zed && $(CARGO) build --locked --target wasm32-wasip2

fmt:
	$(DUNE) build @fmt --auto-promote

check:
	$(DUNE) build @check
