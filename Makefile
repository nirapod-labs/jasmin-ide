OPAMJOBS ?= 4

DUNE ?= $(shell if command -v dune >/dev/null 2>&1; then echo dune; \
	elif command -v opam >/dev/null 2>&1; then \
		if opam switch list --short 2>/dev/null | grep -qx jasmin-ide; then \
			echo 'opam exec --switch jasmin-ide -- dune'; \
		else echo dune; fi; \
	else echo dune; fi)

PNPM ?= pnpm

RUSTUP_BIN := $(HOME)/.cargo/bin
CARGO ?= $(shell if [ -x $(RUSTUP_BIN)/cargo ]; then echo $(RUSTUP_BIN)/cargo; \
	else echo cargo; fi)

RUN_CARGO := $(if $(wildcard $(RUSTUP_BIN)/rustc),PATH=$(RUSTUP_BIN):$$PATH ,)$(CARGO)

HEAVY ?=
ifeq ($(strip $(HEAVY)),1)
$(error HEAVY names a runner command; set HEAVY=/path/to/runner, or leave it empty)
endif

RUN_DUNE := $(if $(strip $(HEAVY)),$(strip $(HEAVY)) ,)$(DUNE)

export OPAMJOBS HEAVY

CI := sh scripts/ci.sh

.PHONY: all vendor build fmt test check editor-code editor-zed ci ci-fast

.NOTPARALLEL:

all: build

vendor:
	@$(CI) vendor vendor/jasmin.lock 'sh vendor/fetch.sh'

build: vendor
	@$(CI) build dune-project '$(RUN_DUNE) build --root . @install -j 4'

fmt:
	@$(CI) fmt dune-project '$(RUN_DUNE) build --root . @fmt -j 4'

check: fmt

test:
	@$(CI) test dune-project '$(RUN_DUNE) runtest --root . -j 4'

editor-code:
	@$(CI) editor-code editor/code/package.json 'cd editor/code && $(HEAVY) $(PNPM) install --frozen-lockfile && $(PNPM) run validate'

editor-zed:
	@$(CI) editor-zed editor/zed/Cargo.toml 'cd editor/zed && $(RUN_CARGO) build --locked --target wasm32-wasip2'

ci: vendor build fmt test editor-code editor-zed

ci-fast: build test
