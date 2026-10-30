OPAMJOBS ?= 4
OPAM ?= opam
OPAM_SWITCH ?= jasmin-ide

DUNE ?= $(shell if command -v dune >/dev/null 2>&1; then echo dune; \
	elif command -v opam >/dev/null 2>&1; then \
		echo 'opam exec --switch $(OPAM_SWITCH) -- dune'; \
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

RUN_OPAM := $(if $(strip $(HEAVY)),$(strip $(HEAVY)) ,)$(OPAM)

export OPAMJOBS HEAVY

CI := sh scripts/ci.sh

.PHONY: all deps vendor build fmt test check editor-code editor-zed ci ci-fast

.NOTPARALLEL:

all: build

deps:
	@$(CI) deps jasmin-lsp.opam.locked '$(RUN_OPAM) install --switch $(OPAM_SWITCH) --locked --deps-only -y ./jasmin-lsp.opam.locked && $(RUN_OPAM) install --switch $(OPAM_SWITCH) --deps-only -y ./vendor/requirements.opam'

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
