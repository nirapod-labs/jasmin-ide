OPAMJOBS ?= 4
OPAM ?= opam
OPAM_SWITCH ?= jasmin-ide

DUNE ?= $(shell if command -v dune >/dev/null 2>&1; then echo dune; \
	elif command -v opam >/dev/null 2>&1; then \
		echo 'opam exec --switch $(OPAM_SWITCH) -- dune'; \
	else echo dune; fi)

PNPM ?= pnpm

REPO_DIR := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))

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

.PHONY: all deps vendor build fmt test check bench bench-build editor-code editor-vscode editor-zed zed-queries ci ci-fast

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

TEST_ARGS := runtest --root . --force -j 4

BENCH_EXE := $(REPO_DIR)/_build/default/bench/bench.exe

bench-build:
	@$(CI) bench-build bench/dune '$(RUN_DUNE) build --root . bench/bench.exe -j 4'

bench:
	@JASMIN_BENCH_EXE=$(BENCH_EXE) $(CI) bench bench/run.sh 'sh bench/run.sh'

test:
	@$(CI) test dune-project 'sh scripts/runtest.sh "$(RUN_DUNE)" $(TEST_ARGS)'

editor-code:
	@$(CI) editor-code editor/code/package.json 'cd editor/code && $(HEAVY) $(PNPM) install --frozen-lockfile && $(PNPM) run validate'

editor-vscode:
	@if [ ! -f editor/code/package.json ]; then \
		printf 'skip editor-vscode (not present)\n'; \
	elif sh scripts/vscode-available.sh; then \
		$(CI) editor-vscode editor/code/package.json \
			'cd editor/code && $(HEAVY) $(PNPM) install --frozen-lockfile && $(HEAVY) $(PNPM) run test:vscode'; \
	else \
		printf 'skip editor-vscode (VS Code cannot be downloaded)\n'; \
	fi

editor-zed:
	@$(CI) editor-zed editor/zed/Cargo.toml 'cd editor/zed && $(RUN_CARGO) build --locked --target wasm32-wasip2'

GRAMMAR_DIR ?= $(if $(strip $(JASMIN_GRAMMAR_DIR)),$(strip $(JASMIN_GRAMMAR_DIR)),$(abspath $(REPO_DIR)/../grammar/tree-sitter-jasmin))

zed-queries:
	@if [ ! -f editor/zed/test/queries.sh ]; then \
		printf 'skip zed-queries (not present)\n'; \
	elif [ ! -f "$(GRAMMAR_DIR)/src/parser.c" ]; then \
		printf 'skip zed-queries (no grammar)\n'; \
	else \
		$(CI) zed-queries "$(GRAMMAR_DIR)/src/parser.c" 'cd editor/zed && JASMIN_GRAMMAR_DIR="$(GRAMMAR_DIR)" ./test/queries.sh'; \
	fi

ci: vendor build fmt test editor-code editor-vscode editor-zed zed-queries

ci-fast: build test
