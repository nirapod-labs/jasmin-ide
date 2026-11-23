# Build, test and check the language server and the editor extensions.
# docs/development.md says what each target needs; docs/release.md covers the
# release targets.

OPAM_SWITCH ?= jasmin-ide
DUNE ?= $(shell command -v dune >/dev/null 2>&1 && echo dune || echo 'opam exec --switch $(OPAM_SWITCH) -- dune')
PNPM ?= pnpm
# The rustup cargo when there is one, because it has the wasm target.
CARGO ?= $(shell [ -x $(HOME)/.cargo/bin/cargo ] && echo 'env PATH=$(HOME)/.cargo/bin:$$PATH cargo' || echo cargo)
# A command to queue the heavy steps behind; empty runs them directly.
HEAVY ?=
JOBS ?= 4

# What the corpus checks compare against. None of it is in this repository.
CORPUS ?=
JASMINC ?=
JASMIN2TEX ?=
CHECKS ?=
JASMIN_TM_CORPUS ?=
JASMIN_GRAMMAR_DIR ?=
JASMIN_ALLOWED_SKIPS ?=

BIN := _build/default
D := $(strip $(HEAVY) $(DUNE))
HARNESSES := $(D) build --root . @install test bench fuzz -j $(JOBS)
comma := ,

# $(call needs,VARIABLES,COMMAND) runs COMMAND when every variable is set and
# otherwise prints the ones the target needs. A variable that is set but wrong
# is refused by the command itself, so a typo never passes as a skip. With
# STRICT=1, as in CI, a missing variable fails instead of skipping.
STRICT ?=
missing = $(strip $(foreach v,$(1),$(if $(strip $($(v))),,$(v))))
needs = $(if $(call missing,$(1)),$(if $(STRICT),@echo '$@ needs $(call missing,$(1))' >&2; exit 1,@echo 'skip $@ (needs $(call missing,$(1)))'),$(2))

.PHONY: all deps vendor build fmt test version-check ci oracle \
	editor-code editor-vscode editor-zed zed-queries workflows \
	parity parity-tree parity-uses parity-mutants calls completion-scope \
	intrinsics intrinsic-arity rename mutation context bench-build bench fuzz \
	release release-static release-check release-sums release-verify-sums \
	release-double-build

all: build

deps:
	$(HEAVY) opam install --switch $(OPAM_SWITCH) --locked --deps-only -y ./jasmin-lsp.opam.locked
	$(HEAVY) opam install --switch $(OPAM_SWITCH) --deps-only -y ./vendor/requirements.opam

vendor:
	sh vendor/fetch.sh

build: vendor
	$(D) build --root . @install -j $(JOBS)

fmt:
	$(D) build --root . @fmt -j $(JOBS)

# --force reruns the test actions; a new JIDE_TEST_RUN reruns the cram tests,
# which dune would otherwise replay (see the root dune file).
RUN_ID = JIDE_TEST_RUN=$$(date +%s)-$$$$
test:
	$(RUN_ID) $(D) build --root . @runtest --force -j $(JOBS)

version-check:
	sh scripts/check-version.sh

# Every step in order; the first failure stops the run. A step skips only
# when its input is not set, and says so. editor-vscode never skips unless
# JASMIN_ALLOWED_SKIPS names it.
ci: version-check build bench-build fmt test oracle editor-code editor-vscode editor-zed zed-queries workflows

oracle:
	$(call needs,JASMINC,$(JASMINC) -version)

editor-code:
	$(call needs,JASMIN_TM_CORPUS,cd editor/code && $(HEAVY) $(PNPM) install --frozen-lockfile && JASMIN_TM_CORPUS="$(JASMIN_TM_CORPUS)" JASMIN_TM_CORPUS_REQUIRED=1 $(PNPM) run validate)

editor-vscode:
ifneq ($(filter editor-vscode,$(subst $(comma), ,$(JASMIN_ALLOWED_SKIPS))),)
	@echo 'skip editor-vscode (allowed by JASMIN_ALLOWED_SKIPS)'
else
	cd editor/code && $(HEAVY) $(PNPM) install --frozen-lockfile && $(HEAVY) $(PNPM) run test:vscode
endif

editor-zed:
	cd editor/zed && $(CARGO) build --locked --target wasm32-wasip2

zed-queries:
	$(call needs,JASMIN_GRAMMAR_DIR,cd editor/zed && JASMIN_GRAMMAR_DIR="$(JASMIN_GRAMMAR_DIR)" ./test/queries.sh)

workflows:
	STRICT=$(STRICT) sh scripts/check-workflows.sh

# The corpus checks. Each one's expected side is the compiler, or the library
# the server links, and each runs its own negative control.
parity:
	$(call needs,CORPUS JASMINC,$(HARNESSES) && $(BIN)/test/parity/parity.exe --corpus "$(CORPUS)" --jasminc "$(JASMINC)" --jobs $(JOBS)$(if $(CHECKS), --checks "$(CHECKS)"))

parity-tree:
	$(call needs,CORPUS JASMIN2TEX,$(HARNESSES) && $(BIN)/test/parity/parity_tree.exe --corpus "$(CORPUS)" --jasmin2tex "$(JASMIN2TEX)" --jobs $(JOBS)$(if $(CHECKS), --checks "$(CHECKS)"))

parity-uses:
	$(call needs,CORPUS,$(HARNESSES) && $(BIN)/test/parity/typed_uses.exe --corpus "$(CORPUS)" --jobs $(JOBS)$(if $(CHECKS), --checks "$(CHECKS)"))

parity-mutants:
	$(call needs,CORPUS JASMINC,$(HARNESSES) && $(BIN)/test/parity/mutate.exe --corpus "$(CORPUS)" --jasminc "$(JASMINC)" --defects test/parity/compiler-defects.tsv --jobs $(JOBS))

calls:
	$(call needs,CORPUS,$(HARNESSES) && $(BIN)/test/parity/calls.exe --corpus "$(CORPUS)")

completion-scope:
	$(call needs,CORPUS JASMINC,$(HARNESSES) && $(BIN)/test/parity/completion_scope.exe --corpus "$(CORPUS)" --compiler "$(JASMINC)" --limit 1000000)

intrinsics:
	$(call needs,JASMINC,$(HARNESSES) && $(BIN)/test/parity/intrinsics.exe --compiler "$(JASMINC)" --jobs $(JOBS))

intrinsic-arity:
	$(call needs,CORPUS JASMINC,$(HARNESSES) && $(BIN)/test/intrinsic/arity.exe --jasminc "$(JASMINC)" "$(CORPUS)")

rename:
	$(call needs,CORPUS JASMINC,$(HARNESSES) && $(BIN)/test/parity/rename_trip.exe --corpus "$(CORPUS)" --jasminc "$(JASMINC)" --jobs $(JOBS))

# CORPUS may name several directories here, separated by spaces.
mutation:
	$(call needs,CORPUS,$(HARNESSES) && $(BIN)/test/mutation/mutation.exe $(CORPUS) --sample 200 --sites 32)

context:
	$(call needs,CORPUS,JASMIN_CONTEXT_CORPUS="$(CORPUS)" $(RUN_ID) $(D) build --root . @test/context/runtest --force -j $(JOBS))

bench-build:
	$(D) build --root . bench/bench.exe -j $(JOBS)

bench:
	$(call needs,CORPUS,JASMIN_BENCH_CORPUS="$(CORPUS)" sh bench/run.sh)

FUZZ_SECONDS ?= 120
FUZZ_SEED ?= 1
fuzz:
	$(HARNESSES) && $(BIN)/fuzz/fuzz_main.exe --seconds $(FUZZ_SECONDS) --seed $(FUZZ_SEED)$(if $(CORPUS), --corpus "$(CORPUS)")

# Release files; docs/release.md is the whole process.
VERSION := $(shell sed -n 's/^let version = "\(.*\)"$$/\1/p' controller/version.ml)
HOST := $(shell uname -sm | tr ' ' -)
PLATFORM ?= $(if $(filter Darwin-arm64,$(HOST)),macos-aarch64,$(if $(filter Linux-x86_64,$(HOST)),linux-x86_64,$(if $(filter Linux-aarch64,$(HOST)),linux-aarch64)))
RELEASE_DIR ?= .artifacts/release
ASSET := $(RELEASE_DIR)/jasmin-lsp-$(VERSION)-$(PLATFORM)
TREE_A ?=
TREE_B ?=

release: release-static release-sums

release-static: vendor
	sh scripts/build-static.sh $(PLATFORM) $(ASSET)

release-check:
	sh scripts/check-linked.sh $(ASSET) $(word 1,$(subst -, ,$(PLATFORM)))

release-sums:
	sh scripts/checksums.sh write $(RELEASE_DIR)

release-verify-sums:
	sh scripts/checksums.sh verify $(RELEASE_DIR)

release-double-build:
	$(call needs,TREE_A TREE_B,sh scripts/release-double-build.sh "$(TREE_A)" "$(TREE_B)" .artifacts/double-build --require-identical)
