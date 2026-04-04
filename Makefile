HS_FILES := $(shell git ls-files '*.hs')

# BUILD_TOOL selects the Haskell build tool: cabal (default) or stack.
BUILD_TOOL ?= cabal

# --- Shared ---

.PHONY: format
format:
	@test -n "$(HS_FILES)" || { echo "No tracked .hs files found"; exit 0; }
	fourmolu -i $(HS_FILES)

.PHONY: format-check
format-check:
	@test -n "$(HS_FILES)" || { echo "No tracked .hs files found"; exit 0; }
	fourmolu -m check $(HS_FILES)

# --- Cabal ---

.PHONY: cabal-check
cabal-check:
	cabal check

.PHONY: cabal-update
cabal-update:
	cabal update

.PHONY: cabal-configure
cabal-configure:
	cabal configure --enable-tests --enable-benchmarks --disable-documentation $(CABAL_CONFIGURE_FLAGS)
	cabal build --dry-run

.PHONY: cabal-deps
cabal-deps:
	cabal build --only-dependencies

.PHONY: cabal-build
cabal-build:
	cabal build all

.PHONY: cabal-test
cabal-test:
	cabal test all

.PHONY: cabal-docs
cabal-docs:
	cabal haddock all --disable-documentation

.PHONY: cabal-ci
cabal-ci: format-check cabal-check cabal-update cabal-configure cabal-deps cabal-build cabal-test cabal-docs

# --- Stack ---

.PHONY: stack-update
stack-update:
	stack update

.PHONY: stack-deps
stack-deps:
	stack build --only-dependencies --test --no-run-tests

.PHONY: stack-build
stack-build:
	stack build

.PHONY: stack-test
stack-test:
	stack test

.PHONY: stack-docs
stack-docs:
	stack haddock --no-haddock-deps

.PHONY: stack-ci
stack-ci: format-check stack-update stack-deps stack-build stack-test stack-docs

# ── Generic (delegates to BUILD_TOOL) ────────────────────────────────

.PHONY: update
update: $(BUILD_TOOL)-update

.PHONY: deps
deps: $(BUILD_TOOL)-deps

.PHONY: build
build: $(BUILD_TOOL)-build

.PHONY: test
test: $(BUILD_TOOL)-test

.PHONY: docs
docs: $(BUILD_TOOL)-docs

.PHONY: ci
ci: $(BUILD_TOOL)-ci
