HS_FILES := $(shell git ls-files '*.hs')

.PHONY: format
format:
	@test -n "$(HS_FILES)" || { echo "No tracked .hs files found"; exit 0; }
	fourmolu -i $(HS_FILES)

.PHONY: format-check
format-check:
	@test -n "$(HS_FILES)" || { echo "No tracked .hs files found"; exit 0; }
	fourmolu -m check $(HS_FILES)

.PHONY: cabal-check
cabal-check:
	cabal check

.PHONY: cabal-update
cabal-update:
	cabal update

.PHONY: configure
configure:
	cabal configure --enable-tests --enable-benchmarks --disable-documentation
	cabal build --dry-run

.PHONY: deps
deps:
	cabal build --only-dependencies

.PHONY: build
build:
	cabal build all

.PHONY: test
test:
	cabal test all

.PHONY: docs
docs:
	cabal haddock all --disable-documentation

.PHONY: ci
ci: format-check cabal-check cabal-update configure deps build test docs
