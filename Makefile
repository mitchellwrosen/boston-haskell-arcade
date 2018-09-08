.PHONY: docs run

docs:
	cabal new-haddock

run:
	cabal new-run
