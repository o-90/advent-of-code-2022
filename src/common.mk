SHELL := /bin/bash

GHC := ghc
GHCFLAGS := -Wall -O1
BIN := run

build:
	$(GHC) $(GHCFLAGS) -o $(BIN) $(SRC)

clean:
	rm -r $(BIN) || true
	rm -r *.o || true
	rm -r *.hi || true