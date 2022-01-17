.PHONY: build clean test clean

build:
	dune build

test:
	dune runtest

js-test:
	dune build @runjstest

clean:
	dune clean

.PHONY: nix/opam-selection.nix
nix/opam-selection.nix:
	nix-shell -A resolve default.nix
