.PHONY: build clean test clean eio eio-shell eio-test fmt js-test

build:
	dune build

test:
	dune runtest

js-test:
	dune build @runjstest

clean:
	dune clean

fmt:
	dune b @fmt --auto-promote

eio: #build eio
	dune build cohttp-eio

eio-test:
	dune runtest cohttp-eio

eio-shell: # nix-shell for eio dev
	nix develop .#eio
