.PHONY: build clean test clean

build:
	dune build @install

test:
	dune runtest

js-test:
	dune build @runjstest

clean:
	dune clean
