.PHONY: build clean test clean all-supported-ocaml-versions

build:
	dune build

test:
	dune runtest

js-test:
	dune build @runjstest

clean:
	dune clean

all-supported-ocaml-versions:
	dune build @install @runtest --workspace jbuild-workspace.dev
