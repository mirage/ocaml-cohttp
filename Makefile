.PHONY: build clean test clean

build:
	dune build

build-win32:
	dune build --only-packages cohttp-lwt-jsoo,cohttp-lwt-unix,cohttp-lwt,cohttp-mirage,cohttp-top,cohttp

test:
	dune runtest

test-win32:
	dune runtest --only-packages cohttp-lwt-jsoo,cohttp-lwt-unix,cohttp-lwt,cohttp-mirage,cohttp-top,cohttp

js-test:
	dune build @runjstest

js-test-win32:
	dune build @runjstest --only-packages cohttp-lwt-jsoo,cohttp-lwt-unix,cohttp-lwt,cohttp-mirage,cohttp-top,cohttp

clean:
	dune clean

