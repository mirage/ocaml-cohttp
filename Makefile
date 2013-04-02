.PHONY: all clean install build
all: build test doc

PREFIX ?= /usr/local
NAME=cohttp

ASYNC=--enable-async
ifneq ($(MIRAGE_OS),xen)
TESTS ?= --enable-tests
endif
# disabled by default as they hang at the moment for Async
# NETTESTS ?= --enable-nettests

setup.bin: setup.ml
	ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	./setup.bin -configure $(LWT) $(ASYNC) $(MIRAGE) $(TESTS) $(NETTESTS) --prefix $(PREFIX)

build: setup.data setup.bin
	./setup.bin -build -classic-display

doc: setup.data setup.bin
	./setup.bin -doc

install: setup.bin
	./setup.bin -install

test: setup.bin build
	./setup.bin -test

fulltest: setup.bin build
	./setup.bin -test

reinstall: setup.bin
	ocamlfind remove $(NAME) || true
	./setup.bin -reinstall

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log setup.bin
