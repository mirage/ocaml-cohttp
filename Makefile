.PHONY: all clean install build
all: build test doc

PREFIX ?= /usr/local
NAME=cohttp

LWT ?= $(shell if ocamlfind query lwt >/dev/null 2>&1; then echo --enable-lwt; fi)
LWT_UNIX ?= $(shell if ocamlfind query lwt.unix >/dev/null 2>&1; then echo --enable-lwt-unix; fi)
ASYNC ?= $(shell if ocamlfind query async >/dev/null 2>&1; then echo --enable-async; fi)
JS ?= $(shell if ocamlfind query js_of_ocaml >/dev/null 2>&1; then echo --enable-js; fi)

DOCDIR = docs
DOC_COHTTP = $(DOCDIR)/cohttp
DOC_ASYNC = $(DOCDIR)/async
DOC_LWT = $(DOCDIR)/cohttp_lwt
DOC_LWTUNIX = $(DOCDIR)/cohttp_lwt_unix

setup.bin: setup.ml
	ocamlopt.opt -o $@ $< 2>/dev/null || ocamlopt -o $@ $< 2>/dev/null || ocamlc -o $@ $<
	rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	./setup.bin -configure $(LWT) $(ASYNC) $(LWT_UNIX) $(JS) $(TESTS) --prefix $(PREFIX)

build: setup.data setup.bin
	./setup.bin -build -classic-display

doc: setup.data setup.bin
	./setup.bin -doc

github: doc
	git checkout gh-pages
	git merge master --no-edit
	$(MAKE)
	rm -Rf $(DOCDIR)
	mkdir -p $(DOC_COHTTP) $(DOC_ASYNC) $(DOC_LWT) $(DOC_LWTUNIX)
	cp _build/lib/cohttp.docdir/* $(DOC_COHTTP)
	cp _build/async/cohttp_async.docdir/* $(DOC_ASYNC)
	cp _build/lwt/cohttp_lwt.docdir/* $(DOC_LWT)
	cp _build/lwt/cohttp_lwt_unix.docdir/* $(DOC_LWTUNIX)
	git add $(DOCDIR)/*/*.html $(DOCDIR)/*/*.css
	git commit -m 'sync ocamldoc' *.html *.css
	git push
	git checkout master

install: setup.bin
	./setup.bin -install

test: setup.bin build
	./setup.bin -test

fulltest: setup.bin build
	./setup.bin -test

reinstall: setup.bin
	ocamlfind remove $(NAME) || true
	./setup.bin -reinstall

generate:
	cd scripts && ocaml generate.ml

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log setup.bin

revdep:
	opam switch system || true
	opam switch remove -y cohttp-revdeps || true
	opam switch -A system cohttp-revdeps
	opam pin -y add cohttp .
	for i in `opam list -s --rec --depends-on=cohttp`; do opam install -y -j 4 $$i; done
	opam switch system
