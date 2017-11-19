.PHONY: build clean test install uninstall clean all-supported-ocaml-versions

build:
	jbuilder build --dev @install

test:
	jbuilder runtest

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	rm -rf _build *.install

REPO=../../mirage/mirage-dev
PACKAGES=$(REPO)/packages
# until we have https://github.com/ocaml/opam-publish/issues/38
pkg-%:
	topkg opam pkg -n $*
	mkdir -p $(PACKAGES)/$*
	cp -r _build/$*.* $(PACKAGES)/$*/
	cd $(PACKAGES) && git add $*

all-supported-ocaml-versions:
	jbuilder build --dev @install @runtest --workspace jbuild-workspace.dev

PKGS=$(basename $(wildcard *.opam))
opam-pkg:
	$(MAKE) $(PKGS:%=pkg-%)


