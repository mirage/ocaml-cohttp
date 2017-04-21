all:
	jbuilder build @install -j 3

tests:
	jbuilder runtest

check: tests

doc: all
	$(MAKE) -C doc

clean:
	rm -rf _build
	$(MAKE) -C doc clean

.PHONY: all tests doc clean check
