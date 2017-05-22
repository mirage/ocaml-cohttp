all:
	jbuilder build @install -j 3

tests:
	jbuilder runtest

check: tests

clean:
	rm -rf _build

.PHONY: all tests doc clean check
