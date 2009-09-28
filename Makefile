include Makefile.defs
export SHELL=/bin/bash

MODULES = \
	http_constants http_types http_parser_sanity http_misc http_common \
	http_tcp_server http_parser http_message http_request http_daemon \
	http_response http_user_agent

THREADED_SRV = http_threaded_tcp_server
MODULES_MT = $(patsubst http_tcp_server, mt/$(THREADED_SRV) http_tcp_server, $(MODULES))
MODULES_NON_MT = $(patsubst http_tcp_server, non_mt/$(THREADED_SRV) http_tcp_server, $(MODULES))
PUBLIC_MODULES = \
	http_types	\
	http_common	\
	http_message	\
	http_request	\
	http_daemon	\
	http_response	\
	http_user_agent
OCAMLDOC_STUFF = *.mli
DOCDIR = doc/html
DOTDIR = doc/dot
TEXDIR = doc/latex
DESTDIR = $(shell $(OCAMLFIND) printconf destdir) 

all: all_non_mt all_mt
opt: opt_non_mt opt_mt
all_non_mt: http.cma
opt_non_mt: http.cmxa
all_mt: http_mt.cma
opt_mt: http_mt.cmxa
world: all opt
doc: all $(DOCDIR)/index.html $(DOTDIR)/ocaml-http.ps $(TEXDIR)/ocaml-http.ps $(OCAMLDOC_STUFF)
$(DOCDIR)/index.html:
	$(OCAMLDOC) -html -d $(DOCDIR) $(OCAMLDOC_STUFF)
$(TEXDIR)/ocaml-http.tex: $(OCAMLDOC_STUFF)
	$(OCAMLDOC) -latex -o $@ $^
$(TEXDIR)/ocaml-http.ps: $(TEXDIR)/ocaml-http.tex
	cd $(TEXDIR);		\
	latex ocaml-http;	\
	latex ocaml-http;	\
	dvips ocaml-http
$(DOTDIR)/ocaml-http.ps: $(DOTDIR)/ocaml-http.dot
	$(DOT) -Tps $< > $@
$(DOTDIR)/ocaml-http.dot: *.ml *.mli
	$(OCAMLDOC) -dot -o $(DOTDIR)/ocaml-http.dot *.ml *.mli

examples:
	$(MAKE) -C examples/
examples.opt:
	$(MAKE) -C examples/ opt

include .depend

depend:
	$(OCAMLDEP) *.ml *.mli > .depend

%.cmi: %.mli
	$(OCAMLC) -c $<
%.cmo: %.ml %.cmi
	$(OCAMLC) -c $<
%.cmx: %.ml %.cmi
	$(OCAMLOPT) -c $<

non_mt/$(THREADED_SRV).cmo: non_mt/$(THREADED_SRV).ml $(THREADED_SRV).cmi
	cp $(THREADED_SRV).{cmi,mli} non_mt/
	$(OCAMLC) -c $<
non_mt/$(THREADED_SRV).cmx: non_mt/$(THREADED_SRV).ml $(THREADED_SRV).cmi
	cp $(THREADED_SRV).{cmi,mli} non_mt/
	$(OCAMLOPT) -c $<

mt/$(THREADED_SRV).cmo: mt/$(THREADED_SRV).ml $(THREADED_SRV).cmi
	cp $(THREADED_SRV).{cmi,mli} mt/
	$(OCAMLC) $(THREADS_FLAGS) -c $<
mt/$(THREADED_SRV).cmx: mt/$(THREADED_SRV).ml $(THREADED_SRV).cmi
	cp $(THREADED_SRV).{cmi,mli} mt/
	$(OCAMLOPT) $(THREADS_FLAGS) -c $<

http.cma: $(patsubst %,%.cmo,$(MODULES_NON_MT))
	$(OCAMLC) -a -o $@ $^
http.cmxa: $(patsubst %,%.cmx,$(MODULES_NON_MT))
	$(OCAMLOPT) -a -o $@ $^
http_mt.cma: $(patsubst %,%.cmo,$(MODULES_MT))
	$(OCAMLC) -a -o $@ $^
http_mt.cmxa: $(patsubst %,%.cmx,$(MODULES_MT))
	$(OCAMLOPT) -a -o $@ $^

meta: META
META: META.in
	cat META.in | sed -e 's/@DISTVERSION@/$(DISTVERSION)/' > META

clean:
	$(MAKE) -C examples/ clean
	for d in . mt non_mt; do	\
		rm -f $$d/*.cm[ioax] $$d/*.cmxa $$d/*.[ao] $$d/test{,.opt};	\
	done
	rm -f {mt,non_mt}/$(THREADED_SRV).mli
docclean:
	-rm -f	\
		$(DOCDIR)/*.html $(DOCDIR)/*.css	\
		$(DOTDIR)/*.dot $(DOTDIR)/*.ps	\
		$(TEXDIR)/*.{dvi,ps,ps.gz,pdf,aux,log,out,toc,tmp,haux,sty,tex}
distclean: clean
	$(MAKE) -C examples/ distclean
	rm -f META
dist: distreal distrm
distdoc: all doc
	if [ -d $(DISTDIR) ]; then rm -rf $(DISTDIR); else true; fi
	mkdir -p $(DISTDIR)/doc/
	cp -r doc/html/ $(DISTDIR)/doc/
	cp doc/dot/ocaml-http.ps $(DISTDIR)/doc/modules.ps
	cp doc/latex/ocaml-http.ps $(DISTDIR)/doc/
distreal: distdoc distclean depend
	for f in	\
			$(patsubst %, %.ml, $(MODULES))	\
			$(patsubst %, %.mli, $(MODULES) $(THREADED_SRV))	\
			mt/ non_mt/ $(EXTRA_DIST) examples/ debian/;	\
	do	\
		cp -r $$f $(DISTDIR)/;	\
	done
	-find $(DISTDIR)/ -type d -name .svn -exec rm -rf {} \;
	tar cvzf $(DISTDIR).tar.gz $(DISTDIR)/
distrm:
	rm -rf $(DISTDIR)/
deb: docclean distreal
	(cd $(DISTDIR)/ && debuild)
	rm -rf $(DISTDIR)/
install: META
	$(OCAMLFIND) install -destdir $(DESTDIR) $(PKGNAME)	\
		$(patsubst %, %.mli, $(PUBLIC_MODULES))	\
		$(patsubst %, %.cmi, $(PUBLIC_MODULES))	\
		$(wildcard *.cma *.cmxa *.a) META

.PHONY:	\
	all opt world all_non_mt all_mt opt_non_mt opt_mt	\
	examples examples.opt depend clean distclean dist	\
	install meta doc deb distreal distrm
