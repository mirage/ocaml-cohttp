include ../Makefile.defs
OBJS_NON_MT = ../http.cma
OBJS_NON_MT_OPT = ../http.cmxa
OBJS_MT = ../http_mt.cma
OBJS_MT_OPT = ../http_mt.cmxa
EXAMPLES_FLAGS = -I .. -linkpkg

EXAMPLES :=	\
	always_ok_daemon.ml	\
	basic_auth.ml		\
	chdir.ml		\
	client_address.ml	\
	damned_recursion.ml	\
	dump_args.ml		\
	highlander.ml		\
	oo_daemon.ml		\
	threads.ml		\
	timeout.ml		\
	webfsd.ml
EXAMPLES := $(patsubst %.ml,%,$(EXAMPLES))

all: $(EXAMPLES)
opt: $(patsubst %,%.opt,$(EXAMPLES))
%: %.ml $(OBJS_NON_MT)
	$(OCAMLC) $(EXAMPLES_FLAGS) $(OBJS_NON_MT) -o $@ $<
%.opt: %.ml $(OBJS_NON_MT_OPT)
	$(OCAMLOPT) $(EXAMPLES_FLAGS) $(OBJS_NON_MT_OPT) -o $@ $<

threads: threads.ml $(OBJS_MT)
	$(OCAMLC) $(EXAMPLES_FLAGS) $(OBJS_MT) $(THREADS_FLAGS) -o $@ $<
threads.opt: threads.ml $(OBJS_MT_OPT)
	$(OCAMLOPT) $(EXAMPLES_FLAGS) $(OBJS_MT_OPT) $(THREADS_FLAGS) -o $@ $<

damned_recursion: damned_recursion.ml $(OBJS_MT)
	$(OCAMLC) $(EXAMPLES_FLAGS) $(OBJS_MT) $(THREADS_FLAGS) -o $@ $<
damned_recursion.opt: damned_recursion.ml $(OBJS_MT_OPT)
	$(OCAMLOPT) $(EXAMPLES_FLAGS) $(OBJS_MT_OPT) $(THREADS_FLAGS) -o $@ $<

distclean: clean
clean:
	-rm -f *.cm[ioax] *.o $(EXAMPLES) $(patsubst %,%.opt,$(EXAMPLES))
