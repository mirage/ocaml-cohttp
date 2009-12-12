OCAMLMAKEFILE=OCamlMakefile

SOURCES = http_types.ml http_constants.ml \
	  http_common.ml http_parser_sanity.ml http_parser.ml \
	  http_misc.ml http_message.ml \
	  http_response.ml http_request.ml \
	  http_user_agent.ml http_cookie.ml
PACKS = netstring lwt
RESULT = cohttp
LIB_PACK_NAME = cohttp
ANNOTATE = yes

.PHONY: all
all: pack-byte-code pack-native-code cohttp.cma cohttp.cmxa
	@ :

DISTVERSION = 0.1

META: META.in
	cat META.in | sed -e 's/@DISTVERSION@/$(DISTVERSION)/' > META

LIBINSTALL_FILES = META cohttp.cma cohttp.cmxa cohttp.a cohttp.cmi

install: libinstall
uninstall: libuninstall
reinstall: uninstall install

-include $(OCAMLMAKEFILE)
