OCAMLMAKEFILE=OCamlMakefile

SOURCES = http_types.mli http_types.ml http_constants.mli http_constants.ml \
	  http_common.mli http_common.ml http_parser_sanity.mli http_parser_sanity.ml http_parser.mli http_parser.ml \
	  http_misc.mli http_misc.ml http_message.mli http_message.ml \
	  http_response.mli http_response.ml http_request.mli http_request.ml \
	  http_client.mli http_client.ml http_cookie.mli http_cookie.ml
PACKS = netstring lwt lwt.unix
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
