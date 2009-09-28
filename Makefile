OCAMLMAKEFILE=OCamlMakefile

SOURCES = http_types.mli http_constants.ml http_types.ml http_parser_sanity.ml http_misc.ml http_common.ml \
	http_tcp_server.ml http_parser.ml http_message.ml http_request.ml http_daemon.ml \
	http_response.ml http_user_agent.ml
PACKS = netstring lwt
RESULT = httplib
LIB_PACK_NAME = httplib

.PHONY: all
all: pack-byte-code pack-native-code
	@ :

META: META.in
	cat META.in | sed -e 's/@DISTVERSION@/$(DISTVERSION)/' > META

-include $(OCAMLMAKEFILE)
