{
  description = "Cohttp Nix Flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nix-ocaml/nix-overlays";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (import nixpkgs {
          inherit system;
          overlays = [
            (final: prev: {
              ocamlPackages = prev.ocamlPackages.overrideScope' (oself: osuper: {
                ctypes-foreign = osuper.ctypes-foreign.overrideAttrs (_: { doCheck = false; });
                ctypes = osuper.ctypes.overrideAttrs (_: { doCheck = false; });
                mdx = osuper.mdx.override {
                  # workaround for:
                  # https://github.com/NixOS/nixpkgs/pull/241476/commits/1ed74f3536d29e5635d7f47a1d7b82a89f5a8077
                  logs = oself.logs;
                };
              });
            })
          ];
        });
        inherit (pkgs.ocamlPackages) buildDunePackage;
        pkg = attrs: buildDunePackage ({
            version = "n/a";
            src = ./. ;
            duneVersion = "3";
            doCheck = true;
          } // attrs);
        ocamlformat = pkgs.ocamlformat_0_26_2;
      in
      with pkgs.ocamlPackages; rec {
        packages = rec {
          default = http;
          http = pkg {
            pname = "http";
            checkInputs = [ alcotest base_quickcheck ppx_expect crowbar ];
          };
          cohttp = pkg {
            pname = "cohttp";
            checkInputs = [ fmt alcotest ];
            propagatedBuildInputs = [
              stringext http re uri uri-sexp logs sexplib0 ppx_sexp_conv
            ];
          };
          cohttp-top = pkg {
            pname = "cohttp-top";
            propagatedBuildInputs = [ cohttp ];
          };
          cohttp-curl = pkg {
            pname = "cohttp-curl";
            propagatedBuildInputs = [ ocurl http stringext ];
          };
          cohttp-curl-lwt = pkg {
            pname = "cohttp-curl-lwt";
            checkInputs = [ cohttp-lwt-unix cohttp cohttp-lwt conduit-lwt ounit uri ];
            propagatedBuildInputs = [ ocurl http stringext lwt ];
          };
          cohttp-curl-async = pkg {
            pname = "cohttp-curl-async";
            checkInputs = [ uri fmt ounit alcotest cohttp-async ];
            propagatedBuildInputs = [
              ocurl http stringext cohttp-curl core core_unix
              async_kernel async_unix 
            ];
          };
          cohttp-lwt = pkg {
            pname = "cohttp-lwt";
            propagatedBuildInputs = [ http cohttp lwt sexplib0 ppx_sexp_conv logs uri ];
          };
          cohttp-lwt-jsoo = pkg {
            pname = "cohttp-lwt-jsoo";
            propagatedBuildInputs = [
              http cohttp cohttp-lwt logs lwt lwt_ppx js_of_ocaml
              js_of_ocaml-ppx js_of_ocaml-lwt
            ];
          };
          cohttp-async = pkg {
            pname = "cohttp-async";
            checkInputs = [ mirage-crypto ounit ];
            propagatedBuildInputs = [ http cohttp async_kernel async_unix async base
              core core_unix conduit-async magic-mime logs fmt sexplib0 ppx_sexp_conv
              uri uri-sexp ipaddr
            ];
          };
          cohttp-lwt-unix = pkg {
            pname = "cohttp-lwt-unix";
            checkInputs = [ ounit ];
            propagatedBuildInputs = [
              http cohttp cohttp-lwt cmdliner lwt conduit-lwt
              conduit-lwt-unix fmt ppx_sexp_conv magic-mime logs
            ];
          };
          cohttp-server-lwt-unix = pkg {
            pname = "cohttp-server-lwt-unix";
            checkInputs = [ conduit-lwt-unix cohttp-lwt cohttp-lwt-unix ];
            propagatedBuildInputs = [ http lwt ];
          };
          cohttp-eio = pkg {
            pname = "cohttp-eio";
            checkInputs = [ alcotest eio_main mdx ppx_here
              tls-eio 
              mirage-crypto-rng-eio
            ];
            propagatedBuildInputs = [ cohttp eio eio_main logs uri fmt ptime http ];
          };
          cohttp-mirage = pkg {
            pname = "cohttp-mirage";
            propagatedBuildInputs = [
              mirage-flow mirage-channel conduit conduit-mirage
              mirage-kv lwt cohttp-lwt cstruct fmt astring magic-mime ppx_sexp_conv
            ];
          };
          cohttp-bench = pkg {
            pname = "cohttp-bench";
            buildInputs = [
              core core_bench eio http cohttp cohttp-eio
              cohttp-lwt-unix cohttp-server-lwt-unix cohttp-async
            ];
          };
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = pkgs.lib.attrValues packages;
          buildInputs = [ ocamlformat ] ++ (with pkgs.ocamlPackages; [
            ocaml-lsp
          ]);
        };
        devShells.eio = pkgs.mkShell {
          inputsFrom = [ cohttp-eio ];
          buildInputs = [ ocamlformat ] ++ (with pkgs; [
            ocamlPackages.ocaml-lsp gmp libev nmap curl
          ]);
        };
      });
}
