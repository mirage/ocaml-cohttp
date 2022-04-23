let
  pkgs = (import <nixpkgs> { });
  local = (import ./default.nix { });
  inherit (pkgs) stdenv lib;
  ocamlformat =
    let
      ocamlformat_version =
        let
          lists = pkgs.lib.lists;
          strings = pkgs.lib.strings;
          ocamlformat_config = strings.splitString "\n" (builtins.readFile ./.ocamlformat);
          prefix = "version = ";
          ocamlformat_version_pred = line: strings.hasPrefix prefix line;
          version_line = lists.findFirst ocamlformat_version_pred "not_found" ocamlformat_config;
          version = strings.removePrefix prefix version_line;
        in
        builtins.replaceStrings ["."] ["_"] version;
    in builtins.getAttr ("ocamlformat_" + ocamlformat_version) pkgs;
in with local;

pkgs.mkShell {
  inputsFrom = [
    http
    cohttp
    cohttp-top
    cohttp-lwt
    cohttp-lwt-unix
    cohttp-lwt-jsoo
    cohttp-async
    cohttp-bench
    cohttp-mirage
    cohttp-curl
    cohttp-curl-lwt
    cohttp-curl-async
  ];
  buildInputs = (with pkgs; [
    yarn
    nodejs-14_x
    ocamlPackages.ocaml-lsp
  ]) ++ [] ++ (with opam; [
    # test
    conf-libev
    ppx_expect
    ocamlformat-rpc
    fmt
    alcotest
    ounit
    lwt_ppx
    base_quickcheck
    ppx_assert
    ppx_sexp_conv
    ppx_compare
    ppx_here
    core_bench
    crowbar
  ]);
}
