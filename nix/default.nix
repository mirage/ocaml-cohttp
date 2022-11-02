# paramaterised derivation with dependencies injected (callPackage style)
{ pkgs, stdenv, opam2nix }:
let
  strings = pkgs.lib.strings;
  args = {
    inherit (pkgs.ocaml-ng.ocamlPackages_4_14) ocaml;
    selection = ./opam-selection.nix;
    src =
      let ignores = pkgs.lib.strings.fileContents ../.gitignore
        + builtins.foldl' (acc: e: acc + "\n" + e) "\n"
        [ "nix" "shell.nix" "default.nix" ];
      in pkgs.nix-gitignore.gitignoreSourcePure ignores ../.;
  };
  opam-selection = opam2nix.build args;
  localPackages =
    let 
      contents = builtins.attrNames (builtins.readDir ../.);
      filter = p: strings.hasSuffix ".opam" p && "cohttp-eio.opam" != p;
    in builtins.filter filter contents;
  resolve = opam2nix.resolve args (localPackages ++ [
    # dev deps
    "ppx_expect"
    "conf-libev"
    "ocamlformat-rpc"
    "fmt"
    "alcotest"
    "ounit"
    "lwt_ppx"
    "base_quickcheck"
    "ppx_assert"
    "ppx_sexp_conv"
    "ppx_compare"
    "ppx_here"
    "core_bench"
    "crowbar"
  ]);

in
(builtins.listToAttrs (builtins.map
  (fname:
    let packageName = strings.removeSuffix ".opam" fname;
    in
    {
      name = packageName;
      value = builtins.getAttr packageName opam-selection;
    })
  localPackages)) // {
  inherit resolve;
  opam = opam-selection;
}
