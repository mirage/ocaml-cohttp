{ pkgs ? import (fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/11fbbb2126ebb6c83b85908a7773979cbfe270d2.tar.gz")
  { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    autoconf
    rsync
    git
    m4
    patch
    unzip
    wget
    pkg-config
    gcc
    gmp
    libev
    hidapi
    libffi
    jq
    zlib
    rustup
    opam
    bc
    httpie
  ];

  dontPatchShebangs = "1";
}
