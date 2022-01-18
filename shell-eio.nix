{ pkgs ? import (fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/11fbbb2126ebb6c83b85908a7773979cbfe270d2.tar.gz")
  { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    curl
    pkg-config
    gmp
    libev
    zlib
    rustup
  ];

  LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath [pkgs.curl];
  dontPatchShebangs = "1";
}
