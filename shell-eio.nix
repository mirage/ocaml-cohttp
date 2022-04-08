{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    libev
    gmp
    pkg-config
    nmap  #ncat testing tool
    curl
  ];

  LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath [pkgs.curl];
  dontPatchShebangs = "1";
}
