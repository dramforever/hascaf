{ pkgs ? import (import ./.nix/nixpkgs.nix {}) {} }:

with pkgs;

let sifive-toolchain = callPackage (import ./.nix/prebuilt-toolchain.nix) {}; in

mkShell.override { stdenv = stdenvNoCC; } {
  name = "hascaf-dev";

  nativeBuildInputs = [
    qemu parallel
    sifive-toolchain
    bashInteractive
  ];
}
