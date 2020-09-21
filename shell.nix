{ pkgs ? import (import ./.nix/nixpkgs.nix {}) {} }:

with pkgs;

let sifive-toolchain = callPackage (import ./.nix/prebuilt-toolchain.nix) {}; in

stdenvNoCC.mkDerivation {
  name = "hascaf-dev";

  nativeBuildInputs = [
    qemu parallel
    sifive-toolchain
  ];
}
