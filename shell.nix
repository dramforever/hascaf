{ pkgs ? import (import ./.nix/nixpkgs.nix {}) {} }:

with pkgs.pkgsCross.riscv32-embedded;

mkShell {
  name = "dev";

  nativeBuildInputs = [ buildPackages.buildPackages.qemu ];
}
