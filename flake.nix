{
  description = "Hascaf";

  outputs = { self, nixpkgs }: {
    defaultPackage.x86_64-linux =
      with nixpkgs.legacyPackages.x86_64-linux;

      let sifive-toolchain = callPackage (import ./prebuilt-toolchain.nix) {}; in

      mkShell.override { stdenv = stdenvNoCC; } {
        name = "hascaf-dev";

        nativeBuildInputs = [
          qemu parallel
          sifive-toolchain
          bashInteractive
        ];
      };
  };
}
