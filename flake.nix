{
  description = "Hascaf";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/296793637b22bdb4d23b479879eba0a71c132a66";

  outputs = { self, nixpkgs }: {
    devShell.x86_64-linux =
      with nixpkgs.legacyPackages.x86_64-linux;

      let sifive-toolchain = callPackage (import ./prebuilt-toolchain.nix) {}; in

      mkShell {
        name = "hascaf-dev";

        nativeBuildInputs = [
          qemu parallel
          sifive-toolchain
          bashInteractive
          (haskell.packages.ghc8102.ghcWithPackages (ps: with ps; [
            cabal-install
            megaparsec
            microlens-platform
            mtl
            parser-combinators
            text
          ]))
        ];
      };
  };
}
