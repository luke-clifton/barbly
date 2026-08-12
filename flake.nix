{
  description = "A very basic flake";

  outputs = { self, nixpkgs }: {

    packages.aarch64-darwin.barbly =
      let
        pkgs = nixpkgs.legacyPackages.aarch64-darwin;
      in pkgs.haskellPackages.callCabal2nix "barbly" ./. {};

    defaultPackage.aarch64-darwin = self.packages.aarch64-darwin.barbly;

  };
}
