{
  description = "A very basic flake";

  outputs = { self, nixpkgs }: {

    packages.aarch64-darwin.barbly =
      let
        pkgs = nixpkgs.legacyPackages.aarch64-darwin;
      in pkgs.haskell.lib.addBuildDepend (pkgs.haskellPackages.callCabal2nix "barbly" ./. {}) pkgs.darwin.apple_sdk.frameworks.AppKit;

    defaultPackage.aarch64-darwin = self.packages.aarch64-darwin.barbly;

  };
}
