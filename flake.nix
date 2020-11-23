{
  description = "A very basic flake";

  outputs = { self, nixpkgs }: {

    packages.x86_64-darwin.barbly =
      let
        pkgs = nixpkgs.legacyPackages.x86_64-darwin;
      in pkgs.haskell.lib.addBuildDepend (pkgs.haskellPackages.callCabal2nix "barbly" ./. {}) pkgs.darwin.apple_sdk.frameworks.AppKit;

    defaultPackage.x86_64-darwin = self.packages.x86_64-darwin.barbly;

  };
}
