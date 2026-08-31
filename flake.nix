{
  description = "hs-inngest — native Haskell SDK for Inngest";

  inputs = {
    # Pin to nixos-unstable to match the vf-haskell sibling repo's dep set.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hs-inngest = pkgs.haskellPackages.callCabal2nix "hs-inngest" ./. {};
        # Dev/test shell: GHC with every lib+test dependency of the package
        # (via `.env`) plus cabal-install, so `cabal build` / `cabal test`
        # build and run the hspec suite offline with incremental compilation.
        devShell = hs-inngest.env.overrideAttrs (old: {
          nativeBuildInputs = (old.nativeBuildInputs or [])
            ++ [ pkgs.cabal-install ];
        });
      in {
        packages = {
          inherit hs-inngest;
          default = hs-inngest;
        };
        devShells = {
          dev = devShell;
          default = devShell;
        };
      });
}
