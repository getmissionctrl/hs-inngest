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
        # allowUnfree: the Inngest dev-server binary is SSPL-licensed.
        pkgs = import nixpkgs { inherit system; config.allowUnfree = true; };
        hs-inngest = pkgs.haskellPackages.callCabal2nix "hs-inngest" ./. {};
        # Dev/test shell: GHC with every lib+test+example dependency of the
        # package (via `.env`) plus cabal-install, and the Inngest dev server +
        # process-compose so `cabal test` and the end-to-end example both run.
        devShell = hs-inngest.env.overrideAttrs (old: {
          nativeBuildInputs = (old.nativeBuildInputs or [])
            ++ [ pkgs.cabal-install pkgs.inngest pkgs.process-compose pkgs.curl ];
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
