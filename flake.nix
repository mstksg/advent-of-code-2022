{
    description = "aoc2022";
    inputs = {
      haskellNix.url = "github:input-output-hk/haskell.nix";
      nixpkgs.follows = "haskellNix/nixpkgs-unstable";
      flake-utils.url = "github:numtide/flake-utils";
    };
    outputs = { self, nixpkgs, flake-utils, haskellNix }:
        flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
          let overlays = [
                haskellNix.overlay
                  (final: prev: {
                    aoc2022 = final.haskell-nix.project' {
                      name = "aoc2022";
                      src = ./.;
                      compiler-nix-name = "ghc902";
                      shell.tools = {
                        cabal = {};
                        # hlint = {};
                        haskell-language-server = {};
                      };
                    };
                  })
              ];
              pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
              flake = pkgs.aoc2022.flake {};
          in flake // { packages.default = flake.packages."aoc2022:exe:aoc2022"; }
        );
}
