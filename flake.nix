{
  description = "Advent of code 2022";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-flake.url = "github:srid/treefmt-flake";
  };

  outputs =
    inputs @ { self
    , nixpkgs
    , flake-parts
    , ...
    }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-flake.flakeModule
      ];
      perSystem =
        { self'
        , config
        , pkgs
        , ...
        }: {
          haskellProjects.default = {
            packages = {
              haskell-template.root = ./.;
            };
            buildTools = hp:
              let
                # https://github.com/NixOS/nixpkgs/issues/140774 reoccurs in GHC 9.2
                workaround140774 = hpkg:
                  with pkgs.haskell.lib;
                  overrideCabal hpkg (drv: {
                    enableSeparateBinOutput = false;
                  });
              in
              {
                ghcid = workaround140774 hp.ghcid;
              }
              // config.treefmt.formatters;
            hlsCheck.enable = true;
            hlintCheck.enable = true;
            haskellPackages = pkgs.haskell.packages.ghc92;
            overrides = self: super: with pkgs.haskell.lib; {
              # All these below are for GHC 9.2 compat.
              relude = dontCheck super.relude_1_1_0_0; # Not the default in nixpkgs yet.
              retry = dontCheck super.retry;
              http2 = dontCheck super.http2; # Fails on darwin
              streaming-commons = dontCheck super.streaming-commons; # Fails on darwin
            };
          };
          treefmt.formatters = {
            inherit
              (pkgs)
              alejandra
              ;
            inherit
              (pkgs.haskellPackages)
              cabal-fmt
              fourmolu
              ;
          };
        };
    };
}
