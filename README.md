# lambda

Get a Haskell development environment up and running quickly. Thanks to Nix, this template is optimized for a fully reproducible and friendly development environment. It is based on:

- [Nix](https://srid.ca/haskell-nix) + [Flakes](https://serokell.io/blog/practical-nix-flakes) (via [`github:srid/haskell-flake`](https://github.com/srid/haskell-flake)) + GHC 9
- VSCode + [HLS](https://github.com/haskell/haskell-language-server)
- [fourmolu](https://github.com/fourmolu/fourmolu) autoformatting 
- [Relude](https://github.com/kowainik/relude#relude) as Prelude.
  - `.hlint.yaml` is [from relude](https://github.com/kowainik/relude/blob/main/.hlint.yaml)

## Getting Started

*tldr: Install Nix, enable Flakes, open in VSCode and run `bin/run`.*

For details, see: https://srid.ca/lambda/start

## Tips

- Run `nix flake update` to update all flake inputs.
- Run `nix --option sandbox false build .#check -L` to run the flake checks.
- Run `treefmt` in nix shell to autoformat the project. This uses [treefmt](https://github.com/numtide/treefmt), which uses `./treefmt.toml` (where fourmolu and nixpkgs-fmt are specified).
- Run `bin/hoogle` to start Hoogle with packages in your cabal file.
- Run the application without installing: `nix run github:srid/lambda` (or `nix run .` from checkout)
- Common workflows
  - Adding tests: http://srid.ca/lambda/tests
  - Adding Garnix CI: http://srid.ca/lambda/garnix

## Discussions

Got questions? Ideas? Suggestions? Post them here: https://github.com/srid/lambda/discussions
