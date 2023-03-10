# How to call for another GHC version:
# - nix-shell --argstr compilerVersion ghc94
# - or just uncomment below

# { compilerVersion ? "ghc884"  }:
# { compilerVersion ? "ghc8107" }:
# { compilerVersion ? "ghc902"  }:
  { compilerVersion ? "ghc924"  }: # default
# { compilerVersion ? "ghc925"  }:
# { compilerVersion ? "ghc944"  }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  myHaskellPackages = pkgs.haskell.packages."${compilerVersion}";

in
  myHaskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (
        with myHaskellPackages;
        [ cabal-install
          cabal2nix
          hpack
        ]
      );
  }
