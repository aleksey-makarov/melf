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

  source-overrides = {
    ghc884 = {
      singletons = "2.6";
      th-desugar = "1.10";
    };
    ghc8107 = {
      singletons = "2.7";
      th-desugar = "1.11";
    };
    ghc902 = {
      th-desugar = "1.12";
      singletons = "3.0";
      singletons-base = "3.0";
      singletons-th = "3.0";
    };
    ghc924 = {};
    ghc925 = {};
    ghc944 = {};
  }."${compilerVersion}";

  overrides = self: super: {
    ghc884 = {};
    ghc8107 = {};
    ghc902 = {};
    ghc924 = {};
    ghc925 = {};
    ghc944 = {
      singletons = super.singletons_3_0_2;
      singletons-base = super.singletons-base_3_1_1;
      singletons-th = super.singletons-th_3_1_1;
      th-desugar = super.th-desugar_1_14;
      string-qq = pkgs.haskell.lib.doJailbreak super.string-qq;
      chell = pkgs.haskell.lib.doJailbreak super.chell;
    };
  }."${compilerVersion}";

  myHaskellPackages = pkgs.haskell.packages."${compilerVersion}".override { inherit overrides; };

in
  myHaskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (
        with myHaskellPackages;
        [ cabal-install
          cabal2nix
          hpack
          niv
        ]
      );
    inherit source-overrides;
    # FIXME: overrides do not work here
  }
