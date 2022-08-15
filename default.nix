# How to call for another GHC version:
# - nix-shell --argstr compilerVersion ghc884
# - or just uncomment below

# { compilerVersion ? "ghc884" }:
# { compilerVersion ? "ghc8107" }:
# { compilerVersion ? "ghc902" }:
{ compilerVersion ? "ghc924" }:
# { compilerVersion ? "ghc941" }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  source-overrides-884 = {
    singletons = "2.6";
    th-desugar = "1.10";
  };
  source-overrides-8107 = {
    singletons = "2.7";
    th-desugar = "1.11";
  };
  source-overrides-902 = {};
  source-overrides-924 = {
    singletons-base = "3.1";
    singletons-th = "3.1";
  };
  source-overrides-941 = {};

  source-overrides = if compilerVersion == "ghc884"  then source-overrides-884  else
                     if compilerVersion == "ghc8107" then source-overrides-8107 else
                     if compilerVersion == "ghc902"  then source-overrides-902  else
                     if compilerVersion == "ghc924"  then source-overrides-924  else
                     # if compilerVersion == "ghc941"  then source-overrides-941  else
                throw "version ${compilerVersion} is not supported";

  myHaskellPackages = pkgs.haskell.packages."${compilerVersion}";

in
  myHaskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (
        with myHaskellPackages;
        [ cabal-install
          cabal-plan
          cabal2nix
          hpack
          niv
        ]
      );
    source-overrides = source-overrides;
  }
