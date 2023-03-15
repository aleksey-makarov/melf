# How to call for another GHC version:
# - nix-shell --argstr compilerVersion ghc94
# - or just uncomment below

# { compilerVersion ? "ghc88"  }: # 8.8.4
# { compilerVersion ? "ghc810" }: # 8.10.7
# { compilerVersion ? "ghc90"  }: # 9.0.2
  { compilerVersion ? "ghc92"  }: # 9.2.7 default
# { compilerVersion ? "ghc94"  }: # 9.4.4
# { compilerVersion ? "ghc96"  }: # 9.6.1
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
