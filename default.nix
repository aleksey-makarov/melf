let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in
  pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          cabal2nix
          hpack
          niv
          packdeps
        ]);
  }
