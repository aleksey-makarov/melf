let
  sources = import ../nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  pkgsCross = import sources.nixpkgs {
    crossSystem.config = "aarch64-unknown-linux-gnu";
  };
  melfSrc = sources.melf;
  melf = pkgs.haskellPackages.callCabal2nix "melf" melfSrc {};
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    melf
  ]);
in
  pkgs.mkShell {
    buildInputs = with pkgs ; [ qemu cabal-install pandoc ghc pkgsCross.buildPackages.gcc ];
    LANG = "C.UTF-8" ;
  }
