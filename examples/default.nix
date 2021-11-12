let
  sources = import ../nix/sources.nix;
  pkgsFunc = import sources.nixpkgs;
  pkgs = pkgsFunc {};
  pkgsCross = pkgsFunc {
    crossSystem.config = "aarch64-unknown-linux-gnu";
  };
  melfSrc = sources.melf;
  melf = pkgs.haskellPackages.callCabal2nix "melf" melfSrc {};
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    melf
  ]);
in
  pkgsCross.mkShell {
    nativeBuildInputs = [ pkgs.qemu pkgs.cabal-install ghc pkgs.pandoc ];
    LANG = "C.UTF-8" ;
  }
