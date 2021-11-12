let
  pkgsFunc = import <nixpkgs>;
  pkgs = pkgsFunc {};
  pkgsCross = pkgsFunc {
    crossSystem.config = "aarch64-unknown-linux-gnu";
  };
  melfSrc = pkgs.fetchFromGitHub {
    owner = "aleksey-makarov";
    repo  = "melf";
    rev = "ed63a07cb8d88dcfc1221d8dbe9f49b911734420";
    sha256 = "06n7g5my9mspr0xdv8959yfpgdck6pib5k29q3jjv0170yy3ywc2";
  };
  melf = pkgs.haskellPackages.callCabal2nix "melf" melfSrc {};
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    melf
  ]);
in
  pkgsCross.mkShell {
    nativeBuildInputs = [ pkgs.qemu pkgs.cabal-install ghc pkgs.pandoc ];
    LANG = "C.UTF-8" ;
  }
