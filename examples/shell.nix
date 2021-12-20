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
    buildInputs = with pkgs ; [ nix niv cabal-install pandoc ghc pkgsCross.buildPackages.gcc qemu ];
    # 8.10.4: en_US.UTF-8 => ascii, C.UTF-8 => utf8
    # 9.0.1:  en_US.UTF-8 => utf8,  C.UTF-8 => ascii
    # WTF?
    LANG = "C.UTF-8" ;
  }
