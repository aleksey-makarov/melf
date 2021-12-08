# nix-shell --argstr compilerVersion ghc884

# { compilerVersion ? "ghc884" }:
# { compilerVersion ? "ghc8107" }:
# { compilerVersion ? "ghc901" }:
{ compilerVersion ? "ghc921" }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  overrides884 = self: super: {
    singletons = pkgs.haskell.lib.overrideCabal super.singletons {
      version = "2.6";
      revision = "0";
      editedCabalFile = "02yvj4cy2m64nk37mmjfwlhn8xwcyxagw146mdgmczz38ajcxsg8";
      sha256 = "1lc6p1f3h0j4nq5ppqwjihrjlgcwl5sx5fsw449m9lvs07vp39xy";
    };
    th-desugar = pkgs.haskell.lib.overrideCabal super.th-desugar {
      version = "1.10";
      sha256 = "1g3v427qlpxl1m4klsbqzg2xas5sj4059j5pdx0vpbshpq9v3x8v";
    };
  };
  overrides8107 = self: super: {};
  overrides901 = self: super: {
    singletons = super.singletons_3_0_1;
    singletons-th = pkgs.haskell.lib.overrideCabal super.singletons-th {
      version = "3.0";
      revision = 0;
      broken = false;
      sha256 = "1c0w7sg0lbpizrzns4g55wxsk5jm8wlqw0w9rz4jzqwy15byb572";
    };
    singletons-base = pkgs.haskell.lib.overrideCabal super.singletons-base {
      version = "3.0";
      revision = 0;
      sha256 = "0syrh4f9rs4g643c90md1vqrpr6p8h8g8sh4x3j2dld12yvrw4wn";
    };
    th-desugar = pkgs.haskell.lib.overrideCabal super.th-desugar {
      version = "1.12";
      sha256 = "1bp47jpif299kbm27zhjaw1nhl12daa09vsc8f0jracq0jhxi3iv";
    };
    system-fileio = pkgs.haskell.lib.dontCheck super.system-fileio;
  };
  overrides921 = self: super: {
    bsb-http-chunked = pkgs.haskell.lib.dontCheck super.bsb-http-chunked;
    singletons = super.singletons_3_0_1;
    singletons-th = pkgs.haskell.lib.overrideCabal super.singletons-th {
      broken = false;
    };
    system-fileio = pkgs.haskell.lib.dontCheck super.system-fileio;
  };
  overrides = if compilerVersion == "ghc8107" then overrides8107 else
              if compilerVersion == "ghc884"  then overrides884  else
              if compilerVersion == "ghc901"  then overrides901  else
              if compilerVersion == "ghc921"  then overrides921  else
                throw "version ${compilerVersion} is not supported";
  compiler = pkgs.haskell.packages."${compilerVersion}".override {
    inherit overrides;
  };
in
  compiler.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (
        with pkgs.haskellPackages;
        [ cabal-install
          cabal-plan
          cabal2nix
          hpack
          niv
          packdeps
        ]
      );
  }
