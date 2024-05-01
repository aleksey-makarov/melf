{
  description = "melf";

  nixConfig.bash-prompt = "melf";
  nixConfig.bash-prompt-prefix = "[\\033[1;33m";
  nixConfig.bash-prompt-suffix = "\\033[0m \\w]$ ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-vscode-extensions = {
      url = "github:nix-community/nix-vscode-extensions";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    nix-vscode-extensions,
  }: let
    system = "x86_64-linux";

    pkgs = nixpkgs.legacyPackages.${system};

    # my-haskell = pkgs.haskellPackages;
    my-haskell = pkgs.haskell.packages.ghc98;

    my-nativeBuildInputs = with pkgs; [
      cabal-install
      my-haskell.haskell-language-server
      my-haskell.ormolu
      vscode
    ];

    melf-shell =
      (my-haskell.developPackage {
        root = ./.;
        withHoogle = true;
        returnShellEnv = true;
      })
      .overrideAttrs (attrs: {
        buildInputs = attrs.nativeBuildInputs ++ my-nativeBuildInputs;
        shellHook = ''
          export HOME=$(pwd)
          echo "*** HOME:"
          echo "''$HOME"
          echo "*** Haskell language server:"
          echo "${my-haskell.haskell-language-server}/bin/haskell-language-server"
          echo "*** VSCode extensions:"
          codium --list-extensions
        '';
      });

    extensions = nix-vscode-extensions.extensions.${system};

    vscode = pkgs.vscode-with-extensions.override {
      vscode = pkgs.vscodium;
      vscodeExtensions = [
        extensions.vscode-marketplace.github.vscode-github-actions
        extensions.vscode-marketplace.bbenoist.nix
        extensions.vscode-marketplace.justusadam.language-haskell
        extensions.vscode-marketplace.haskell.haskell
        # extensions.vscode-marketplace.ms-vscode.cpptools
      ];
    };
  in {
    devShells.${system} = {
      default = melf-shell;
    };
  };
}
