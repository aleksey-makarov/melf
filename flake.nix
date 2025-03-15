{
  description = "melf develop environment";

  nixConfig.bash-prompt = "melf-develop";
  nixConfig.bash-prompt-prefix = "[\\033[1;33m";
  nixConfig.bash-prompt-suffix = "\\033[0m \\w]$ ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-vscode-extensions = {
      url = "github:nix-community/nix-vscode-extensions";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      nix-vscode-extensions,
    }:
    let
      system = "x86_64-linux";

      overlays = [
        nix-vscode-extensions.overlays.default
      ];

      pkgs = import nixpkgs {
        inherit system overlays;
        config = {
          allowUnfree = true;
        };
      };

      vscode = pkgs.vscode-with-extensions.override {
        vscodeExtensions = with pkgs; [
          vscode-marketplace.bbenoist.nix
          vscode-marketplace.timonwong.shellcheck
          vscode-marketplace.haskell.haskell
          vscode-marketplace.justusadam.language-haskell
          vscode-marketplace-release.github.copilot
          vscode-marketplace-release.github.copilot-chat
        ];
      };

    in
    {
      devShells.${system} = rec {
        default =
          with pkgs;
          mkShell {
            packages = [
              vscode
              shellcheck
              nixfmt-rfc-style
              ghc
              cabal-install
              haskell-language-server
            ];
            shellHook = ''
              echo "nixpkgs: ${nixpkgs}"
              echo For the list of available ghc compilers run:
              echo "nix-env -f ${nixpkgs} -qaP -A haskell.compiler"
              echo "Configure VSCode Haskell plugin with"
              echo "haskell.serverExecutablePath: ${pkgs.haskell-language-server}/bin/haskell-language-server"

              export HOME=$(pwd)
            '';
          };
      };
    };
}
