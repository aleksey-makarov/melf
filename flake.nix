{
  description = "melf develop environment";

  nixConfig.bash-prompt = "melf-develop";
  nixConfig.bash-prompt-prefix = "[\\033[1;33m";
  nixConfig.bash-prompt-suffix = "\\033[0m \\w]$ ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    {
      self,
      nixpkgs,
    }:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
        };
      };

      vscode = pkgs.vscode-with-extensions.override {
        vscode = pkgs.vscodium;
        vscodeExtensions = with pkgs.vscode-extensions; [
          bbenoist.nix
          timonwong.shellcheck
          github.copilot
          github.copilot-chat
        ];
      };

      my-ghc = pkgs.haskell.compiler.ghc912;

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
              my-ghc
              cabal-install
            ];
            shellHook = ''
              echo "nixpkgs: ${nixpkgs}"
              echo For the list of available ghc compilers run:
              echo "nix-env -f ${nixpkgs} -qaP -A haskell.compiler"

              export HOME=$(pwd)
            '';
          };
      };
    };
}
