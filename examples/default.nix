
let
    pkgsf = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/64b4617883844efe0cc20163e007ee636462eb18.tar.gz);
    pkgs = pkgsf {};
    pkgsCross = pkgsf {
        crossSystem.config = "aarch64-unknown-linux-gnu";
    };

    # local store:
    #
    melf = /nix/store/m018j93qw518gn1hsh3w49djgp8b7xdw-melf-0.1;

    # FIXME: this does not work
    # cachix:
    #
    # 1. install cachix:
    # nix-env -iA cachix -f https://cachix.org/api/v1/install
    #
    # 2. add 'aleksey-makarov' cachix cache
    # cachix use aleksey-makarov
    #
    # melf = /nix/store/bl376nkfcnf8w41a1k7ymy3bzv1s8cbd-melf-0.1;

in

    pkgsCross.mkShell {
      nativeBuildInputs = [ melf pkgs.qemu ];
      LANG = "C.UTF-8" ;
    }
