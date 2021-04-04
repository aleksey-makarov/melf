
let
    pkgsf = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/64b4617883844efe0cc20163e007ee636462eb18.tar.gz);
    pkgs = pkgsf {};
    pkgsCross = pkgsf {
        crossSystem.config = "aarch64-unknown-linux-gnu";
    };

    # local store:
    #
    # melf = /nix/store/m018j93qw518gn1hsh3w49djgp8b7xdw-melf-0.1;

    # FIXME: works, but I doubt it's correct
    # cachix:
    #
    # 1. install cachix:
    # nix-env -iA cachix -f https://cachix.org/api/v1/install
    #
    # 2. add 'aleksey-makarov' cachix cache
    # cachix use aleksey-makarov
    #
    # 3. nix-store -r /nix/store/9kcm1j610kdf0mdilryf4kih7g7v024s-melf-0.1
    #
    # How to upgrade to the late build from cache:
    # - Copy the last line of "Run nix-build" from gihub's actions
    # - Issue this command
    # find . -type f -print0 | xargs -0 sed -i 's$/nix/store/9kcm1j610kdf0mdilryf4kih7g7v024s-melf-0.1$<insert new path here>$g'

    melf = /nix/store/9kcm1j610kdf0mdilryf4kih7g7v024s-melf-0.1;

in

    pkgsCross.mkShell {
      nativeBuildInputs = [ melf pkgs.qemu ];
      LANG = "C.UTF-8" ;
    }
