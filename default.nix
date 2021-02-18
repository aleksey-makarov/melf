let
  # nixos-unstable latest
  # FIXME: use niv
  pkgs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/64b4617883844efe0cc20163e007ee636462eb18.tar.gz) { };

in
  { melf = pkgs.haskellPackages.callPackage ./melf.nix { };
  }
