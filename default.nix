let
  # nixos-unstable latest
  # FIXME: use niv
  pkgs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/e1f8852faac7638e88d5e8a5b9ee2a7568685e3f.tar.gz) { };

in
  { melf = pkgs.haskellPackages.callPackage ./melf.nix { };
  }
