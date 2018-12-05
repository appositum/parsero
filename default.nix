let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./parsero.nix {}
