let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./cuceta.nix {}
