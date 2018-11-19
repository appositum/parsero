{ compiler ? "ghc844", doBenchmark ? false }:

let nixpkgs = import <nixpkgs> {};
    orig = nixpkgs.pkgs.haskellPackages.callPackage ./default.nix {};
in orig.env
