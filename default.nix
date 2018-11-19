{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc844" }:
let pkgs = import <nixpkgs> { };
in pkgs.haskellPackages.callPackage ./cuceta.nix { }
