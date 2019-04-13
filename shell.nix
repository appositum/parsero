with import <nixpkgs> {};

let
  env = haskellPackages.ghcWithPackages (p:
    [
      (p.callPackage ./parsero.nix {})
    ]);
in
stdenv.mkDerivation rec {
  name = "env";
  buildInputs = [ env ];
  shellHook = ''
  ghci && exit
  '';
}
