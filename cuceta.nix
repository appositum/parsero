{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "cuceta";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  doHaddock = false;
  homepage = "https://github.com/appositum/cuceta#readme";
  license = stdenv.lib.licenses.asl20;
  shellHook = ''
  cabal v1-repl
  '';
}
