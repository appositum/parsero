{ mkDerivation, stdenv, base, text }:
mkDerivation {
  pname = "cuceta";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base text ];
  testHaskellDepends = [ base text ];
  doHaddock = false;
  homepage = "https://github.com/appositum/cuceta#readme";
  license = stdenv.lib.licenses.asl20;
}
