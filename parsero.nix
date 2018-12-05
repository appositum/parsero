{ mkDerivation, stdenv, base, text }:
mkDerivation {
  pname = "parsero";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [ base text ];
  testHaskellDepends = [ base ];
  doHaddock = false;
  homepage = "https://github.com/appositum/parsero#readme";
  license = stdenv.lib.licenses.asl20;
}
