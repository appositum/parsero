{ mkDerivation, stdenv, base, hspec, QuickCheck, text, }:
mkDerivation {
  pname = "parsero";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [ base hspec QuickCheck text ];
  testHaskellDepends = [ base hspec QuickCheck ];
  doHaddock = false;
  homepage = "https://github.com/appositum/parsero#readme";
  license = stdenv.lib.licenses.asl20;
  shellHook = ''
    source ~/.bashrc
  '';
}
