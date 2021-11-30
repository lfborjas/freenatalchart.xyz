{ mkDerivation, base, directory, fetchgit, hpack, hspec
, hspec-discover, lib, QuickCheck, random, time, vector
}:
mkDerivation {
  pname = "swiss-ephemeris";
  version = "1.4.1.0";
  src = fetchgit {
    url = "https://github.com/lfborjas/swiss-ephemeris";
    sha256 = "1m5194rdqra9rmglqd5zxa91kih5hnj239bs7nfksj276qszp7la";
    rev = "17ad66f7d0884ccc17aeb9a559ef4335032e2ee0";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base time vector ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base directory hspec QuickCheck random time vector
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/lfborjas/swiss-ephemeris#readme";
  description = "Haskell bindings for the Swiss Ephemeris C library";
  license = lib.licenses.agpl3;
}
