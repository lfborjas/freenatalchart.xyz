{ mkDerivation, base, containers, directory, fetchgit, foldl, hspec
, lib, microlens, QuickCheck, random, streaming, swiss-ephemeris
, time, vector
}:
mkDerivation {
  pname = "almanac";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lfborjas/almanac";
    sha256 = "0zqzq0b6y2fvmpxac1cq4kwpz7xcmfk0wbnqxhqd93nl4m1zbixr";
    rev = "9731199f1d43bafca1d002bbf68a80c8c4783535";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base containers foldl streaming swiss-ephemeris time vector
  ];
  testHaskellDepends = [
    base containers directory hspec microlens QuickCheck random
    swiss-ephemeris time
  ];
  description = "utilities for ephemeris analysis";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
