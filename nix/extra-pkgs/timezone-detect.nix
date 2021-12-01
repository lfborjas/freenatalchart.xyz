{ mkDerivation, base, directory, hspec, hspec-discover, lib
, QuickCheck, time, timezone-olson, timezone-series
}:
mkDerivation {
  pname = "timezone-detect";
  version = "0.3.0.1";
  sha256 = "1d538555d3055056644c4213a94fd51d80797966d3982512c0ff9b1440af92d8";
  libraryHaskellDepends = [
    base time timezone-olson timezone-series
  ];
  testHaskellDepends = [
    base directory hspec QuickCheck time timezone-olson timezone-series
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/lfborjas/timezone-detect#readme";
  description = "Haskell bindings for the zone-detect C library; plus tz-aware utils";
  license = lib.licenses.gpl2;
}
