module Transits.RootFindingSpec (spec) where

import Import (Longitude (Longitude))
import SwissEphemeris (JulianTime (JulianTime), Planet (Sun))
import Test.Hspec (Spec, describe, it, shouldBe)
import Transits.RootFinding (ExactTransit (..), findExactTransit)

spec :: Spec
spec = do
  describe "findExactTransit" $ do
    it "finds the exact moment of a known conjunction, given bracketing times." $ do
      let natalPluto = Longitude 224.6882
          transiting = Sun
          -- 11/6/2020 15:46:23 UTC
          transitIsExactAt = ExactAt $ JulianTime 2459160.1572215613
          -- 11/6/2020 00:00:00 UTC
          beginSearch = JulianTime 2459158.5
          -- 11/8/2020 00:00:00 UTC
          endSearch = JulianTime 2459161.5
      (findExactTransit transiting natalPluto beginSearch endSearch) `shouldBe` transitIsExactAt
