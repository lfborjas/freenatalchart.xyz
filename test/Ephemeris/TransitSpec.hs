module Ephemeris.TransitSpec (spec) where

import Ephemeris
import Test.Hspec (Spec, describe, it, shouldBe)
import Ephemeris.Transit (ExactTransit (..), findExactTransit, findExactTransitAround)

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

  describe "findExactTransitAround" $ do
    it "finds the exact moment of a known transit, given a time when the longitude is closest" $ do
      let natalSunSquare = Longitude 195.9234
          transiting = Venus
          exactAt = ExactAt . JulianTime
          candidate = JulianTime 2459163.5
          foundTransit = findExactTransitAround transiting natalSunSquare candidate
      foundTransit `shouldBe` (exactAt 2459163.631149835)
  
    it "determines that the transit doesn't happen when close, but never past" $ do
      let natalSunSquare = Longitude 15.9234
          transiting = Mars
          candidate = JulianTime 2459161.5
          transitSearch = findExactTransitAround transiting natalSunSquare candidate
      transitSearch `shouldBe` OutsideBounds
