module Ephemeris.TransitSpec (spec) where

import Ephemeris
import Test.Hspec (xit, Spec, describe, it, shouldBe)

exactAt :: Double -> ExactTransit JulianTime
exactAt = ExactAt . JulianTime


spec :: Spec
spec = do
  describe "findExactTransit" $ do
    xit "finds the exact moment of a known conjunction, given bracketing times." $ do
      let natalPluto = Longitude 224.6882
          transiting' = Sun
          -- 11/6/2020 15:46:23 UTC
          transitIsExactAt = ExactAt $ JulianTime 2459160.1572215613
          -- 11/6/2020 00:00:00 UTC
          beginSearch = JulianTime 2459158.5
          -- 11/8/2020 00:00:00 UTC
          endSearch = JulianTime 2459161.5
      (findExactTransit transiting' natalPluto beginSearch endSearch) `shouldBe` transitIsExactAt

  describe "findExactTransitAround" $ do
    xit "finds the exact moment of a known transit, given a time when the longitude is closest" $ do
      let natalSunSquare = Longitude 195.9234
          transiting' = Venus
          candidate = JulianTime 2459163.5
          foundTransit = findExactTransitAround transiting' natalSunSquare candidate
      foundTransit `shouldBe` (exactAt 2459163.631149835)
  
    it "determines that the transit doesn't happen when close, but never past" $ do
      let natalSunSquare = Longitude 15.9234
          transiting' = Mars
          candidate = JulianTime 2459161.5
          transitSearch = findExactTransitAround transiting' natalSunSquare candidate
      transitSearch `shouldBe` OutsideBounds

    xit "correctly finds a crossing that jumps over 360" $ do
      let marsCrossing = Longitude 0.76
          transiting' = Mars
          candidate = JulianTime 2457073.5
          foundTransit = findExactTransitAround transiting' marsCrossing candidate
      foundTransit `shouldBe` (exactAt 2457074.495819183)
