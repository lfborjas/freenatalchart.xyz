{-# LANGUAGE OverloadedStrings#-}

module Chart.CalculationsSpec (spec) where

import Import
    ( BirthData(BirthData),
      HoroscopeData(horoscopePlanetPositions, horoscopeHouses,
                    horoscopeSystem),
      Latitude(Latitude),
      Location(Location),
      Longitude(Longitude) )
import TestUtil ( testEphe, testTzDB )
import Arbitrary ()
import Chart.Calculations ( horoscope )
import Test.Hspec ( describe, Spec )
import Test.Hspec.QuickCheck ( prop )
import Test.QuickCheck
    ( Arbitrary(arbitrary), Gen, choose, forAll )
import Test.QuickCheck.Monadic ( assert, monadicIO, run )
import Data.Time.LocalTime.TimeZone.Detect (openTimeZoneDatabase, TimeZoneDatabase)
import System.IO.Unsafe (unsafePerformIO)
import SwissEphemeris (HouseSystem(Placidus, Porphyrius))
import RIO.Time (LocalTime(..))

tzDB :: TimeZoneDatabase
tzDB = unsafePerformIO $ openTimeZoneDatabase testTzDB
{-# NOINLINE tzDB #-}

notEmpty :: [a] -> Bool
notEmpty = not . null

spec :: Spec
spec = do
  describe "horoscope" $ do
    -- sanity check that horoscopes work. For more thorough properties
    -- of the underlying calculations, see:
    -- https://github.com/lfborjas/swiss-ephemeris/blob/master/test/SwissEphemerisSpec.hs
    prop "calculates horoscope data for birth data in valid times and places" $
      forAll genBirthData $ \(birthData) -> monadicIO $ do
        horoscope' <- run $ horoscope tzDB testEphe birthData
        assert $ notEmpty . horoscopePlanetPositions $ horoscope'
        assert $ (== 12) . length . horoscopeHouses  $ horoscope'
        assert $ (horoscopeSystem horoscope') `elem` [Placidus, Porphyrius]
  -- TODO: describe aspects

genBirthData :: Gen BirthData
genBirthData = do
  placeName <- pure "Test Location"
  placeLat  <- choose (-90.0, 90.0)
  placeLng  <- choose (-180.0, 180.0)
  time      <- arbitrary :: Gen LocalTime
  return $ (BirthData (Location placeName (Latitude placeLat) (Longitude placeLng)) time)
