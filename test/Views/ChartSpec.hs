{-# LANGUAGE OverloadedStrings #-}
module Views.ChartSpec (spec) where

import TestUtil (goldenFixtureText, testTzDB, goldenFixture, renderHtmlToString, testEphe)
import Views.Common (fixtureRenderContext)
import Views.Chart (render, renderText)
import Test.Hspec ( context, describe, it, Spec )
import Import
import RIO.Time ( defaultTimeLocale, parseTimeM )
import Ephemeris (horoscope)
import Data.Time.LocalTime.TimeZone.Detect (TimeZoneDatabase, openTimeZoneDatabase)
import System.IO.Unsafe (unsafePerformIO)
import Ephemeris.Types

testTZDB :: IO TimeZoneDatabase
testTZDB = openTimeZoneDatabase testTzDB

renderHoroscope :: String
renderHoroscope = unsafePerformIO $ do
  birthTime <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "1989-01-06 00:30:00" 
  tzDB <- testTZDB
  let birthData = 
        BirthData
        (Location "Teguz" (Latitude 14.0839053) (Longitude $ -87.2750137))
        birthTime
  horoscope' <- horoscope tzDB testEphe birthData
  return $ renderHtmlToString $ render fixtureRenderContext birthData horoscope'

renderHoroscopeText :: Text
renderHoroscopeText = unsafePerformIO $ do
  birthTime <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "1989-01-06 00:30:00" 
  tzDB <- testTZDB
  let birthData = 
        BirthData
        (Location "Teguz" (Latitude 14.0839053) (Longitude $ -87.2750137))
        birthTime
  horoscope' <- horoscope tzDB testEphe birthData
  return $ renderText fixtureRenderContext birthData horoscope'

spec :: Spec
spec = do
  describe "Chart page" $ do
    context "when given valid birth data" $ do
      it "renders a full chart page" $ do
        goldenFixture "natalChart" renderHoroscope

  describe "Chart page, text version" $ do
    context "when given valid birth data" $ do
      it "renders a full chart page, in text" $ do
        goldenFixtureText "natalChartText" renderHoroscopeText
