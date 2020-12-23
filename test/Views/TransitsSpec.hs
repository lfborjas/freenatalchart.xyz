{-# LANGUAGE OverloadedStrings #-}
module Views.TransitsSpec (spec) where

import TestUtil (goldenFixtureText, testTzDB)
import Views.Transits (renderText)
import Test.Hspec ( context, describe, it, Spec )
import Import
import RIO.Time ( defaultTimeLocale, parseTimeM, UTCTime, LocalTime )
import Ephemeris (transitData)
import Data.Time.LocalTime.TimeZone.Detect (TimeZoneDatabase, openTimeZoneDatabase)
import System.IO.Unsafe (unsafePerformIO)
import Ephemeris.Types
import Data.Time.Format.ISO8601 (iso8601ParseM)

tzDB :: TimeZoneDatabase
tzDB = unsafePerformIO $ openTimeZoneDatabase testTzDB

data TestViewContext = TestViewContext
  {
    configDir :: FilePath 
  } deriving (Eq, Show)

instance HasTimeZoneDatabase TestViewContext where
  timeZoneDatabaseL = lens (const tzDB) (const . id)

instance HasEphePath TestViewContext where
  ephePathL = lens configDir (const . id)

instance HasEphemerisDatabase TestViewContext where
  ephemerisDatabaseL = lens ((<> "/precalculated_ephemeris.db") . configDir) (const . id)

testContext :: TestViewContext
testContext = TestViewContext "./config"

renderTransits :: Text
renderTransits = unsafePerformIO $ do
  birthplace <- pure $ Location "Tegucigalpa" (Latitude 14.0839053) (Longitude $ -87.2750137)
  birthtime <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "1989-01-06 00:30:00" :: IO LocalTime
  -- see: 
  -- https://hackage.haskell.org/package/time-1.11.1.1/docs/Data-Time-Format-ISO8601.html
  -- for more useful 8601 functions
  momentOfTransit <- iso8601ParseM "2020-12-22T02:14:58.450Z" :: IO UTCTime
  let birthdata = BirthData birthplace birthtime
  transits <- transitData testContext momentOfTransit birthdata 
  pure $ renderText testContext birthdata momentOfTransit transits

spec :: Spec
spec =
  describe "Transits, text version" $ do
    context "when given valid birth data and a valid moment" $ do
      it "renders a full transits text file" $ do
        goldenFixtureText "transitsText" renderTransits
