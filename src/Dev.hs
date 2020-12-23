{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Dev where

import Chart.Graphics (renderTransitChart, renderChart)
import Data.Time.LocalTime.TimeZone.Detect (openTimeZoneDatabase, TimeZoneDatabase, withTimeZoneDatabase)
import Ephemeris
import qualified Graphics.Svg as Svg
import Import
import Lucid (renderToFile)
import RIO.Time (LocalTime, defaultTimeLocale, parseTimeM, UTCTime)
import Views.Common (fixtureRenderContext)
import qualified Views.Index as Index
import qualified Views.Chart as Chart
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Views.Transits as Transits

tzDB :: TimeZoneDatabase
tzDB = unsafePerformIO $ openTimeZoneDatabase "./config/timezone21.bin"
{-# NOINLINE tzDB #-}

data DevContext = DevContext
  {
    configDir :: FilePath 
  } deriving (Eq, Show)

instance HasTimeZoneDatabase DevContext where
  timeZoneDatabaseL = lens (const tzDB) (const . id)

instance HasEphePath DevContext where
  ephePathL = lens configDir (const . id)

instance HasEphemerisDatabase DevContext where
  ephemerisDatabaseL = lens ((<> "/precalculated_ephemeris.db") . configDir) (const . id)

devContext :: DevContext
devContext = DevContext "./config"

-- | render the index page to a known test location
renderTestIndex :: IO ()
renderTestIndex = renderToFile "dev/files/index.html" $ Index.render fixtureRenderContext Nothing

renderTestChart :: IO ()
renderTestChart = do
  ephe <- pure $ "./config"
  withTimeZoneDatabase "./config/timezone21.bin" $ \db -> do
    birthplace <- pure $ Location "Tegucigalpa" (Latitude 14.0839053) (Longitude $ -87.2750137)
    birthtime <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "1989-01-06 00:00:00" :: IO LocalTime
    calculations <- horoscope db ephe (BirthData birthplace birthtime)
    Svg.renderToFile "dev/files/radix.svg" $ renderChart [] 400 calculations

renderTestTransitChart :: IO ()
renderTestTransitChart = do
  birthplace <- pure $ Location "Tegucigalpa" (Latitude 14.0839053) (Longitude $ -87.2750137)
  birthtime <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "1989-01-06 00:30:00" :: IO LocalTime
  -- see: 
  -- https://hackage.haskell.org/package/time-1.11.1.1/docs/Data-Time-Format-ISO8601.html
  -- for more useful 8601 functions
  momentOfTransit <- iso8601ParseM "2020-12-22T02:14:58.450Z" :: IO UTCTime
  let birthdata = BirthData birthplace birthtime
  transits' <- transitData devContext momentOfTransit birthdata
  Svg.renderToFile "dev/files/transit-radix.svg" $ renderTransitChart [] 400  transits'


renderTestChartPage :: IO ()
renderTestChartPage = do
  ephe <- pure $ "./config"
  withTimeZoneDatabase "./config/timezone21.bin" $ \db -> do
    birthplace <- pure $ Location "Tegucigalpa" (Latitude 14.0839053) (Longitude $ -87.2750137)
    birthtime <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "1989-01-06 00:30:00" :: IO LocalTime
    let birthdata = BirthData birthplace birthtime
    calculations <- horoscope db ephe birthdata
    renderToFile "dev/files/chart.html" $ Chart.render fixtureRenderContext birthdata calculations

renderTestTransitsText :: IO ()
renderTestTransitsText = do
  birthplace <- pure $ Location "Tegucigalpa" (Latitude 14.0839053) (Longitude $ -87.2750137)
  birthtime <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "1989-01-06 00:30:00" :: IO LocalTime
  -- see: 
  -- https://hackage.haskell.org/package/time-1.11.1.1/docs/Data-Time-Format-ISO8601.html
  -- for more useful 8601 functions
  momentOfTransit <- iso8601ParseM "2020-12-22T02:14:58.450Z" :: IO UTCTime
  let birthdata = BirthData birthplace birthtime
  transits' <- transitData devContext momentOfTransit birthdata 
  writeFileUtf8 "dev/files/transits20201222.txt" $ Transits.renderText devContext birthdata momentOfTransit transits'


renderTestTransitsPage :: IO ()
renderTestTransitsPage = do
  birthplace <- pure $ Location "Tegucigalpa" (Latitude 14.0839053) (Longitude $ -87.2750137)
  birthtime <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "1989-01-06 00:30:00" :: IO LocalTime
  -- see: 
  -- https://hackage.haskell.org/package/time-1.11.1.1/docs/Data-Time-Format-ISO8601.html
  -- for more useful 8601 functions
  momentOfTransit <- iso8601ParseM "2020-12-22T02:14:58.450Z" :: IO UTCTime
  let birthdata = BirthData birthplace birthtime
  transits' <- transitData devContext momentOfTransit birthdata 
  renderToFile "dev/files/transits.html" $ Transits.render fixtureRenderContext birthdata momentOfTransit transits'
