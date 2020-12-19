{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Dev where

import Chart.Graphics (renderChart)
import Data.Time.LocalTime.TimeZone.Detect (withTimeZoneDatabase)
import Ephemeris
import qualified Graphics.Svg as Svg
import Import
import Lucid (renderToFile)
import RIO.Time (LocalTime, defaultTimeLocale, parseTimeM)
import Views.Common (fixtureRenderContext)
import qualified Views.Index as Index
import qualified Views.Chart as Chart

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


renderTestChartPage :: IO ()
renderTestChartPage = do
  ephe <- pure $ "./config"
  withTimeZoneDatabase "./config/timezone21.bin" $ \db -> do
    birthplace <- pure $ Location "Tegucigalpa" (Latitude 14.0839053) (Longitude $ -87.2750137)
    birthtime <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "1989-01-06 00:30:00" :: IO LocalTime
    let birthdata = BirthData birthplace birthtime
    calculations <- horoscope db ephe birthdata
    renderToFile "dev/files/chart.html" $ Chart.render fixtureRenderContext birthdata calculations
