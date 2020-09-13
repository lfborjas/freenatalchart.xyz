{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Views.Chart (render, renderTestChartPage) where

import Chart.Calculations (horoscope)
import Chart.Graphics (renderChart)
import Data.Time.LocalTime.TimeZone.Detect (withTimeZoneDatabase)
import qualified Graphics.Svg as Svg
import Import hiding (for_)
import Lucid
import RIO.Text (pack)
import RIO.Time (LocalTime, defaultTimeLocale, formatTime, parseTimeM)
import SwissEphemeris (ZodiacSignName (..))
import Views.Common

render :: BirthData -> HoroscopeData -> Html ()
render BirthData {..} h@HoroscopeData {..} = html_ $ do
  head_ $ do
    title_ "Your Natal Chart"
    metaCeremony

  body_ $ do
    div_ [id_ "main", class_ "container"] $ do
      -- TODO: add a navbar/header?
      div_ [class_ "columns"] $ do
        div_ [class_ "column col-8"] $ do
          div_ [id_ "chart", class_ "p-centered my-2"] $ do
            toHtmlRaw $ Svg.renderBS $ renderChart 600 h

          dl_ [] $ do
            dt_ [] "Place of Birth:"
            dd_ [] (toHtml $ birthLocation & locationInput)

            dt_ [] "Sun sign"
            dd_ [] (asIcon Capricorn)

            -- TODO: include timezone?
            dt_ [] "Time of Birth:"
            dd_ [] (toHtml $ birthLocalTime & formatTime defaultTimeLocale "%Y-%m-%d %l:%M:%S %P")
            dt_ [] "Universal Time"
            dd_ [] (toHtml $ horoscopeUniversalTime & formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z")

            dt_ [] "Julian Day"
            dd_ [] (toHtml $ horoscopeJulianTime & show)

    -- the SVG font for all icons.
    link_ [rel_ "stylesheet", href_ "/css/freenatalchart-icons.css"]
  footer_ [class_ "navbar bg-secondary"] $ do
    section_ [class_ "navbar-section"] $ do
      a_ [href_ "/about", class_ "btn btn-link", title_ "tl;dr: we won't sell you anything, or store your data."] "About"
    section_ [class_ "navbar-center"] $ do
      -- TODO: add a lil' icon?
      span_ "Brought to you by a ♑"
    section_ [class_ "navbar-section"] $ do
      a_ [href_ "https://github.com/lfborjas/freenatalchart.xyz", title_ "Made in Haskell with love and a bit of insanity.", class_ "btn btn-link"] "Source Code"

asIcon :: ZodiacSignName -> Html ()
asIcon z = i_ [class_ (pack $ "fnc-" <> (show z)), title_ (pack $ show z)] ""

renderTestChartPage :: IO ()
renderTestChartPage = do
  ephe <- pure $ "./config"
  withTimeZoneDatabase "./config/timezone21.bin" $ \db -> do
    birthplace <- pure $ Location "Tegucigalpa" (Latitude 14.0839053) (Longitude $ -87.2750137)
    birthtime <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "1989-01-06 00:00:00" :: IO LocalTime
    let birthdata = BirthData birthplace birthtime
    calculations <- horoscope db ephe birthdata
    renderToFile "test-chart.html" $ render birthdata calculations
