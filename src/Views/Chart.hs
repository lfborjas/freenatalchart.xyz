{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, QuasiQuotes#-}

module Views.Chart (render, renderTestChartPage) where

import Import hiding (for_)
import Lucid
import Views.Common
import RIO.Time (parseTimeM, LocalTime, defaultTimeLocale, formatTime)
import Chart.Graphics (renderChart)
import Chart.Calculations (horoscope)
import qualified Graphics.Svg as Svg
import Data.Time.LocalTime.TimeZone.Detect (withTimeZoneDatabase)

render :: BirthData -> HoroscopeData-> Html ()
render BirthData{..} h@HoroscopeData{..} = html_ $ do
    head_ $ do
        title_ "Your Natal Chart"
        metaCeremony

    body_ $ do
        div_ [id_ "main", class_ "container"] $ do
            -- TODO: add a navbar/header?
            div_ [class_ "columns"] $ do
                div_ [class_ "column col-8"] $ do
                    dl_ [] $ do
                        dt_ [] "Place of Birth:"
                        dd_ [] (toHtml $ birthLocation & locationInput)

                        -- TODO: include timezone?
                        dt_ [] "Time of Birth:"
                        dd_ [] (toHtml $ birthLocalTime & formatTime defaultTimeLocale "%Y-%m-%d %l:%M:%S %P")

                        dt_ [] "Universal Time"
                        dd_ [] (toHtml $ horoscopeUniversalTime & formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z")

                        dt_ [] "Julian Day"
                        dd_ [] (toHtml $ horoscopeJulianTime & show)

                    toHtmlRaw $ Svg.renderBS $ renderChart 600 h

                div_ [class_ "column col-4"] $ do
                    div_ [class_ "accordion"] $ do
                        input_ [id_ "at-a-glance", type_ "radio", name_ "at-a-glance", hidden_ "", checked_]
                        label_ [class_ "accordion-header c-hand", for_ "at-a-glance"] $ do
                            i_ [] "At A Glance"


    footer_ [class_ "navbar bg-secondary"] $ do
        section_ [class_ "navbar-section"] $ do
            a_ [href_ "/about", class_ "btn btn-link", title_ "tl;dr: we won't sell you anything, or store your data."] "About"
        section_ [class_ "navbar-center"] $ do
            -- TODO: add a lil' icon?
            span_ "Brought to you by a ♑"
        section_ [class_ "navbar-section"] $ do
            a_ [href_ "https://github.com/lfborjas/freenatalchart.xyz", title_ "Made in Haskell with love and a bit of insanity.",  class_ "btn btn-link"] "Source Code"


renderTestChartPage :: IO ()
renderTestChartPage = do
    ephe <- pure $ "./config"
    withTimeZoneDatabase "./config/timezone21.bin" $ \db -> do
        birthplace <- pure $ Location "Tegucigalpa" (Latitude 14.0839053) (Longitude $ -87.2750137)
        birthtime  <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "1989-01-06 00:00:00" :: IO LocalTime
        let birthdata = BirthData birthplace birthtime
        calculations <- horoscope db ephe birthdata
        renderToFile "test-chart.html" $ render birthdata calculations
