{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Views.Transits (render, renderText) where

import Import
import RIO.Time (rfc822DateFormat, defaultTimeLocale, formatTime, UTCTime)
import Ephemeris
import Lucid hiding (renderText)
import Views.Chart.Common
import RIO.Writer (Writer, MonadWriter(tell), execWriter)
import RIO.Text (justifyLeft, pack)
import Data.Foldable (Foldable(maximum))
import RIO.List (repeat, headMaybe)
import qualified RIO.Text as T
import Views.Common
import qualified Graphics.Svg as Svg
import Chart.Graphics (renderTransitChart)


renderText :: a -> BirthData -> UTCTime -> TransitData -> Text
renderText _ BirthData {..} transitMoment TransitData {..} =
  execWriter $ do
    ln_ "Freenatalchart.xyz"
    ln_ "=================="
    ln_ "Transits for:"
    ln_ . pack $ transitMoment & formatUTCTimestamp
    ln_ $ (birthLocation & locationInput) <> " " <> (latLngTxt birthLocation)
    ln_ ""
    ln_ "Natal Planet Positions"
    ln_ "----------------------"
    ln_ . heading $ 
      [
        justifyPlanetPos "Planet", justifyHouseNum "House",
        justifyLongitude "Longitude", justifyDouble "Speed",
        justifyLatitude "Latitude", justifyDeclination "Declination",
        "Zodiac Longitude"
      ]
    forM_ natalPlanetPositions $ \np@PlanetPosition{..} -> do
      if isRetrograde np then
        tell . justifyPlanetPos $ (labelText planetName)  <> " (r)"
      else
        tell . justifyPlanetPos $ (labelText planetName)
      tell "|"
      tell . justifyHouseNum $ housePositionText $ housePosition natalHouses planetLng
      tell "|"
      tell . justifyLongitude . pack $ formatDouble $ unLongitude planetLng
      tell "|"
      tell . justifyDouble . pack $ formatDouble $ planetLngSpeed
      tell "|"
      tell . justifyLatitude . pack $ formatDouble $ unLatitude planetLat
      tell "|"
      tell . justifyDeclination . pack $ formatDouble $ planetDeclination
      tell "|"
      tell $ textDegreesZodiac planetLng
      ln_ ""
    ln_ ""
    ln_ "Transiting Planet Positions"
    ln_ "---------------------------"
    ln_ . heading $ 
      [
        justifyPlanetPos "Planet", justifyHouseNum "House",
        justifyLongitude "Longitude", justifyDouble "Speed",
        justifyLatitude "Latitude", justifyDeclination "Declination",
        "Zodiac Longitude"
      ]
    forM_ transitingPlanetPositions $ \tp@PlanetPosition{..} -> do
      if isRetrograde tp then
        tell . justifyPlanetPos $ (labelText planetName)  <> " (r)"
      else
        tell . justifyPlanetPos $ (labelText planetName)
      tell "|"
      -- NOTE(luis) this is intentional: we want to show which natal house a transiting
      -- planet is in.
      tell . justifyHouseNum $ housePositionText $ housePosition natalHouses planetLng
      tell "|"
      tell . justifyLongitude . pack $ formatDouble $ unLongitude planetLng
      tell "|"
      tell . justifyDouble . pack $ formatDouble $ planetLngSpeed
      tell "|"
      tell . justifyLatitude . pack $ formatDouble $ unLatitude planetLat
      tell "|"
      tell . justifyDeclination . pack $ formatDouble $ planetDeclination
      tell "|"
      tell $ textDegreesZodiac planetLng
      ln_ ""
    ln_ ""
    ln_ "Planetary Transits"
    ln_ "------------------"
    transitActivity "to Natal Planets" transitMoment planetaryTransits
    ln_ "Axes Transits"
    ln_ "-------------"
    transitActivity "to Natal Axes" transitMoment angleTransits

aspectsHeading :: Text
aspectsHeading = 
  heading [
      justifyAspecting "Aspecting", justifyAspect "Aspect"
    , justifyAspected "Aspected", justifyDouble "Angle"
    , justifyDouble "Orb"
    ]

transitActivity :: HasLabel a => Text -> UTCTime -> [(TransitAspect a, Transit a)] -> (Writer Text ())
transitActivity extraHeading moment transits' = do
  ln_ ""
  ln_ $ "All Aspects " <> extraHeading
  ln_ $ "~~~~~~~~~~~~" <> pack (repeat '~' & take (T.length extraHeading))
  ln_ aspectsHeading
  forM_  (transitAspects transits') $ \(HoroscopeAspect aspect (aspecting, aspected) angle orb) -> do
    tell . justifyAspecting . labelText . planetName $ aspecting
    tell "|"
    tell . justifyAspect . labelText . aspectName $ aspect
    tell "|"
    tell . justifyAspected . labelText  $ aspected
    tell "|"
    tell . justifyDouble . pack $ formatDouble angle
    tell "|"
    tell . justifyDouble . pack $ formatDouble orb
    ln_ ""
  ln_ ""
  ln_ $ "Active Transits " <> extraHeading
  ln_ $ "~~~~~~~~~~~~~~~~" <> pack (repeat '~' & take (T.length extraHeading))
  ln_ . heading $
    [
      justifyTransiting "Transiting", justifyAspect "Aspect", justifyTransited "Transited",
      justifyTimestamp "Starts", justifyTimestamp "Ends",
      justifyTimestamp "Exact On"
    ]
  forM_ (transitActivityAround moment transits') $ \(a,t) -> do
    tell . justifyTransiting . labelText . planetName . transiting $ t
    tell "|"
    tell . justifyAspect . labelText . aspectName . aspect $ a
    tell "|"
    tell . justifyTransited . labelText . transited $ t
    tell "|"
    tell . justifyTimestamp . pack . (maybe "N/A" formatUTCTimestamp) . transitStarts $ t
    tell "|"
    tell . justifyTimestamp . pack . (maybe "N/A" formatUTCTimestamp) . transitEnds $ t
    tell "|"
    tell . justifyTimestamp . pack . (maybe "N/A" formatUTCTimestamp) . headMaybe . immediateTriggers  $ t
    ln_ ""
  ln_ ""
  where
    justifyPlanetOrString :: String -> (Text -> Text)
    justifyPlanetOrString s = justifyLeft (maximum [length s, planetWidth]) ' '
    justifyTransiting = justifyPlanetOrString "Transiting"
    justifyTransited  = justifyPlanetOrString "Transited"
    justifyTimestamp  = justifyLeft 24 ' '


render :: HasStaticRoot a => a -> BirthData -> UTCTime -> TransitData -> Html ()
render renderCtx BirthData {..} transitMoment t@TransitData{..} = html_ $ do
  head_ $ do
    title_ "Your Current Transits"
    metaCeremony renderCtx
    style_ $ do
      "svg { height: auto; width: auto}\
      \.table-hover-dark tr:hover{ border-bottom: .05rem solid #9da8ff !important; }\
      \.light-links a{ color: white !important; }\
      \.transiting-planet{ stroke: purple; fill: purple; }\
      \"

  body_ $ do
    navbar_

    main_ [id_ "main", class_ "container grid-xl mx-4 under-navbar"] $ do
      div_ [class_ "blue-stars-bg text-center"] $ do
        p_ $ do
          toHtml $ birthLocalTime & formatTime defaultTimeLocale rfc822DateFormat
          br_ []
          toHtml $ birthLocation & locationInput
        p_ $ do
          "Transits as of: "
          toHtml $ transitMoment & formatTime defaultTimeLocale rfc822DateFormat

      figure_ [class_ "figure p-centered my-2", style_ "max-width: 600px;"] $ do
        div_ [] $ do
          (toHtmlRaw $ Svg.renderBS $ renderTransitChart [Svg.makeAttribute "height" "not", Svg.makeAttribute "width" "not"] 600 t)

      ul_ [class_ "tab tab-block tab-block-dark"] $ do
        li_ [class_ "tab-item active"] $ do
          a_ [href_ "#analyze"] "Analyze"
        li_ [class_ "tab-item"] $ do
          a_ [href_ "#understand"] "Understand"
        li_ [class_ "tab-item"] $ do
          a_ [href_ "#introspect"] "Introspect"

      div_ [class_ "divider", id_ "analyze"] ""

      details_ [id_ "natal-planet-positions", class_ "accordion my-2", open_ ""] $ do
        summary_ [class_ "accordion-header"] $ do
          headerIcon
          sectionHeading $ do
            "Natal Planet Positions"      
        div_ [class_ "accordion-body scrollable-container"] $ do
          planetPositionsTable natalPlanetPositions natalHouses
      div_ [class_ "divider"] ""

      details_ [id_ "transiting-planet-positions", class_ "accordion my-2", open_ ""] $ do
        summary_ [class_ "accordion-header"] $ do
          headerIcon
          sectionHeading $ do
            "Transiting Planet Positions"      
        div_ [class_ "accordion-body scrollable-container"] $ do
          planetPositionsTable transitingPlanetPositions natalHouses
      div_ [class_ "divider"] ""

    link_ [rel_ "stylesheet", href_ "https://unpkg.com/spectre.css/dist/spectre-icons.min.css"]
    footerNav

navbar_ :: Html ()
navbar_ =
  header_ [class_ "navbar bg-dark navbar-fixed navbar-fixed-top"] $ do
    section_ [class_ "navbar-section"] $ do
      a_ [href_ "/", class_ "mr-2"] $ do
        i_ [class_ "icon icon-refresh", title_ "Recalculate Transits"] ""
        span_ [class_ "hide-sm"] " Recalculate Transits"
    section_ [class_ "navbar-section navbar-center navbar-brand"] $ do
       a_ [href_ "/", class_ "brand-text"] "FreeNatalChart.xyz"
    section_ [class_ "navbar-section"] $ do
      a_ [href_ "#main"] $ do
        span_ [class_ "hide-sm"] "Back to Top "
        i_ [class_ "icon icon-upward", title_ "Back to Top"] ""
