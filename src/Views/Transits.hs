{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Views.Transits (render, renderText) where

import Import
import RIO.Time (UTCTime)
import Ephemeris
import Lucid hiding (renderText)
import Views.Chart.Common
import RIO.Writer (Writer, MonadWriter(tell), execWriter)
import RIO.Text (justifyLeft, pack)
import Data.Foldable (Foldable(maximum))
import RIO.List (repeat, headMaybe)
import qualified RIO.Text as T


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


render :: a -> BirthData -> UTCTime -> TransitData -> Html ()
render = mempty
