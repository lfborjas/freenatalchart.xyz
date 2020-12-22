{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Views.Transits (render, renderText) where

import Import
import RIO.Time (UTCTime)
import Ephemeris
import Lucid hiding (renderText)
import Views.Chart.Common
import RIO.Writer (MonadWriter(tell), execWriter)
import RIO.Text (justifyLeft, pack)
import Data.Foldable (Foldable(maximum))
import RIO.List (headMaybe)


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
      tell . justifyHouseNum $ housePositionText $ housePosition transitingHouses planetLng
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
    ln_ "Planetary Aspects"
    ln_ "-----------------"
    ln_ aspectsHeading
    forM_  transitPlanetaryAspects $ \(HoroscopeAspect aspect (aspecting, aspected) angle orb) -> do
      tell . justifyAspecting . labelText . planetName $ aspecting
      tell "|"
      tell . justifyAspect . labelText . aspectName $ aspect
      tell "|"
      tell . justifyAspected . labelText . planetName $ aspected
      tell "|"
      tell . justifyDouble . pack $ formatDouble angle
      tell "|"
      tell . justifyDouble . pack $ formatDouble orb
      ln_ ""
    ln_ ""
    ln_ "Transits"
    ln_ "--------"
    ln_ . heading $
      [
        justifyTransiting "Transiting", justifyTransited "Transited",
        justifyTimestamp "Starts", justifyTimestamp "Ends",
        justifyTimestamp "Exact On"
      ]
    forM_ transitActivity $ \t -> do
      tell . justifyTransiting . labelText . planetName . transiting $ t
      tell "|"
      tell . justifyTransited . labelText . planetName . transited $ t
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
    aspectsHeading = 
      heading [
          justifyAspecting "Aspecting", justifyAspect "Aspect"
        , justifyAspected "Aspected", justifyDouble "Angle"
        , justifyDouble "Orb"
        ]

render :: a -> BirthData -> UTCTime -> TransitData -> Html ()
render = mempty
