{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ephemeris.Horoscope where

import Import
import Ephemeris.Types
import Data.Time.LocalTime.TimeZone.Detect (timeAtPointToUTC, TimeZoneDatabase)
import SwissEphemeris (eclipticToEquatorial, calculateEclipticPosition, calculateObliquity, calculateCusps, withEphemerides)
import Ephemeris.Aspect (celestialAspects, planetaryAspects)
import Ephemeris.Utils (mkEcliptic, utcToJulian)
import Ephemeris.Planet (defaultPlanets)

horoscope :: TimeZoneDatabase -> EphemeridesPath -> BirthData -> IO HoroscopeData
horoscope timezoneDB ephePath BirthData {..} = do
  latitude <- pure $ birthLocation & locationLatitude & unLatitude
  longitude <- pure $ birthLocation & locationLongitude & unLongitude
  -- convert to what the underlying library expects: a UTC time, and a pair of raw coordinates.
  uTime <- timeAtPointToUTC timezoneDB latitude longitude birthLocalTime
  time <- pure $ utcToJulian uTime
  place <- pure $ GeographicPosition {geoLat = latitude, geoLng = longitude}

  withEphemerides ephePath $ do
    -- we `fail` if the obliquity couldn't be calculated, since it should be available for any moment in the supported
    -- time range!
    obliquity <- obliquityOrBust time
    positions <- planetPositions obliquity time
    (CuspsCalculation cusps angles' sys) <- calculateCusps Placidus time place
    return $
      HoroscopeData
        positions
        angles'
        (houses obliquity cusps)
        sys
        (planetaryAspects positions)
        (celestialAspects positions angles')
        uTime
        time

obliquityOrBust :: JulianTime -> IO ObliquityInformation
obliquityOrBust time = do
  obliquity <- calculateObliquity time
  case obliquity of
    Left e -> fail $ "Unable to calculate obliquity: " <> e <> " (for time: " <> (show time) <> ")"
    Right o -> pure o

planetPositions :: ObliquityInformation -> JulianTime -> IO [PlanetPosition]
planetPositions o@ObliquityInformation {..} time = do
  maybePositions <- forM defaultPlanets $ \p -> do
    coords <- calculateEclipticPosition time p
    case coords of
      Left _ -> pure Nothing
      Right c -> do
        let decl = eclipticToEquatorial o c & declination
        pure $ Just $ PlanetPosition p (Latitude . lat $ c) (Longitude . lng $ c) (lngSpeed c) decl
  pure $ catMaybes maybePositions

houses :: ObliquityInformation -> [HouseCusp] -> [House]
houses obliquity cusps =
  map buildHouse $ (zip [I .. XII] cusps)
  where
    buildHouse (n, c) =
      House n (Longitude c) (declination equatorial)
      where
        equatorial = eclipticToEquatorial obliquity (mkEcliptic {lng = c})
