{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ephemeris.Horoscope (horoscope, transitData) where

import Import
import Ephemeris.Types
import Data.Time.LocalTime.TimeZone.Detect (timeAtPointToUTC, TimeZoneDatabase)
import SwissEphemeris (eclipticToEquatorial, calculateEclipticPosition, calculateObliquity, calculateCusps, withEphemerides)
import Ephemeris.Aspect (transitingAspects, celestialAspects, planetaryAspects)
import Ephemeris.Utils (mkEcliptic, utcToJulian)
import Ephemeris.Planet (defaultPlanets)
import RIO.Time (UTCTime)
import Ephemeris.Transit (transits)

horoscope :: TimeZoneDatabase -> EphemeridesPath -> BirthData -> IO HoroscopeData
horoscope timezoneDB ephePath BirthData {..} = do
  latitude <- pure $ birthLocation & locationLatitude & unLatitude
  longitude <- pure $ birthLocation & locationLongitude & unLongitude
  -- convert to what the underlying library expects: a UTC time, and a pair of raw coordinates.
  uTime <- timeAtPointToUTC timezoneDB latitude longitude birthLocalTime
  time <- pure $ utcToJulian uTime
  place <- pure $ locationToGeo birthLocation

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

transitData :: 
  (HasTimeZoneDatabase ctx, HasEphePath ctx, HasEphemerisDatabase ctx)
  => ctx
  -> UTCTime
  -> BirthData
  -> IO TransitData
transitData ctx momentOfTransit BirthData {..} = do
  let timezoneDB = ctx ^. timeZoneDatabaseL
      ephePath   = ctx ^. ephePathL
      epheDB     = ctx ^. ephemerisDatabaseL

  latitude <- pure $ birthLocation & locationLatitude & unLatitude
  longitude <- pure $ birthLocation & locationLongitude & unLongitude
  -- convert to what the underlying library expects: a UTC time, and a pair of raw coordinates.
  uTime <- timeAtPointToUTC timezoneDB latitude longitude birthLocalTime
  natalTime <- pure $ utcToJulian uTime
  transitTime <- pure $ utcToJulian momentOfTransit
  place <- pure $ locationToGeo birthLocation

  withEphemerides ephePath $ do
    -- we `fail` if the obliquity couldn't be calculated, since it should be available for any moment in the supported
    -- time range!
    natalObliquity <- obliquityOrBust natalTime
    natalPositions <- planetPositions natalObliquity natalTime
    (CuspsCalculation natalCusps natalAngles' natalSys) <- calculateCusps Placidus natalTime place

    transitObliquity <- obliquityOrBust transitTime
    transitPositions <- planetPositions transitObliquity transitTime
    (CuspsCalculation transitCusps transitAngles transitSys) <- calculateCusps Placidus transitTime place

    pAspects <- pure $ transitingAspects natalPositions transitPositions
    pTransitActivity <- transits epheDB transitTime pAspects

    return $
      TransitData {
        natalPlanetPositions = natalPositions
      , natalAngles = natalAngles'
      , natalHouses = houses natalObliquity natalCusps
      , natalHouseSystem = natalSys
      , transitingPlanetPositions = transitPositions
      , transitingHouses = houses transitObliquity transitCusps
      , transitingAngles = transitAngles
      , transitingHouseSystem = transitSys
      , transitPlanetaryAspects = transitingAspects transitPositions natalPositions
      , transitActivity = pTransitActivity
      }

locationToGeo :: Location -> GeographicPosition
locationToGeo Location {..} =
  GeographicPosition {
      geoLat = (locationLatitude & unLatitude)
    , geoLng = (locationLongitude & unLongitude)
    }


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
