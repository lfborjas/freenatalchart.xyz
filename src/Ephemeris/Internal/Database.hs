{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Ephemeris.Internal.Database where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow ()
import Database.SQLite.Simple.Internal (Field (Field))
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.QQ
import Import
import qualified RIO.Text as T
import SwissEphemeris
    ( calculateEclipticPosition,
      withEphemerides,
      Planet(..))
import Ephemeris.Types
import Ephemeris.Planet
import Ephemeris.Internal.Approximations


data EclipticLongitudeEphemeris = EclipticLongitudeEphemeris
  { planet :: Planet,
    julianTime :: JulianTime,
    longitude :: Double,
    longitudinalSpeed :: Double
  }

toEphemeris :: Planet -> JulianTime -> EclipticPosition -> EclipticLongitudeEphemeris
toEphemeris p t (EclipticPosition lo _ _ ls _ _) =
  EclipticLongitudeEphemeris p t lo ls


-- Some missing instances for the SwissEphemeris types
-- TODO: consider moving up to Ephemeris.Types
deriving stock instance Read Planet
deriving newtype instance Num JulianTime

instance FromField Planet where
  fromField f@(Field (SQLText txt) _) =
    case (readMaybe s) of
      Just p -> Ok p
      Nothing -> returnError ConversionFailed f ("Invalid planet value: " <> s)
    where
      s = T.unpack txt
  fromField f = returnError ConversionFailed f ("Expected text value.")

instance ToField Planet where
  toField = SQLText . T.pack . show

instance FromField JulianTime where
  fromField (Field (SQLFloat flt) _) = Ok . JulianTime $ flt
  fromField f = returnError ConversionFailed f "Expected a float."

instance ToField JulianTime where
  toField (JulianTime d) = SQLFloat d

instance FromField Longitude where
  fromField (Field (SQLFloat flt) _) = Ok . Longitude $ flt
  fromField f = returnError ConversionFailed f "Expected a float."

instance ToField Longitude where
  toField (Longitude d) = SQLFloat d

instance FromRow EclipticLongitudeEphemeris where
  fromRow = EclipticLongitudeEphemeris <$> field
      <*> field
      <*> field
      <*> field


-- | Given a Planet, a Julian Time and a Longitude, find the day
-- in an appropriate time range when said planet is likely to cross over
-- the given Longitude.
crossingCandidateQuery :: Connection -> Planet -> Longitude -> JulianTime -> IO (Maybe JulianTime)
crossingCandidateQuery conn crossingPlanet soughtLongitude (JulianTime soughtTime) = do
  -- results :: IO [Only (Maybe Double)]
  results <-
    query conn [sql|
        select max(julian_time) from ecliptic_longitude_ephemeris
        where planet = ? 
        and longitude between ? and ?
        and julian_time between ? and ?
      |] (crossingPlanet, lowerLongitudeBound, upperLongitudeBound, lowerTimeBound, upperTimeBound)
  case results of
    [] -> pure Nothing
    (Only x:_) -> pure (JulianTime <$> x)
  where
    -- see Approximations.hs:
    -- a lot of ranges depend on the speed of the transiting planet,
    -- so we look for movement between the sought point and the longitude
    -- the planet would theoretically be at a day before, depending on the speed.
    -- Same for the range of time observed: enough to move ~180 degrees (less
    -- due to retrograde motion,) which is enough to have begun its approach to the position.
    lowerLongitudeBound = orbBefore crossingPlanet soughtLongitude 1
    upperLongitudeBound = soughtLongitude
    lowerTimeBound      = soughtTime - (maxDayDelta crossingPlanet)
    upperTimeBound      = soughtTime + (maxDayDelta crossingPlanet)

{-
select t,longitude,l,lng_speed_signum from (
  select max(julian_time)as t,longitude, 'end' as l, lng_speed_signum from ecliptic_longitude_ephemeris where planet = 'Pluto' and julian_time between 2457023.5 and 2463232.5  and longitude between 292.9268 and 293.9268
  union
  select min(julian_time)as t,longitude, 'start' as l, lng_speed_signum from ecliptic_longitude_ephemeris where planet = 'Pluto' and julian_time between 2457023.5 and 2463232.5 and longitude between 291.9268 and 292.9268
) where t is not null order by t asc;
-}

activityPeriodQuery :: Connection -> Planet -> Longitude -> JulianTime -> IO (Maybe JulianTime, Maybe JulianTime)
activityPeriodQuery conn crossingPlanet soughtLongitude (JulianTime soughtTime) = do
  results <-
    queryNamed conn [sql|
      select t from (
        select min(julian_time) as t from ecliptic_longitude_ephemeris 
          where planet = :planet
          and julian_time between :start and :end
          and longitude between :lowerBound and :longitude
        union
        select max(julian_time) as t from ecliptic_longitude_ephemeris
          where planet = :planet 
          and julian_time between :start and :end
          and longitude between :longitude and :upperBound
      )
    |] [ ":planet" := crossingPlanet
       , ":start" := lowerTimeBound
       , ":end" := upperTimeBound
       , ":lowerBound" := lowerLongitudeBound
       , ":upperBound" := upperLongitudeBound
       , ":longitude" := soughtLongitude
       ]
  case results of
    [] -> pure (Nothing, Nothing)
    (Only s):(Only e):_ -> pure (JulianTime <$> s, JulianTime <$> e)
    [(Only _)] -> pure (Nothing, Nothing)
  where
    lowerTimeBound = soughtTime - (maxDayDelta crossingPlanet)
    upperTimeBound = soughtTime + (maxDayDelta crossingPlanet)
    lowerLongitudeBound = orbBefore crossingPlanet soughtLongitude 1
    upperLongitudeBound = orbAfter crossingPlanet soughtLongitude 1


-- query utils

-- TODO: note that the orb* functions have a /literal/ edge case:
-- if the orb is too close to the 0/360 point, we don't currently
-- query cleanly, we just cut it off at that point. For fast-moving
-- bodies, this will mean that there will be some misses, if the
-- distance between 0/360 and the sought longitude is smaller than
-- the speed at the given time, the crossing will happen "under the radar:"
-- too fast for the coarse day-step ephemeris to detect without
-- "crossing over" the 0/360 point. This is totally feasible, I think,
-- just not something I can figure out currently with good SQL!

-- | Given a planet, a longitude and a desired orb, find the longitude
-- where the orb "begins". Note that if the orb is smaller than the planet's
-- max daily speed in the ephemeris DB, the latter will be preferred when calculating.
-- We also account for "crossing over" the beginning of the ecliptic circle.
orbBefore :: Planet -> Longitude -> Double -> Longitude
orbBefore planet (Longitude lng) orb =
  if nonAngularVal < 0 then
    Longitude 0 -- nonAngularVal + 360
  else
    Longitude nonAngularVal
  where
    nonAngularVal = lng - preferredOrb
    preferredOrb = max (maxSpeed planet) orb

-- | Given a planet, a longitude and a desired orb, find the longitude
-- where the orb "begins". Note that if the orb is smaller than the planet's
-- max daily speed in the ephemeris DB, the latter will be preferred when calculating.
-- We also account for "crossing over" the beginning of the ecliptic circle.
orbAfter :: Planet -> Longitude -> Double -> Longitude
orbAfter planet (Longitude lng) orb =
  if nonAngularVal >= 360 then
    Longitude 360 -- nonAngularVal - 360
  else
    Longitude nonAngularVal
  where
    nonAngularVal = lng + preferredOrb
    preferredOrb = max (maxSpeed planet) orb


--
-- Dev Utilities
-- 

devConnection :: IO Connection
devConnection = open "./config/precalculated_ephemeris.db"

ephemeridesFor :: Planet -> (JulianTime, JulianTime) -> Double -> IO [Either String EclipticLongitudeEphemeris]
ephemeridesFor planet (JulianTime start, JulianTime end) step =
  mapM (planetPositionAt planet) [start, (start + step) .. end]
  where
    planetPositionAt p t =
      fmap (toEphemeris p t') <$> calculateEclipticPosition t' p
      where
        t' = JulianTime t

insertEphemeris :: Connection -> Either String EclipticLongitudeEphemeris -> IO ()
insertEphemeris _ (Left _) = pure ()
insertEphemeris conn (Right EclipticLongitudeEphemeris {..}) = do
  execute
    conn
    "INSERT INTO ecliptic_longitude_ephemeris (planet, julian_time, longitude, speed) VALUES (?,?,?,?)"
    (planet, julianTime, longitude, longitudinalSpeed)


populateEphemeris :: (JulianTime, JulianTime) -> IO ()
populateEphemeris range = do
  conn <- open "./config/precalculated_ephemeris.db"
  forM_ defaultPlanets $ \p -> do
    ephe <- ephemeridesFor p range 1.0
    forM_ (ephe) $ \e -> do
      insertEphemeris conn e

  close conn

-- one year of ephemeris takes 3.19 seconds to calculate, and occupies ~700kb in disk; producing 4771 rows.
-- (to time the execution, I used `:set +s` in `ghci`)
populateEphemeris2020 :: IO ()
populateEphemeris2020 =
  withEphemerides "./config" $
    populateEphemeris (JulianTime 2458849.5, JulianTime 2459215.5)

-- | Pre-populate 16 years of ephemeris, between 1/1/2015 and 1/1/2032.
-- takes about 53 seconds and occupies 13Mb on disk; 80730 rows.
-- I used https://ssd.jpl.nasa.gov/tc.cgi#top for easy string/julian
-- conversion.
-- To pre-populate for your own purposes, you'll have to export this 
-- function explicitly.
prepopulateEphemeris :: IO ()
prepopulateEphemeris =
  withEphemerides "./config" $
    populateEphemeris (JulianTime 2457023.5, JulianTime 2463232.5) 

{-
Further notes at: https://gist.github.com/lfborjas/ce12f992d64096b4b87b936a9868ae49
-}
