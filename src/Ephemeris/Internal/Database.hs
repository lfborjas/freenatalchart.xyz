{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Ephemeris.Internal.Database where

{-
Precomputed ephemeris, and functions to interact with them

* Need a function to find all ephemeris in a range of years, for a given planet and step (default 1 day, moon can bee 1/2 day,)
* Another function to set up the tables
* Another to populate the whole thing
* Another to, given a longitude, find the day(s) a planet is closest to it; depending on the
  longitudinal speed, we'll work with the previous/next position given the step used:
  this is our bracket for root finding (i.e. we just need the days, the previous/next can just be derived by applying the step.)
  * Note that this obviates the "find extrema" step used by astro.com, leaving the task to the DB.

To explore next:

* See: https://stackoverflow.com/a/32017013 for a SQLite-specific, efficient way of finding
  the longitude closest to a given one, given also the planet name and the transit time
  (this [other answer](https://stackoverflow.com/a/595691) hints at possibly using a composite index, too.)
* We essentially want a hybrid approach: maintain a database of precomputed ephemeris that's fast to search
  in, use queries to find good bracketing candidates for interpolation, and then use functions
  in `Transists.RootFinding` to find the exact moment of transit.
-}

import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow ()
import Database.SQLite.Simple.Internal (Field (Field))
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Import
import qualified RIO.Text as T
import SwissEphemeris
    ( calculateEclipticPosition,
      withEphemerides,
      Planet(..))
import Ephemeris.Types
import Ephemeris.Planet


data EclipticLongitudeEphemeris = EclipticLongitudeEphemeris
  { planet :: Planet,
    julianTime :: JulianTime,
    longitude :: Double,
    longitudinalSpeed :: Double
  }

toEphemeris :: Planet -> JulianTime -> EclipticPosition -> EclipticLongitudeEphemeris
toEphemeris p t (EclipticPosition lo _ _ ls _ _) =
  EclipticLongitudeEphemeris p t lo ls

deriving stock instance Read Planet

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

instance FromRow EclipticLongitudeEphemeris where
  fromRow = EclipticLongitudeEphemeris <$> field
      <*> field
      <*> field
      <*> field

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
-- takes about 48 seconds and occupies 11Mb on disk; 80730 rows.
-- I used https://ssd.jpl.nasa.gov/tc.cgi#top for easy string/julian
-- conversion. I run this on `ghci` with `:set +s` enabled. To
-- pre-populate for your own purposes, you'll have to export this 
-- function explicitly.
prepopulateEphemeris :: IO ()
prepopulateEphemeris =
  withEphemerides "./config" $
    populateEphemeris (JulianTime 2457023.5, JulianTime 2463232.5) 

{-
Further notes at: https://gist.github.com/lfborjas/ce12f992d64096b4b87b936a9868ae49
-}
