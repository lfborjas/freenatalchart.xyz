{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

module Transits.Persistence where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow ()
import Database.SQLite.Simple.Internal (Field (Field))
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Import
import qualified RIO.Text as T
import SwissEphemeris

data EclipticLongitudeEphemeris = EclipticLongitudeEphemeris
  { planet :: Planet,
    julianTime :: JulianTime,
    longitude :: Double,
    longitudinalSpeedSignum :: Int
  }

toEphemeris :: Planet -> JulianTime -> EclipticPosition -> EclipticLongitudeEphemeris
toEphemeris p t (EclipticPosition lo _ _ ls _ _) =
  EclipticLongitudeEphemeris p t lo speedSignum
  where
    -- we're only storing the sign of the speed, to know at first glance if it was retrogade
    -- or prograde in that moment. Still unsure if it'll be useful!
    speedSignum = truncate . signum $ ls
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
    "INSERT INTO ecliptic_longitude_ephemeris (planet, julian_time, longitude, lng_speed_signum) VALUES (?,?,?,?)"
    (planet, julianTime, longitude, longitudinalSpeedSignum)

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
Next:
expose queries to:
  * see if a given timeframe + longitude is even in the ephemeris
  * given a timeframe (365 days around the given date,) a planet and a longitude
    being sought, find the closest lower-bound longitude,e.g.

select max(longitude), julian_time, lng_speed_signum from ecliptic_longitude_ephemeris where planet = 'Sun' and julian_time between 2458849.5 and 2459215.5 and longitude <= 224.6882;

returns:

max(longitude)	julian_time	lng_speed_signum
224.02899728298763	2459159.5	1

Which aligns with what we already know: transiting sun _was_ about to cross a natal pluto with longitude of 224.68 on 11/7/2020, the timestamp found is one day before.

Since there might be multiple crossings, maybe a query like this would work:

select longitude, julian_time, lng_speed_signum from ecliptic_longitude_ephemeris where planet = 'Sun' and julian_time between 2458849.5 and 2459215.5 and longitude <= 224.6882 and longitude > 224.0;
(ossia select longitude, julian_time, lng_speed_signum from ecliptic_longitude_ephemeris where planet = 'Sun' and julian_time between 2458849.5 and 2459215.5 and longitude between 224.0 and 224.6882;)

which could return multiple entries, if there's more than one crossing in the timeframe!

here's another one:

select longitude, julian_time, lng_speed_signum from ecliptic_longitude_ephemeris where planet = 'Venus' and julian_time between 2458849.5 and 2459215.5 and longitude between 195.0 and 195.9234;

longitude	julian_time	lng_speed_signum
195.76251068357936	2459163.5	1

which agrees with astro.com that the crossing is likely to happen after midnight 11/10/2020 (in fact, it is exact at 3:09 am.)

for orbs, we could perhaps do a union? And in fact, we don't need to know _exactly_ when it enters orb, just approximately,
so for the enter/exit orb, we don't even need to interpolate: just say "it will be close to orb after date X and will exit after date Y."

Here's an interesting one, mars transiting natal sun this year:

select longitude, julian_time, lng_speed_signum from ecliptic_longitude_ephemeris where planet = 'Mars' and julian_time between 2458849.5 and 2459215.5 and longitude between 15.0 and 15.9234;

for a while there, it becomes retrograde, so there's 23 instances in which it is close to the natal sun! This means we probably want:

* To only ask for "likely to be exact" for a much tighter timeframe, perhaps a couple of days, or the same day? (in which case it becomes: will it happen today or not.)
* For "periods of activity", we probably want very tight orbs, and for "enter," to ask the first date it is close; for leave, the _last_ date.

For example, this seems to agree with astro.com
select min(julian_time) from ecliptic_longitude_ephemeris where planet = 'Mars' and julian_time between 2458849.5 and 2459215.5 and longitude between 15.0 and 15.9234;
returns: 2459056.5, or July 26, 2020

and
select max(julian_time) from ecliptic_longitude_ephemeris where planet = 'Mars' and julian_time between 2458849.5 and 2459215.5 and longitude between 15.0 and 15.9234;
returns 2459177.5, Nov 24, 2020

or, using 1 degree orbs around the exact degree:
select max(julian_time) from ecliptic_longitude_ephemeris where planet = 'Mars' and julian_time between 2458849.5 and 2459215.5 and longitude between 14.9234 and 16.9234;

returns similar results. Perhaps a union works even better:

select t from (
  select min(julian_time)as t from ecliptic_longitude_ephemeris where planet = 'Mars' and julian_time between 2458849.5 and 2459215.5 and longitude between 14.9234 and 16.9234
  union
  select max(julian_time)as t from ecliptic_longitude_ephemeris where planet = 'Mars' and julian_time between 2458849.5 and 2459215.5 and longitude between 14.9234 and 16.9234
);

gives us both boundaries:

t
2459055.5
2459183.5

So we can use the above for determining the "period of activity," with no further querying/interpolation.

To ask the _exact_ moment of crossing, we can just ask if it's happening between yesterday and tomorrow:

-- does the crossing happen between yesterday and tomorrow? (today being 11/9/2020 00:00 UTC, i.e. 2459161.5 Julian)
select julian_time, longitude from ecliptic_longitude_ephemeris where planet = 'Venus' and julian_time between 2459161.5 and 2459163.5 and longitude between 194.9234 and 196.9234;
which returns the closest moment that we're within one degree orb, before crossing past:
2459163.5 (nov 10, 2020)

at this moment, we can interpolate. Still unsure if the signum can help determine if we should reverse the order? Maybe the `Alternative` instance helps here? (i.e. just do both)

as such, I think we can:

* Use the `union` query for the period of activity, and we're done; limit it to 6 months before and 6 months after the queried date. Only applicable for already proven
  transits that are happening (i.e. find aspects/houses/sign containment.)
* Use the "within three days" query to see if the crossing may be happening soon (see if the returned date is in the natal timezone's definition of "today")
* If yes to the above, interpolate to find exactness.

I say may because take this example: Mars is "in orb", but the crossing isn't happening in the three day interval given, when interpolated:
select julian_time, longitude, lng_speed_signum from ecliptic_longitude_ephemeris where planet = 'Mars' and julian_time between 2459161.5 and 2459163.5 and longitude between 14.9234 and 16.9234;

In fact, to disambiguate, I think this query is better for an interpolation candidate:
select max(longitude), julian_time, lng_speed_signum from ecliptic_longitude_ephemeris where planet = 'Mars' and julian_time between 2459161.5 and 2459163.5 and longitude between 14.9234 and 16.9234;

which returns the time at which it is closest:
max(longitude)	julian_time	lng_speed_signum
15.47755306436939	2459161.5	-1

and can be used thusly:

> findExactTransitAround Mars (Longitude 15.9234) (JulianTime 2459163.5)
=> OutsideBounds

References:
https://ssd.jpl.nasa.gov/tc.cgi#top
astro.com daily horoscope for november 9th.
https://stackoverflow.com/a/32017013

-}
