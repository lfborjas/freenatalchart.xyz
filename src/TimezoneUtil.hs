{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module TimezoneUtil where

import Import
import RIO.Time
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import TimezoneDetect

-- | Gets timezone info from the default Unix location for tzdata
-- we assume /usr/share, but it could also be in /etc:
-- https://hackage.haskell.org/package/timezone-series-0.1.9/docs/Data-Time-LocalTime-TimeZone-Series.html#g:3
getTimeZoneSeries :: TimezoneName -> IO TimeZoneSeries
getTimeZoneSeries tzName =
    getTimeZoneSeriesFromOlsonFile ("/usr/share/zoneinfo/" <> tzName)

-- | Converts a local time string to UTC, given a timezone
-- e.g.
-- *Main TimezoneUtil> localTimeStringToUTC "America/New_York" "2019-12-25 00:30:00"
-- 2019-12-25 05:30:00 UTC
-- *Main TimezoneUtil> localTimeStringToUTC "America/New_York" "2019-09-25 00:30:00"
-- 2019-09-25 04:30:00 UTC
-- *Main TimezoneUtil> localTimeStringToUTC "America/Tegucigalpa" "2019-09-25 00:30:00"
-- 2019-09-25 06:30:00 UTC
-- *Main TimezoneUtil> localTimeStringToUTC "America/Tegucigalpa" "2019-12-25 00:30:00"
-- 2019-12-25 06:30:00 UTC
localTimeStringToUTC :: TimezoneName -> String -> IO UTCTime
localTimeStringToUTC tz localTimeStr = do
    series <- getTimeZoneSeries tz
    localTime <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" localTimeStr
    return $ localTimeToUTC' series localTime

toUTC :: TimezoneName -> LocalTime -> IO UTCTime
toUTC tz localTime = do
    series <- getTimeZoneSeries tz
    return $ localTimeToUTC' series localTime

-- | Given a latitude, longitude and reference time, return the UTC time
-- at that point in space and time:
--
-- >>> localTime <- (parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "2019-12-25 00:30:00" :: IO LocalTime)
-- >>> timeAtPointToUTC 40.7831 (-73.9712) localTime
-- Right 2019-12-25 05:30:00 UTC
-- >>> localTimeSummer <- (parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "2019-08-25 00:30:00" :: IO LocalTime)
-- >>> timeAtPointToUTC 40.7831 (-73.9712) localTimeSummer 
-- Right 2019-08-25 04:30:00 UTC
-- TODO: wrap in the RIO monad with an `env` that points to the tz db?
timeAtPointToUTC :: Double -> Double -> LocalTime -> IO (Either String UTCTime)
timeAtPointToUTC lat lng referenceTime = do
    let tzName = lookupTimezone "./config/timezone21.bin" (realToFrac lat) (realToFrac lng)
    case tzName of
        Left e -> pure $ Left e
        Right tz -> do
            t <- toUTC tz referenceTime
            pure $ Right t
