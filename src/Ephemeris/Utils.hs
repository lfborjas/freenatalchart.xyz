{-# LANGUAGE RecordWildCards #-}
module Ephemeris.Utils where

import Ephemeris.Types
import RIO.Time (fromGregorian, picosecondsToDiffTime, diffTimeToPicoseconds, toGregorian, UTCTime(..))
import SwissEphemeris(julianDay, defaultSplitDegreesOptions, gregorianDateTime)
import qualified SwissEphemeris as SWE

mkEcliptic :: EclipticPosition
mkEcliptic = EclipticPosition 0 0 0 0 0 0

angularDifference :: Double -> Double -> Double
angularDifference a b
  | (b - a) < 1 = (b + 360 - a)
  | otherwise = b - a

isRetrograde :: PlanetPosition -> Bool
isRetrograde PlanetPosition {..} =
  case planetName of
    -- the nodes are never "retrograde"
    MeanNode -> False
    TrueNode -> False
    _ -> planetLngSpeed < 0.0

picosecondsInHour :: Double
picosecondsInHour = 3600 * 1e12

-- | Convert between a UTC timestamp and the low-level JulianTime that SwissEphemeris requires.
utcToJulian :: UTCTime -> JulianTime
utcToJulian (UTCTime day time) =
  julianDay (fromIntegral y) m d h
  where
    (y, m, d) = toGregorian day
    h = (1/picosecondsInHour) * (fromIntegral $ diffTimeToPicoseconds time)

julianToUTC :: JulianTime -> UTCTime
julianToUTC jd = 
  UTCTime day dt
  where
    (y,m,d,h) = gregorianDateTime jd
    day = fromGregorian (fromIntegral y) m d
    dt = picosecondsToDiffTime $ round $ h * picosecondsInHour

-- 2459160.1572215613

splitDegrees :: Double -> LongitudeComponents
splitDegrees = SWE.splitDegrees $ defaultSplitDegreesOptions <> [RoundSeconds]

splitDegreesZodiac :: Double -> LongitudeComponents
splitDegreesZodiac = SWE.splitDegreesZodiac
