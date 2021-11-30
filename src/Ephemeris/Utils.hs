{-# LANGUAGE RecordWildCards #-}
module Ephemeris.Utils where

import Ephemeris.Types
import RIO.Time (fromGregorian, picosecondsToDiffTime, diffTimeToPicoseconds, toGregorian, UTCTime(..))
import SwissEphemeris(defaultSplitDegreesOptions)
import qualified SwissEphemeris as SWE

mkEcliptic :: EclipticPosition
mkEcliptic = EclipticPosition 0 0 0 0 0 0

isRetrograde :: PlanetPosition -> Bool
isRetrograde PlanetPosition {..} =
  case planetName of
    -- the nodes are never "retrograde"
    MeanNode -> False
    TrueNode -> False
    _ -> planetLngSpeed < 0.0

picosecondsInHour :: Double
picosecondsInHour = 3600 * 1e12

splitDegrees :: Double -> LongitudeComponents
splitDegrees = SWE.splitDegrees $ defaultSplitDegreesOptions <> [RoundSeconds]

splitDegreesZodiac :: Double -> LongitudeComponents
splitDegreesZodiac = SWE.splitDegreesZodiac
