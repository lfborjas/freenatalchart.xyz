{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE RecordWildCards #-}
module Chart.Calculations where

import Data.Time.LocalTime.TimeZone.Detect
import Import hiding (Earth)
import RIO.List (cycle, headMaybe, lastMaybe, sortBy)
import RIO.Partial (toEnum)
import RIO.Time (UTCTime (..), diffTimeToPicoseconds, toGregorian)
import SwissEphemeris

-- "main" fn

horoscope :: TimeZoneDatabase -> EphemeridesPath -> BirthData -> IO HoroscopeData
horoscope timezoneDB ephePath BirthData {..} = do
  latitude <- pure $ birthLocation & locationLatitude & unLatitude
  longitude <- pure $ birthLocation & locationLongitude & unLongitude
  -- convert to what the underlying library expects: a UTC time, and a pair of raw coordinates.
  uTime <- timeAtPointToUTC timezoneDB latitude longitude birthLocalTime
  time <- pure $ utcToJulian uTime
  place <- pure $ mkCoordinates {lat = latitude, lng = longitude}

  withEphemerides ephePath $ do
    positionsM <- forM defaultPlanets $ \p -> do
      coords <- calculateCoordinates time p
      -- TODO: actually calculate
      declination <- pure $ 0.0
      case coords of
        Left _ -> pure Nothing
        Right c -> pure $ Just $ PlanetPosition p (Latitude . lat $ c) (Longitude . lng $ c) (lngSpeed c) declination

    -- TODO: calculate ecliptic obliquity, too.
    (CuspsCalculation cusps angles' sys) <- calculateCusps Placidus time place
    -- TODO: calculate house positions here, with swe_house_pos.
    -- TODO: might as well also calculate the "split degrees" with swisseph.
    let positions = catMaybes positionsM
    return $
      HoroscopeData
        positions
        angles'
        (houses cusps)
        sys
        (planetaryAspects positions)
        (celestialAspects positions angles')
        uTime
        time


-- PURE FNs

angularDifference :: Double -> Double -> Double
angularDifference a b
  | (b - a) < 1 = (b + 360 - a)
  | otherwise = b - a

-- from: https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = zipWith const (drop n (cycle xs)) xs

-- TODO(luis): this feels silly but orderly, maybe SwissEphemeris
-- should just return an array? A house's number is important though,
-- so some manner of silly unpacking and repacking would happen anyway?
houses :: [HouseCusp] -> [House]
houses cusps =
  zip
    [ House I,
      House II,
      House III,
      House IV,
      House V,
      House VI,
      House VII,
      House VIII,
      House IX,
      House X,
      House XI,
      House XII
    ]
    cusps
    & map (\(ctr, cusp) -> ctr $ Longitude cusp)

isRetrograde :: PlanetPosition -> Bool
isRetrograde PlanetPosition {..} =
  case planetName of
    -- the nodes are never "retrograde"
    MeanNode -> False
    TrueNode -> False
    _ -> planetLngSpeed < 0.0

mkTime :: Int -> Int -> Int -> Double -> JulianTime
mkTime = julianDay

-- | Convert between a UTC timestamp and the low-level JulianTime that SwissEphemeris requires.
utcToJulian :: UTCTime -> JulianTime
utcToJulian (UTCTime day time) =
  julianDay (fromIntegral $ y) m d h
  where
    (y, m, d) = toGregorian day
    h = 2.77778e-16 * (fromIntegral $ diffTimeToPicoseconds time)

aspects' :: (HasLongitude a, HasLongitude b) => [Aspect] -> [a] -> [b] -> [HoroscopeAspect a b]
aspects' possibleAspects bodiesA bodiesB =
  (concatMap aspectsBetween pairs) & catMaybes
  where
    pairs = [(x, y) | x <- bodiesA, y <- bodiesB]
    aspectsBetween bodyPair = map (haveAspect bodyPair) possibleAspects
    haveAspect (a, b) asp@Aspect {..} =
      let angleBetween = angularDifference (getLongitudeRaw a) (getLongitudeRaw b)
          orbBetween = (angle - (abs angleBetween)) & abs
       in if orbBetween <= maxOrb
            then Just $ HoroscopeAspect {aspect = asp, bodies = (a, b), aspectAngle = angleBetween, orb = orbBetween}
            else Nothing

aspects :: (HasLongitude a, HasLongitude b) => [a] -> [b] -> [HoroscopeAspect a b]
aspects = aspects' defaultAspects

planetaryAspects :: [PlanetPosition] -> [HoroscopeAspect PlanetPosition PlanetPosition]
planetaryAspects ps = aspects ps $ rotateList 1 ps

celestialAspects :: [PlanetPosition] -> Angles -> [HoroscopeAspect PlanetPosition House]
celestialAspects ps Angles {..} = aspects ps [House I (Longitude ascendant), House X (Longitude mc)]

-- | Get the house a given celestial body is "in". Note that it will
-- /only/ "promote" to the next house if the body is exactly on the cusp.
-- NOTE/TODO(luis) this is a naïve implementation, the more correct way to do it, to
-- account for latitudes, is to use the SwissEphemeris `swe_house_pos` function
-- <https://www.astro.com/swisseph/swephprg.htm#_Toc49847893 15. House position of a planet>
housePosition :: HasLongitude a => [House] -> a -> Maybe House
housePosition houses' body =
  span (\h -> (getLongitude h) <= (getLongitude body)) sortedHouses
    & fst
    & lastMaybe
  where
    -- & fmap houseNumber

    sortedHouses = sortBy (\a b -> compare (getLongitude a) (getLongitude b)) houses'
