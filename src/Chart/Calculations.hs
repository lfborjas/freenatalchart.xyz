{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Chart.Calculations
  ( horoscope,
    rotateList,
    angularDifference,
    isRetrograde,
    mkTime,
    housePosition,
    splitDegrees,
    splitDegreesZodiac,
    findAspectBetweenPlanets,
    findAspectWithAngle,
    findSunSign,
    findAscendant,
    planetsByHouse,
    planetsInHouse
  )
where

import Data.Time.LocalTime.TimeZone.Detect
import Import hiding (Earth)
import RIO.List (cycle, headMaybe, lastMaybe, sortBy)
import RIO.Time (UTCTime (..), diffTimeToPicoseconds, toGregorian)
import SwissEphemeris hiding (houseNumber, splitDegrees, splitDegreesZodiac)
import qualified SwissEphemeris as SWE

-- "main" fn

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

-- PURE FNs

angularDifference :: Double -> Double -> Double
angularDifference a b
  | (b - a) < 1 = (b + 360 - a)
  | otherwise = b - a

-- from: https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = zipWith const (drop n (cycle xs)) xs

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
celestialAspects ps Angles {..} = aspects ps [House I (Longitude ascendant) 0, House X (Longitude mc) 0]

-- | Get the house a given celestial body is "in". Note that it will
-- /only/ "promote" to the next house if the body is exactly on the cusp.
-- it is a re-interpretation of this:
-- https://groups.io/g/swisseph/message/4052
housePosition :: HasLongitude a => [House] -> a -> Maybe House
housePosition houses' body =
  case split of
    ([], xs) -> lastMaybe xs
    (xs, _) -> lastMaybe xs
  where
    split = span (\h -> (getLongitude h) <= (getLongitude body)) sortedHouses
    sortedHouses = sortBy (\a b -> compare (getLongitude a) (getLongitude b)) houses'

planetsByHouse :: [House] -> [PlanetPosition] -> [(House, PlanetPosition)]
planetsByHouse houses' planets =
  map maybeHouse planets & catMaybes
  where
    pos = housePosition houses'
    maybeHouse = (\p -> maybe Nothing (\h -> Just (h, p)) (pos p))


planetsInHouse :: [(House,  PlanetPosition)] -> House -> [PlanetPosition]
planetsInHouse mapped haus = 
  filter (\(h,_) -> h == haus) mapped
  & map snd

findAspectBetweenPlanets :: [HoroscopeAspect PlanetPosition PlanetPosition] -> Planet -> Planet -> Maybe (HoroscopeAspect PlanetPosition PlanetPosition)
findAspectBetweenPlanets aspectList pa pb =
  aspectList
    & filter (\HoroscopeAspect {..} -> (planetName . fst $ bodies, planetName . snd $ bodies) `elem` [(pa, pb), (pb, pa)])
    & headMaybe

findAspectWithAngle :: [HoroscopeAspect PlanetPosition House] -> Planet -> HouseNumber -> Maybe (HoroscopeAspect PlanetPosition House)
findAspectWithAngle aspectList pa hb =
  aspectList
    & filter (\HoroscopeAspect {..} -> (planetName . fst $ bodies, houseNumber . snd $ bodies) == (pa, hb))
    & headMaybe

findSunSign :: [PlanetPosition] -> Maybe ZodiacSignName
findSunSign positions =
  positions
    & dropWhile (\PlanetPosition {..} -> planetName /= Sun)
    & headMaybe
    & fmap (longitudeZodiacSign . splitDegreesZodiac . getLongitudeRaw . planetLng)
    & maybe Nothing id

findAscendant :: [House] -> Maybe ZodiacSignName
findAscendant houses' =
  houses'
    & dropWhile (\House {..} -> houseNumber /= I)
    & headMaybe
    & fmap (longitudeZodiacSign . splitDegreesZodiac . getLongitudeRaw . houseCusp)
    & maybe Nothing id

-- simplifications of SWE helpers

splitDegrees :: Double -> LongitudeComponents
splitDegrees = SWE.splitDegrees $ defaultSplitDegreesOptions <> [RoundSeconds]

splitDegreesZodiac :: Double -> LongitudeComponents
splitDegreesZodiac = SWE.splitDegreesZodiac
