{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Chart.Calculations where

import Import hiding (Earth)
import SwissEphemeris
import Data.Time.LocalTime.TimeZone.Detect
import RIO.List (lastMaybe, sortBy, headMaybe, cycle)
import RIO.Time (diffTimeToPicoseconds, toGregorian, UTCTime(..))
import RIO.Partial (toEnum)


-- "main" fn

horoscope :: TimeZoneDatabase -> EphemeridesPath -> BirthData -> IO HoroscopeData
horoscope timezoneDB ephePath BirthData{..} = do
    latitude   <- pure $ birthLocation & locationLatitude & unLatitude
    longitude  <- pure $ birthLocation & locationLongitude & unLongitude
    -- convert to what the underlying library expects: a UTC time, and a pair of raw coordinates.
    uTime      <- timeAtPointToUTC timezoneDB latitude longitude birthLocalTime
    time       <- pure $ utcToJulian uTime
    place      <- pure $ mkCoordinates{lat=latitude, lng=longitude}

    withEphemerides ephePath $ do
        positionsM <- forM [Sun .. Chiron] $ \p -> do
            coords <- calculateCoordinates time p
            pure $ (p, coords)

        (CuspsCalculation cusps angles' sys) <- calculateCusps Placidus time place
        
        let positions = planetPositions positionsM
        return $ HoroscopeData positions
                               angles'
                               (houses cusps)
                               sys
                               (planetaryAspects positions)
                               (celestialAspects positions angles')
                               uTime
                               time

-- PURE FNs

angularDifference :: Double -> Double -> Double
angularDifference a b | (b - a) < 1 = (b + 360 - a)
                      | otherwise = b - a

-- from: https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n  xs = zipWith const (drop n (cycle xs)) xs

-- TODO(luis): this feels silly but orderly, maybe SwissEphemeris
-- should just return an array? A house's number is important though,
-- so some manner of silly unpacking and repacking would happen anyway?
houses :: [HouseCusp] -> [House]
houses cusps =
    zip 
        [ House I
        , House II
        , House III
        , House IV
        , House V
        , House VI
        , House VII
        , House VIII
        , House IX
        , House X
        , House XI
        , House XII
        ]
        cusps
    & map (\(ctr, cusp) -> ctr $ Longitude cusp)

isRetrograde :: PlanetPosition -> Bool
isRetrograde PlanetPosition{..} = 
    case planetName of
        -- the nodes are never "retrograde"
        MeanNode -> False
        TrueNode -> False
        _ -> (lngSpeed planetCoordinates) < 0

-- | Given a celestial body and (maybe) its coordinates, apply some business logic
-- and construct a richer `PlanetPosition`.
-- The business logic:
-- * If coordinates couldn't be calculated, the planet is ignored (can happen for Chiron
--   if not using ephemeris that have data for it.)
-- * If it's one of the "ignored" bodies (Earth, True Node, True Lilith,) we also ignore it.
planetPositions :: [(Planet, Either String Coordinates)] -> [PlanetPosition]
planetPositions ps =
    map positionBuilder ps & rights
    where
        positionBuilder (p, c) =
            case p of
                Earth -> Left "Earth: not displayed by default."
                OscuApog -> Left "Osculating Apogee (true Lilith) not displayed by default."
                TrueNode -> Left "True node not displayed by default, using Mean Node"
                _ -> PlanetPosition p <$> c


mkTime :: Int -> Int -> Int -> Double -> JulianTime
mkTime = julianDay

-- | Convert between a UTC timestamp and the low-level JulianTime that SwissEphemeris requires.
utcToJulian :: UTCTime -> JulianTime
utcToJulian (UTCTime day time) = 
    julianDay (fromIntegral $ y) m d h
    where
        (y, m, d) = toGregorian day
        h         = 2.77778e-16 * (fromIntegral $ diffTimeToPicoseconds time)
    -- T (uses the Julian Time calculated by astro.com)
    -- to get this julian from our own library, e.g.
    -- >>  timeAtPointToUTC' = timeAtPointToUTC "./config/timezone21.bin"
    -- >>> localTime <- (parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "1989-12-25 04:30:00" :: IO LocalTime)
    -- >>> TsUTC <- timeAtPointToUTC' 40.7831 (-73.9712) localTime
    -- >>> Calculations.utcToJulian tsUTC
    -- 2447885.89583365
    --let calculations = horoscope 2447885.896491 (mkCoordinates 40.7831 (-73.9712))
    -- you'll note that our number and astro.com's number differ... but it seems like
    -- ours is actually correct?
    -- e.g. inputting T's date on these sites:
    -- http://www.csgnetwork.com/juliandatetodaycalc.html
    -- https://ssd.jpl.nasa.gov/tc.cgi#top
    -- yields the 1989-12-15 9:30:00 for our number, but 9:30:**57** for astro.com's??
    -- I'm sure they have a good reason, but it's cool we didn't mess up too much!
    -- Home
    --let calculations = horoscope (mkTime 2020 8 23 0.0) (mkCoordinates 40.7282 (-73.7949))

aspects' :: (HasLongitude a, HasLongitude b) => [Aspect] -> [a] -> [b] -> [HoroscopeAspect a b]
aspects' possibleAspects bodiesA bodiesB =
    (concatMap aspectsBetween pairs) & catMaybes
    where
        pairs = [(x,y) | x <- bodiesA, y <- bodiesB]
        aspectsBetween bodyPair = map (haveAspect bodyPair) possibleAspects
        haveAspect (a,b) asp@Aspect{..} =
            let
                angleBetween = angularDifference (getLongitudeRaw a) (getLongitudeRaw b)
                orbBetween = (angle - (abs angleBetween)) & abs
            in
            if orbBetween <= maxOrb then
                Just $ HoroscopeAspect { aspect = asp, bodies = (a, b), aspectAngle = angleBetween, orb = orbBetween}
            else
                Nothing

aspects :: (HasLongitude a, HasLongitude b) => [a] -> [b] -> [HoroscopeAspect a b]
aspects = aspects' defaultAspects

planetaryAspects :: [PlanetPosition] -> [HoroscopeAspect PlanetPosition PlanetPosition]
planetaryAspects ps = aspects ps $ rotateList 1 ps

celestialAspects :: [PlanetPosition] -> Angles -> [HoroscopeAspect PlanetPosition House]
celestialAspects ps Angles{..} = aspects ps [House I (Longitude ascendant), House X (Longitude mc)]

-- NEXT:
-- longitudeToParts :: Longitude -> ZodiacalLongitude
-- https://github.com/lfborjas/senex/blob/master/frontend/src/Main.elm#L1687
-- housePos :: HasLongitude a => [House] -> a

-- | Given an entity with a longitude, find its position in the zodiac in
-- sign, degrees, minutes, seconds (and fraction)
-- based on swiss ephemeris `swe_split_deg`:
-- https://github.com/lfborjas/swiss-ephemeris/blob/c31d31286e708537e89b857dc7f607ea8eb3b48d/csrc/swephlib.c#L4013
zodiacPosition :: HasLongitude a => a -> ZodiacPosition
zodiacPosition a = 
    let longitude = getLongitudeRaw a
        truncated :: Int
        truncated = truncate longitude
        zodiacNum = truncated `div` 30
        sign :: ZodiacSignName
        sign = if zodiacNum >= 12 then Aries else toEnum zodiacNum
        -- the actual components
        degrees = truncated `rem` 30 & fromIntegral 
        minutes = truncate $ (longitude - (fromIntegral truncated)) * 60.0
        seconds = round $ (longitude - (fromIntegral truncated) - ((fromIntegral minutes) / 60)) * 3600
    in
        ZodiacPosition sign degrees minutes seconds

-- | Get the house a given celestial body is "in". Note that it will
-- /only/ "promote" to the next house if the body is exactly on the cusp.
housePosition :: HasLongitude a => [House] -> a -> Maybe House
housePosition houses' body =
    span (\h -> (getLongitude h) <= (getLongitude body)) sortedHouses
        & fst
        & lastMaybe
        -- & fmap houseNumber
    where
        sortedHouses = sortBy (\a b -> compare (getLongitude a) (getLongitude b)) houses'
