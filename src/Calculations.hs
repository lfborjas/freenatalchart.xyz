{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Calculations where

import Import
import SwissEphemeris
import RIO.List (cycle)

angularDifference :: Longitude -> Longitude -> Longitude
angularDifference a b | (b - a) < 1 = (b + 360 - a)
                      | otherwise = b - a

-- from: https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n  xs = zipWith const (drop n (cycle xs)) xs

-- TODO(luis): this feels silly but orderly, maybe SwissEphemeris
-- should just return an array? A house's number is important though,
-- so some manner of silly unpacking and repacking would happen anyway?
houses :: HouseCusps -> [House]
houses HouseCusps{..} =
    [ House I i
    , House II ii
    , House III iii
    , House IV iv
    , House V v
    , House VI vi
    , House VII vii
    , House VIII viii
    , House IX ix
    , House X x
    , House XI xi
    , House XII xii
    ]

planetPositions :: [(Planet, Either String Coordinates)] -> [PlanetPosition]
planetPositions ps =
    map (\(p, c) -> if p /= SwissEphemeris.Earth then (PlanetPosition p) <$> c else Left "earth") ps & rights

mkCoordinates :: Double -> Double -> Coordinates
mkCoordinates lat' lng' = defaultCoordinates{lat = lat', lng = lng'}

mkTime :: Int -> Int -> Int -> Double -> JulianTime
mkTime = julianDay

horoscope :: JulianTime -> Coordinates -> HoroscopeData
horoscope time place =
    HoroscopeData positions
                  angles
                  housesCalculated
                  Placidus
                  (planetaryAspects positions)
                  (celestialAspects positions angles)
    where
        positions = map (\p -> (p, calculateCoordinates time p)) [Sun .. Chiron] & planetPositions
        CuspsCalculation h angles = calculateCusps time place Placidus
        housesCalculated = houses $ h

aspects' :: (HasLongitude a, HasLongitude b) => [Aspect] -> [a] -> [b] -> [HoroscopeAspect a b]
aspects' possibleAspects bodiesA bodiesB =
    (concatMap aspectsBetween pairs) & catMaybes
    where
        pairs = [(x,y) | x <- bodiesA, y <- bodiesB]
        aspectsBetween bodyPair = map (haveAspect bodyPair) possibleAspects
        haveAspect (a,b) asp@Aspect{..} =
            let
                angleBetween = angularDifference (getLongitude a) (getLongitude b)
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
celestialAspects ps Angles{..} = aspects ps [House I ascendant, House X mc]
