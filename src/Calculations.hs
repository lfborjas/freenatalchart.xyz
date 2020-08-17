{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Calculations where

import Import
import SwissEphemeris
import RIO.List (cycle)
import RIO.List.Partial ((!!))

angularDifference :: Longitude -> Longitude -> Longitude
angularDifference a b | (b - a) < 1 = (b + 360 - a)
                      | otherwise = b - a

-- from: https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n  xs = zipWith const (drop n (cycle xs)) xs

ascendant :: [House] -> House
ascendant h =  h !! 0

mc :: [House] -> House
mc h = h !! 9

-- TODO(luis): this feels silly but orderly, maybe SwissEphemeris
-- should just return an array? A house's number is important though,
-- so some manner of silly unpacking and repacking would happen anyway?
houses :: CuspsCalculation -> [House]
houses (CuspsCalculation HouseCusps{..} _) =
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

-- TODO: change [House] to NonEmpty House
celestialAspects :: [PlanetPosition] -> [House] -> [HoroscopeAspect PlanetPosition House]
celestialAspects ps hs = aspects ps [Calculations.ascendant hs, Calculations.mc hs]
