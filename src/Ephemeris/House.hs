{-# LANGUAGE NoImplicitPrelude#-}

module Ephemeris.House where

import Import
import Ephemeris.Types
    ( HasLongitude(getLongitude), House, PlanetPosition )
import RIO.List (sortBy, lastMaybe)

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
