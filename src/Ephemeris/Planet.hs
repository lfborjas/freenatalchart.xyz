module Ephemeris.Planet where

import Ephemeris.Types
    ( Planet(..) )

defaultPlanets :: [Planet]
defaultPlanets = [Sun .. Pluto] <> [MeanNode, MeanApog, Chiron]
