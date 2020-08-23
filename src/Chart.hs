{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Chart where

import Import hiding ((^.), local, over)
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Vector (e)
import Diagrams.Core.Types (keyVal)
import Calculations (isRetrograde, mkCoordinates, mkTime, horoscope, angularDifference, rotateList)
import Control.Category ((<<<))
import Prerendered as P
import SwissEphemeris (Planet(..), Angles(..), closeEphemerides, setEphemeridesPath)
import RIO.List (groupBy, sortBy)

zodiacCircle :: ChartContext -> Diagram B
zodiacCircle env = 
    mconcat $ map zodiacBand westernZodiacSigns
    where
        zodiacBand (ZodiacSign signName zLng zElement) = 
            g <> w # fc signColor
                   # lw thin
                   # (href $ "#zodiac-" <> (show signName))
                   -- can set `title`, `id` or `class`:
                   -- https://hackage.haskell.org/package/diagrams-svg-1.4.3/docs/src/Diagrams.Backend.SVG.html
                   # (keyVal $ ("title", show signName))
            where
                onZodiacs = env ^. zodiacCircleRadiusL
                d :: Direction V2 Longitude
                d = rotateBy ((zLng @@ deg) ^. turn) xDir
                a :: Angle Double
                a = 30 @@ deg
                w = annularWedge 1 (onZodiacs) d a
                glyphPosition = longitudeToPoint (onZodiacs + 0.1) (zLng + 15)
                g = (stroke $ P.prerenderedSign signName)
                    # scale 0.15
                    # moveTo glyphPosition
                    # rectifyAround glyphPosition env
                    # fc black
                    # lw thin
                signColor =
                    case zElement of
                        Import.Earth -> darkgreen
                        Air -> yellow
                        Fire -> red
                        Water -> lightblue



cuspsCircle :: ChartContext -> [House] -> Diagram B
cuspsCircle env c = 
    mconcat $ map cuspBand pairedC
    where
        onZodiacs = env ^. zodiacCircleRadiusL
        onAspects = env ^. aspectCircleRadiusL
        pairedC = zip c $ rotateList 1 c
        cuspBand (House houseName cuspBegin, House _ cuspEnd) =
            t <> w # lw thin
                   # lc gray
                   # (href $ "#house-" <> (show houseName))
            where
                d = rotateBy ((cuspBegin @@ deg) ^. turn) xDir
                a = (angularDifference cuspBegin cuspEnd) @@ deg
                w = annularWedge (onZodiacs) (onAspects) d a
                textPosition :: Point V2 Double
                textPosition = longitudeToPoint (onAspects + 0.05) (cuspBegin + 5)
                t = (text $ houseLabel houseName) 
                    # moveTo textPosition
                    # fontSize (local 0.05)
                    # fc gray
                    # rectifyAround textPosition env

quadrants :: ChartContext -> Angles-> Diagram B
quadrants env Angles{..} = 
    mconcat $ map quadrant angles
    where
        angles = [(ascendant, "Asc"), (mc, "MC")]
        quadrant (cuspBegin, label) =
            t
            where
                onZodiacs = env ^. zodiacCircleRadiusL 
                textPosition = longitudeToPoint (onZodiacs - 0.05) (cuspBegin + 4)
                t = (text $ label) 
                    # moveTo textPosition
                    # fontSize (local 0.05)
                    # fc black
                    # rectifyAround textPosition env

aspects :: ChartContext -> [HoroscopeAspect PlanetPosition PlanetPosition] -> Diagram B
aspects env pAspects = do
    mconcat $ map aspectLine pAspects
    where
        aspectLine HoroscopeAspect{..} =
            aPos ~~ bPos # lc aspectColor
                         # lw thin
            where
                onAspect = env ^. aspectCircleRadiusL
                (aPos, bPos) = (over each) (longitudeToPoint onAspect <<< getLongitude) bodies
                aspectColor = 
                    case (temperament aspect) of
                        Analytical -> red
                        Synthetic -> blue
                        Neutral -> green

planets :: ChartContext -> [PlanetPosition] -> Diagram B
planets env planetPositions =
    mconcat $ map drawPlanet (correctCollisions 3 planetPositions)
    where
        onAspects = env ^. aspectCircleRadiusL
        onZodiacs = env ^. zodiacCircleRadiusL
        onPlanets = env ^. planetCircleRadiusL
        drawPlanet (corrected, pos@PlanetPosition{..}) =
            (stroke $ P.prerenderedPlanet planetName)
            # scale 0.1
            # moveTo correctedPosition
            # rectifyAround correctedPosition env
            # fc black
            # lw ultraThin
            # (keyVal $ ("title", planetLabel planetName))
            # (href $ "#" <> (planetLabel planetName))
            <> guideLines
            <> (correctionLine # lw thin # lc darkgray)
            <> retrogradeMark
            where
                -- TODO: maybe `planetLabel` should be promoted more, so tables
                -- can also refer to `MeanApog` as `Lilith`?
                planetLabel MeanApog = "Lilith"
                planetLabel p = show p
                drawPlanetAt = maybe (getLongitude pos) id corrected 
                atCorrectedPosition  = flip longitudeToPoint $ drawPlanetAt
                correctedPosition = atCorrectedPosition onPlanets
                atEclipticPosition = flip longitudeToPoint $ getLongitude pos
                eclipticPosition = atEclipticPosition onPlanets
                aspectCircleLine = atEclipticPosition onAspects ~~ atEclipticPosition (onAspects + 0.03)
                zodiacCircleLine = atEclipticPosition onZodiacs ~~ atEclipticPosition (onZodiacs - 0.03)
                guideLines = (aspectCircleLine <> zodiacCircleLine) # lw thin
                correctionLine = 
                    case corrected of
                        Nothing -> mempty
                        Just _  -> (atCorrectedPosition (onPlanets + 0.02)) ~~ (atEclipticPosition (onZodiacs - 0.035))
                retrogradeMark =
                    if (isRetrograde pos) then
                        text "r"
                        # moveTo (atCorrectedPosition (onPlanets - 0.055))
                        # rectifyAround (atCorrectedPosition (onPlanets - 0.03)) env
                        # fontSize (local 0.05)
                    else
                        mempty


--chart :: (Semigroup m, TrailLike (QDiagram b V2 Longitude m)) => [House] -> QDiagram b V2 Longitude m
chart :: ChartContext -> HoroscopeData -> Diagram B
chart env HoroscopeData{..}  = do
    (zodiacCircle env)
    <> (planets env horoscopePlanetPositions )
    <> (cuspsCircle env horoscopeHouses)
    <> (quadrants env horoscopeAngles)
    <> (aspects env horoscopePlanetaryAspects)
    -- TODO: horoscopeCelestialAspects?

--
-- CHART UTILS
-- 

rectifyAround :: Point V2 Double -> ChartContext -> Diagram B -> Diagram B
rectifyAround point env =
    rotateAround point (negated (offset_ @@ deg))
    where
        offset_ = env ^. ascendantOffsetL

houseLabel :: HouseNumber -> String
houseLabel = fromEnum >>> (+1) >>> show

-- | Given a longitude and a magnitude (distance from origin)
-- return a point sitting at the equivalent vector
-- more on vectors:
-- https://archives.haskell.org/projects.haskell.org/diagrams/doc/vector.html#vector-operations
longitudeToPoint :: Double -> Longitude -> Point V2 Double
longitudeToPoint magnitude longitude = 
    origin .+^ v
    where
        theta = longitude @@ deg
        v = magnitude *^ e theta

-- TODO(luis) if the tolerance is too high (about 5 degrees,) these "corrections" may create further
-- collisions! We could do several passes before giving up... or come up with a fancier algorithm.
-- this is okay for my personal horoscopy, happy to revisit!
correctCollisions :: HasLongitude a => Double -> [a] -> [(Maybe Longitude, a)]
correctCollisions tolerance originalPositions =
    sorted
    & groupBy proximity
    & concatMap correctCollisions'
    & (flip zip) sorted
    & map (\(corrected, original) -> if corrected == (getLongitude original) then (Nothing, original) else (Just corrected, original))
    where
        correction = tolerance / 2 
        sorted = sortBy (\a b -> compare (getLongitude a) (getLongitude b)) originalPositions
        proximity x y = abs (getLongitude y - getLongitude x) <= tolerance
        -- TODO: this is naïve: it's only somewhat elegant for pairs,
        -- and for bigger clusters it just pushes them away... but it works?
        -- also, what happens when we end up negative angles?
        -- and, what if a correction pushes a planet past another one and the guidelines are now
        -- confusing (this happens when I put the tolerance all the way up to 5 degrees in my test data!)
        -- this is a more fastidious approach:
        -- https://github.com/Kibo/AstroChart/blob/8615cccdc7dbec52b8ba3bdf5f28c0ad8b09691f/project/src/utils.js
        correctCollisions' :: HasLongitude b => [b] -> [Longitude]
        correctCollisions' [] = []
        correctCollisions' [x,y] = [(getLongitude x) - correction, (getLongitude y) + correction]
        correctCollisions' (x:xs) = (getLongitude x) : map (\y -> (getLongitude y) + correction) xs 
        

-- TODO: probably need a reader monad context here (for pre-rendered things.)
-- also, need to return the diagram, not render to a file.
renderChart :: HoroscopeData -> IO ()
renderChart z@HoroscopeData{..}= do
  let env = ChartContext{
    chartAscendantOffset = ascendantOffset,
    chartZodiacCircleRadius = 0.8,
    chartAspectCircleRadius = 0.5,
    chartPlanetCircleRadius = 0.65
  }
  renderSVG 
    "circle.svg"
    (mkWidth 400)
    (chart env z # rotateBy ((ascendantOffset @@ deg) ^. turn))
    where
        ascendantOffset = 180 - (ascendant horoscopeAngles)

renderTestChart :: IO ()
renderTestChart = do
    -- TODO: bring in the `directory` package?
    setEphemeridesPath "/Users/luis/code/lfborjas/cassiel/config"
    --let calculations = horoscope 2447532.771485 (mkCoordinates 14.0839053 (-87.2750137))
    let calculations = horoscope (mkTime 1989 1 6 0.0) (mkCoordinates 14.0839053 (-87.2750137))
    --let calculations = horoscope 2447885.896491 (mkCoordinates 40.7831 (-73.9712))
    renderChart calculations
    closeEphemerides
