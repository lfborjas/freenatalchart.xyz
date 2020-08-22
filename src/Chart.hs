{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Chart where

import Import hiding ((^.), local, over)
import Diagrams.Prelude
import Diagrams.Backend.SVG
import RIO.List.Partial ((!!))
import Diagrams.TwoD.Vector (e)
import Diagrams.Core.Types (keyVal)
import Calculations (mkCoordinates, mkTime, horoscope, angularDifference, rotateList)
import Control.Category ((<<<))
import Prerendered as P
import SwissEphemeris (closeEphemerides, setEphemeridesPath)

--signColor :: (Ord a, Floating a) => ZodiacSign -> Colour a
signColor :: ZodiacSign -> Colour Double
signColor (ZodiacSign _ _ zElement) =
    case zElement of
        Import.Earth -> darkgreen
        Air -> yellow
        Fire -> red
        Water -> lightblue


zodiacCircle :: ChartContext -> Diagram B
zodiacCircle env = 
    mconcat $ map zodiacBand westernZodiacSigns
    where
        zodiacBand sign@(ZodiacSign signName zLng _) = 
            g <> w # fc (signColor sign)
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

houseLabel :: HouseNumber -> String
houseLabel = fromEnum >>> (+1) >>> show

ascendant :: [House] -> House
ascendant h =  h !! 0

mc :: [House] -> House
mc h = h !! 9

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


quadrantLabel :: HouseNumber -> String
quadrantLabel I = "ASC"
quadrantLabel IV = "IC"
quadrantLabel VII = "DC"
quadrantLabel X = "MC"
quadrantLabel _ = ""

quadrants :: ChartContext -> [House] -> Diagram B
quadrants env c = 
    mconcat $ map quadrant angles
    where
        angles = 
            [(c !! 0, c !! 3) -- AC
            ,(c !! 3, c !! 6) -- IC
            ,(c !! 6, c !! 9) -- DC
            ,(c !! 9, c !! 0) -- MC
            ]
        quadrant (House houseName cuspBegin, House _ cuspEnd) =
            t <> w # lw thin
                   # (href $ "#angle-" <> (show houseName))
            where
                onZodiacs = env ^. zodiacCircleRadiusL 
                d = rotateBy ((cuspBegin @@ deg) ^. turn) xDir
                a = (angularDifference cuspBegin cuspEnd) @@ deg
                w = wedge 1 d a
                textPosition = longitudeToPoint (onZodiacs - 0.05) (cuspBegin + 4)
                t = (text $ quadrantLabel houseName) 
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

-- TODO: group planets by latitude and lay them radially, vs. hoping they're not in the same latitude!
planets :: ChartContext -> [PlanetPosition] -> Diagram B
planets env planetPositions =
    mconcat $ map drawPlanet planetPositions
    where
        onAspects = env ^. aspectCircleRadiusL
        onZodiacs = env ^. zodiacCircleRadiusL
        onPlanets = env ^. planetCircleRadiusL
        drawPlanet pos@PlanetPosition{..} =
            (stroke $ P.prerenderedPlanet planetName)
            # scale 0.1
            # moveTo eclipticPosition
            # rectifyAround eclipticPosition env
            # fc black
            # lw ultraThin
            # (keyVal $ ("title", show planetName))
            # (href $ "#" <> (show planetName))
            <> guideLines
            where
                atEclipticPosition = flip longitudeToPoint $ getLongitude pos
                eclipticPosition = atEclipticPosition onPlanets
                aspectCircleLine = atEclipticPosition onAspects ~~ atEclipticPosition (onAspects + 0.03)
                zodiacCircleLine = atEclipticPosition onZodiacs ~~ atEclipticPosition (onZodiacs - 0.03)
                guideLines = (aspectCircleLine <> zodiacCircleLine) # lw thin


--chart :: (Semigroup m, TrailLike (QDiagram b V2 Longitude m)) => [House] -> QDiagram b V2 Longitude m
chart :: ChartContext -> HoroscopeData -> Diagram B
chart env HoroscopeData{..}  = do
    (zodiacCircle env)
    <> (planets env horoscopePlanetPositions )
    <> (cuspsCircle env horoscopeHouses)
    <> (quadrants env horoscopeHouses)
    <> (aspects env horoscopePlanetaryAspects)
    -- TODO: horoscopeCelestialAspects?

rectifyAround :: Point V2 Double -> ChartContext -> Diagram B -> Diagram B
rectifyAround point env =
    rotateAround point (negated (offset_ @@ deg))
    where
        offset_ = env ^. ascendantOffsetL

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
        ascendantOffset =  (180 - ((houseCusp . ascendant) horoscopeHouses))

renderTestChart :: IO ()
renderTestChart = do
    -- TODO: bring in the `directory` package?
    setEphemeridesPath "/Users/luis/code/lfborjas/cassiel/config"
    --let calculations = horoscope 2447532.771485 (mkCoordinates 14.0839053 (-87.2750137))
    let calculations = horoscope (mkTime 1989 1 6 0.0) (mkCoordinates 14.0839053 (-87.2750137))
    renderChart calculations
    closeEphemerides
