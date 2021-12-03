{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
module Chart.Graphics where

import Ephemeris
    (transitAspects,  Angles(..),
      ZodiacSign(ZodiacSign),
      Element(Water, Earth, Air, Fire),
      HasLabel(label),
      HasLongitude(..),
      House(House, houseCusp),
      HouseNumber,
      Aspect(temperament),
      AspectTemperament(Neutral, Analytical, Synthetic),
      HoroscopeAspect(..),
      Longitude(Longitude),
      westernZodiacSigns,
      isRetrograde,
      HoroscopeData(..),
      TransitData(..),
      PlanetPosition(..),
      triggeredTransits, Planet)
import Chart.Prerendered as P
    ( prerenderedPlanet, prerenderedSign )
import Diagrams.Backend.SVG (svgClass,  Options(SVGOptions), SVG(SVG), B )
import Diagrams.Core.Types (keyVal)
import Diagrams.Prelude hiding (aspect)
import Diagrams.TwoD.Vector (e)
import qualified Graphics.Svg as Svg
import Import hiding ((^.), local)
import RIO.List (groupBy, sortBy, sortOn)
import Utils (rotateList)
import SwissEphemeris.ChartUtils
import qualified Debug.Trace as Debug
import Data.List (head)

zodiacCircle :: ChartContext -> Diagram B
zodiacCircle env =
  mconcat $ map zodiacBand westernZodiacSigns
  where
    zodiacBand (ZodiacSign signName (Longitude z) zElement) =
      g <> w
        # lw ultraThin
        # (href $ "#" <> (show signName))
        -- can set `title`, `id` or `class`:
        -- https://hackage.haskell.org/package/diagrams-svg-1.4.3/docs/src/Diagrams.Backend.SVG.html
        # (keyVal $ ("title", show signName))
        # svgClass "zodiac-segment"
      where
        onZodiacs = env ^. zodiacCircleRadiusL
        d :: Direction V2 Double
        d = rotateBy ((z @@ deg) ^. turn) xDir
        a :: Angle Double
        a = 30 @@ deg
        w = annularWedge 1 (onZodiacs) d a
        glyphPosition = longitudeToPoint (onZodiacs + 0.1) (Longitude $ z + 15)
        g =
          (stroke $ P.prerenderedSign signName)
            # scale 0.08
            # moveTo glyphPosition
            # rectifyAround glyphPosition env
            # fc black
            # lw none
            # svgClass zodiacClass
        zodiacClass =
          case zElement of
            Earth -> "earth-sign"
            Air -> "air-sign"
            Fire -> "fire-sign"
            Water -> "water-sign"

cuspsCircle :: ChartContext -> [House] -> Diagram B
cuspsCircle env c =
  mconcat $ map cuspBand pairedC
  where
    onZodiacs = env ^. zodiacCircleRadiusL
    onAspects = env ^. aspectCircleRadiusL
    pairedC = zip c $ rotateList 1 c
    classPrefix = env & chartHouseClassPrefix
    cuspBand (House houseName (Longitude cuspBegin) _, House _ (Longitude cuspEnd) _) =
      t <> w # lw ultraThin
        # lc black
        # (href $ "#house-" <> (show houseName))
        # svgClass (classPrefix <> "-segment")
      where
        d = rotateBy ((cuspBegin @@ deg) ^. turn) xDir
        a = (angularDifference cuspBegin cuspEnd) @@ deg
        w = annularWedge (onZodiacs) (onAspects) d a
        textPosition :: Point V2 Double
        textPosition = longitudeToPoint (onAspects + 0.05) (Longitude $ cuspBegin + 5)
        t =
          (text $ houseLabel houseName)
            # moveTo textPosition
            # fontSize (local 0.05)
            # rectifyAround textPosition env
            # svgClass (classPrefix <> "-label")

quadrants :: ChartContext -> Angles -> Diagram B
quadrants env Angles {..} =
  mconcat $ map quadrant angles
  where
    angles = [(Longitude ascendant, "Asc"), (Longitude mc, "MC")]
    quadrant (cuspBegin, label') =
      t
      where
        onZodiacs = env ^. zodiacCircleRadiusL
        textPosition = longitudeToPoint (onZodiacs - 0.05) (cuspBegin + 4)
        t =
          (text $ label')
            # moveTo textPosition
            # fontSize (local 0.05)
            # fc black
            # rectifyAround textPosition env
            # svgClass ((env & chartHouseClassPrefix) <> "-label")

aspects :: (HasLongitude a, HasLongitude b) => ChartContext -> [HoroscopeAspect a b] -> Diagram B
aspects env pAspects = do
  mconcat $ map aspectLine pAspects
  where
    aspectLine HoroscopeAspect {..} =
      aPos ~~ bPos # svgClass aspectClass
        # lw thin
      where
        onAspect = env ^. aspectCircleRadiusL
        aPos = (fst bodies) & getLongitude & (longitudeToPoint onAspect)
        bPos = (snd bodies) & getLongitude & (longitudeToPoint onAspect)
        aspectClass =
          case (temperament aspect) of
            Analytical -> "analytical-aspect"
            Synthetic -> "synthetic-aspect"
            Neutral -> "neutral-aspect"

planets :: ChartContext -> [PlanetPosition] -> [House] -> Diagram B
planets env planetPositions houses =
  -- multiply the scale to obtain "degrees"
  Debug.trace (show correctedPositions) $ mconcat $ map drawPlanet correctedPositions
  where
    correctedPositions = correctCollisions (originalScale * 100) planetPositions houses
    onAspects = env ^. aspectCircleRadiusL
    onZodiacs = env ^. zodiacCircleRadiusL
    onPlanets = env ^. planetCircleRadiusL
    classPrefix = env & chartPlanetClassPrefix
    originalScale = 0.06
    drawPlanet (corrected, pos@PlanetPosition {..}) =
      (stroke $ P.prerenderedPlanet planetName)
        # scale (maybe originalScale ((originalScale*) . glyphScale) corrected)
        # moveTo correctedPosition
        # rectifyAround correctedPosition env
        # fc black
        # lw none
        # (keyVal $ ("title", label planetName))
        # (href $ "#" <> (label planetName))
        # svgClass classPrefix
        <> (guideLines # svgClass (classPrefix <> "-lines"))
        <> (retrogradeMark # svgClass classPrefix)
      where
        drawPlanetAt = maybe (getLongitude pos) (Longitude . placedPosition) corrected
        atCorrectedPosition = flip longitudeToPoint $ drawPlanetAt
        correctedPosition = atCorrectedPosition onPlanets
        atEclipticPosition = flip longitudeToPoint $ getLongitude pos
        --eclipticPosition = atEclipticPosition onPlanets
        aspectCircleLine = atEclipticPosition onAspects ~~ atEclipticPosition (onAspects + 0.045)
        zodiacCircleLine = atEclipticPosition onZodiacs ~~ atEclipticPosition (onZodiacs - 0.045)
        guideLines = (aspectCircleLine <> zodiacCircleLine) # lw thin
        retrogradeMark =
          if (isRetrograde pos)
            then
              text "r"
                # moveTo (atCorrectedPosition (onPlanets - 0.055))
                # rectifyAround (atCorrectedPosition (onPlanets - 0.055)) env
                # fontSize (local 0.05)
            else mempty


correctCollisions :: Double -> [PlanetPosition] -> [House] -> [(Maybe (GlyphInfo Planet), PlanetPosition)]
correctCollisions scale' planets' houses =
  case correctedPositions of
    Left er -> Debug.trace ("BIG OL ERROR" <> er) $ map (Nothing,) planets'
    Right glyphs ->
      zipWith
        (curry $ first Just)
        (sortOn extraData $ Debug.trace (show firstCusp <> " - " <> show glyphs) $ map recenterGlyph glyphs)
        (sortOn planetName planets')
  where
    firstCusp = getLongitude . head $ houses
    relativeToFirstCusp pos =
      let corrected = pos - firstCusp
      in if corrected < 0 then
        corrected + 360
      else
        corrected
    recenterGlyph g@GlyphInfo{originalPosition, placedPosition} =
      g{
        originalPosition = unrelativeToFirstCusp originalPosition,
        placedPosition = unrelativeToFirstCusp placedPosition
      }
    unrelativeToFirstCusp :: Double -> Double
    unrelativeToFirstCusp pos =
      let undone = pos + (getLongitudeRaw firstCusp)
      in if undone >= 360 then
        undone - 360
      else
        undone
    correctedPositions =
      gravGroup2
        (scale'/2, scale'/2)
        (Debug.trace (show (map (\(_,p)-> (planetName p, planetLng p)) transformedPlanets)) $ transformedPlanets)
        (Debug.trace (show transformedHouses) $ transformedHouses <> [head transformedHouses + 360])
        True
    -- make all the houses relative to the first cusp
    transformedHouses = map (getLongitudeRaw . relativeToFirstCusp . houseCusp) houses
    transformedPlanets =
      -- make all the planets relative to the first cusp, too
      map (\pos@PlanetPosition{planetName, planetLng} -> (planetName, pos{planetLng = relativeToFirstCusp planetLng})) planets'

containerCircle :: Double -> Diagram B
containerCircle r = circle r # lw thin # svgClass "container-circle"

degreeMarkers :: Bool -> Double -> Diagram B
degreeMarkers radiatesOut r =
  mconcat . map degreeLine $ [0..360]
  where
    degreeLine :: Int -> Diagram B
    degreeLine degree =
      (longitudeToPoint startingPoint $ fromIntegral degree) ~~ (longitudeToPoint endingPoint $ fromIntegral degree)
      # lw lineThickness
      where
        startingPoint =
          if radiatesOut then
            r + lineLength
          else
            r
        endingPoint =
          if radiatesOut then
            r
          else
            r - lineLength
        lineLength =
          if (degree `mod` 5 == 0) then 0.025 else 0.02
        lineThickness =
          if (degree `mod` 5 == 0) then thin else ultraThin

chart :: ChartContext -> HoroscopeData -> Diagram B
chart env HoroscopeData {..} =
  do
    (zodiacCircle env)
    <> (planets env horoscopePlanetPositions horoscopeHouses)
    <> (cuspsCircle env horoscopeHouses)
    <> (quadrants env horoscopeAngles)
    <> (aspects env horoscopePlanetaryAspects)
    <> (aspects env horoscopeAngleAspects)
    <> containerCircle 1
    <> (containerCircle $ env ^. zodiacCircleRadiusL)
    <> (containerCircle $ env ^. aspectCircleRadiusL)
    <> (degreeMarkers False $ env ^. zodiacCircleRadiusL)
    <> (degreeMarkers True $ env ^. aspectCircleRadiusL)

transitChart :: ChartContext -> TransitData -> Diagram B
transitChart env TransitData {..} =
  do
    (zodiacCircle env)
    -- <> (planets env natalPlanetPositions natalHouses)
    <> (cuspsCircle env{chartZodiacCircleRadius = 0.6} natalHouses)
    -- <> (quadrants env natalAngles)
    <> (planets env{chartPlanetCircleRadius = 0.7, chartPlanetClassPrefix = "transiting-planet"} transitingPlanetPositions transitingHouses)
    <> (cuspsCircle env{chartAspectCircleRadius = 0.6, chartHouseClassPrefix = "transiting-house"} transitingHouses)
    -- Only draw aspects that become active in the investigated period:
    <> ((aspects env) . transitAspects . triggeredTransits $ planetaryTransits)
    <> ((aspects env) . transitAspects . triggeredTransits $ angleTransits)
    <> containerCircle 1
    <> (containerCircle $ env ^. zodiacCircleRadiusL)
    <> (containerCircle $ env ^. aspectCircleRadiusL)
    <> (degreeMarkers False $ env ^. zodiacCircleRadiusL)
    <> (degreeMarkers True $ env ^. aspectCircleRadiusL)

{-
[(Sun,138.26732378987305),
(Moon,120.87873215931633),
(Mercury,140.4385755626626),
(Venus,178.8158217539658),
(Mars,119.97018451187218),
(Jupiter,212.81203024930488),
(Saturn,196.28016073400948),
(Uranus,288.8339869274421),
(Neptune,237.54824447901484),(Pluto,182.2338301441692),(MeanNode,308.2058767838678),(MeanApog,322.4011695350471),(Chiron,255.70743011754547)]
[0.0,
26.305297898436706,
55.59570096128938,
87.71343819007201,
120.14798419210295,
150.84021142213925,
180.0,206.30529789843672,
235.59570096128942,267.713438190072,300.14798419210297,330.8402114221392]
[(Just (GlyphInfo {originalPosition = 251.12225364423455, glyphSize = (3.0, 3.0), placedPosition = 251.12225364423455, sectorNumber = 4, sequenceNumber = 0, , extraData = Sun}), 
PlanetPosition {planetName = Sun, planetLat = -7.711757762744395e-5, planetLng = 251.12225364423458, planetLngSpeed = 1.0145116477748564, planetDeclination = -22.108142816536382}),

(Just (GlyphInfo {originalPosition = 233.73366201367784, glyphSize = (3.0, 3.0), placedPosition = 236.00291404646447, sectorNumber = 4, sequenceNumber = 1, levelNumber = 0, glyphScale = 1.0, extraData = Moon}), 
PlanetPosition {planetName = Moon, planetLat = 0.7392591636486708, planetLng = 233.73366201367784, planetLngSpeed = 15.087518346061662, planetDeclination = -17.988811724613207}), 

(Just (GlyphInfo {originalPosition = 253.2935054170241, glyphSize = (3.0, 3.0), placedPosition = 112.85492985436152, sectorNumber = 4, sequenceNumber = 2, levelNumber = 0, glyphScale = 1.0, extraData = Mercury}), 
PlanetPosition {planetName = Mercury, planetLat = -1.1076841601804106, planetLng = 253.2935054170241, planetLngSpeed = 1.5684782748812118, planetDeclination = -23.492282720387138}), 

(Just (GlyphInfo {originalPosition = 291.6707516083273, glyphSize = (3.0, 3.0), placedPosition = 289.8549298543615, sectorNumber = 5, sequenceNumber = 3, levelNumber = 0, glyphScale = 1.0, extraData = Venus}), 
PlanetPosition {planetName = Venus, planetLat = -2.7452692768473175, planetLng = 291.6707516083273, planetLngSpeed = 0.5425476510755264, planetDeclination = -24.403277890920503}),

 (Just (GlyphInfo {originalPosition = 232.8251143662337, glyphSize = (3.0, 3.0), placedPosition = 242.00291404646447, sectorNumber = 3, sequenceNumber = 4, levelNumber = 0, glyphScale = 1.0, extraData = Mars}), 
 PlanetPosition {planetName = Mars, planetLat = 0.16826337416975135, planetLng = 232.8251143662337, planetLngSpeed = 0.6922247135016955, planetDeclination = -18.314257279775564}), 
 
 (Just (GlyphInfo {originalPosition = 325.6669601036664, glyphSize = (3.0, 3.0), placedPosition = 325.6669601036664, sectorNumber = 7, sequenceNumber = 5, levelNumber = 0, glyphScale = 1.0, extraData = Jupiter}), 
 PlanetPosition {planetName = Jupiter, planetLat = -1.0435382752690145, planetLng = 325.6669601036664, planetLngSpeed = 0.1380179613314278, planetDeclination = -13.94579462391789}), 
 
 (Just (GlyphInfo {originalPosition = 309.135090588371, glyphSize = (3.0, 3.0), placedPosition = 309.135090588371, sectorNumber = 6, sequenceNumber = 6, levelNumber = 0, glyphScale = 1.0, extraData = Saturn}), PlanetPosition {planetName = Saturn, planetLat = -0.8157859002733764, planetLng = 309.135090588371, planetLngSpeed = 8.083326894244558e-2, planetDeclination = -18.756679145028677}), (Just (GlyphInfo {originalPosition = 41.688916781803584, glyphSize = (3.0, 3.0), placedPosition = 41.688916781803584, sectorNumber = 9, sequenceNumber = 7, levelNumber = 0, glyphScale = 1.0, extraData = Uranus}), PlanetPosition {planetName = Uranus, planetLat = -0.4164318409983254, planetLng = 41.68891678180356, planetLngSpeed = -3.4338362181592245e-2, planetDeclination = 14.943239258816819}), (Just (GlyphInfo {originalPosition = 350.40317433337634, glyphSize = (3.0, 3.0), placedPosition = 351.4506308156509, sectorNumber = 8, sequenceNumber = 8, levelNumber = 0, glyphScale = 1.0, extraData = Neptune}), PlanetPosition {planetName = Neptune, planetLat = -1.1494502391759427, planetLng = 350.40317433337634, planetLngSpeed = 9.221639697004721e-4, planetDeclination = -4.858906967241313}), (Just (GlyphInfo {originalPosition = 295.0887599985307, glyphSize = (3.0, 3.0), placedPosition = 295.8549298543615, sectorNumber = 6, sequenceNumber = 9, levelNumber = 0, glyphScale = 1.0, extraData = Pluto}), PlanetPosition {planetName = Pluto, planetLat = -1.6978750311943902, planetLng = 295.0887599985307, planetLngSpeed = 2.5455979151782895e-2, planetDeclination = -22.783419326272032}), (Just (GlyphInfo {originalPosition = 61.06080663822928, glyphSize = (3.0, 3.0), placedPosition = 61.06080663822928, sectorNumber = 10, sequenceNumber = 10, levelNumber = 0, glyphScale = 1.0, extraData = MeanNode}), PlanetPosition {planetName = MeanNode, planetLat = 0.0, planetLng = 61.060806638229316, planetLngSpeed = -5.2927120164918615e-2, planetDeclination = 20.370152610767}), (Just (GlyphInfo {originalPosition = 75.25609938940858, glyphSize = (3.0, 3.0), placedPosition = 75.25609938940858, sectorNumber = 10, sequenceNumber = 11, levelNumber = 0, glyphScale = 1.0, extraData = MeanApog}), PlanetPosition {planetName = MeanApog, planetLat = 1.264991349508381, planetLng = 75.25609938940862, planetLngSpeed = 0.11084766917715703, planetDeclination = 23.87945999896589}), (Just (GlyphInfo {originalPosition = 8.562359971906972, glyphSize = (3.0, 3.0), placedPosition = 8.562359971906972, sectorNumber = 8, sequenceNumber = 12, levelNumber = 0, glyphScale = 1.0, extraData = Chiron}), PlanetPosition {planetName = Chiron, planetLat = 2.2496231065188264, planetLng = 8.562359971906991, planetLngSpeed = -1.5011310915184279e-2, planetDeclination = 5.46215027730398})]
-}

--


-- CHART UTILS
--

rectifyAround :: Point V2 Double -> ChartContext -> Diagram B -> Diagram B
rectifyAround point env =
  rotateAround point (negated (offset_ @@ deg))
  where
    offset_ = env ^. ascendantOffsetL

houseLabel :: HouseNumber -> String
houseLabel = fromEnum >>> (+ 1) >>> show

-- | Given a longitude and a magnitude (distance from origin)
-- return a point sitting at the equivalent vector
-- more on vectors:
-- https://archives.haskell.org/projects.haskell.org/diagrams/doc/vector.html#vector-operations
longitudeToPoint :: Double -> Longitude -> Point V2 Double
longitudeToPoint magnitude longitude =
  origin .+^ v
  where
    theta = (getLongitudeRaw longitude) @@ deg
    v = magnitude *^ e theta

renderChart :: [Svg.Attribute] -> Double -> HoroscopeData -> Svg.Element
renderChart attrs width' z@HoroscopeData {..} =
  renderDia SVG (SVGOptions (mkWidth width') Nothing "" attrs True) birthChart
  where
    cfg =
      ChartContext
        { chartAscendantOffset = ascendantOffset,
          chartZodiacCircleRadius = 0.8,
          chartAspectCircleRadius = 0.5,
          chartPlanetCircleRadius = 0.65,
          chartPlanetClassPrefix = "planet",
          chartHouseClassPrefix = "house"
        }
    birthChart = chart cfg z # rotateBy ((ascendantOffset @@ deg) ^. turn)
    ascendantOffset = 180 - (ascendant horoscopeAngles)

renderTransitChart :: [Svg.Attribute] -> Double -> TransitData -> Svg.Element
renderTransitChart attrs width' t@TransitData{..} =
  renderDia SVG (SVGOptions (mkWidth width') Nothing "" attrs True) transitChart'
  where
    cfg =
      ChartContext
        { chartAscendantOffset = ascendantOffset
        , chartZodiacCircleRadius = 0.8
        , chartAspectCircleRadius = 0.3
        , chartPlanetCircleRadius = 0.45
        , chartPlanetClassPrefix = "planet"
        , chartHouseClassPrefix = "house"
        }
    transitChart' = transitChart cfg t # rotateBy ((ascendantOffset @@ deg) ^. turn)
    ascendantOffset = 180 - (ascendant natalAngles)

-- | NOTE(luis) this only really works for spans
-- between house cusps. See the much more precise functions in
-- `Ephemeris.Aspect` for calculating angles between longitudes,
-- with apparent phase and orb.
angularDifference :: Double -> Double -> Double
angularDifference a b
  | (b - a) & abs & (< 1) = abs $ b - a
  | (b - a) & signum & (< 1) = (b + 360 - a)
  | otherwise = b - a
