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
  mconcat $ map drawPlanet correctedPositions
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
    Left _e -> map (Nothing,) planets'
    Right glyphs ->
      zipWith
        (curry $ first Just)
        (sortOn extraData $ map recenterGlyph glyphs)
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
      gravGroup
        (scale'/2, scale'/2)
        transformedPlanets
        (transformedHouses <> [head transformedHouses + 360])
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
    <> (planets env natalPlanetPositions natalHouses)
    <> (cuspsCircle env{chartZodiacCircleRadius = 0.6} natalHouses)
    -- <> (quadrants env natalAngles)
    <> (Debug.trace (show transitingPlanetPositions <> " - " <> show transitingHouses ) $ planets env{chartPlanetCircleRadius = 0.7, chartPlanetClassPrefix = "transiting-planet"} transitingPlanetPositions transitingHouses)
    <> (cuspsCircle env{chartAspectCircleRadius = 0.6, chartHouseClassPrefix = "transiting-house"} transitingHouses)
    -- Only draw aspects that become active in the investigated period:
    <> ((aspects env) . transitAspects . triggeredTransits $ planetaryTransits)
    <> ((aspects env) . transitAspects . triggeredTransits $ angleTransits)
    <> containerCircle 1
    <> (containerCircle $ env ^. zodiacCircleRadiusL)
    <> (containerCircle $ env ^. aspectCircleRadiusL)
    <> (degreeMarkers False $ env ^. zodiacCircleRadiusL)
    <> (degreeMarkers True $ env ^. aspectCircleRadiusL)

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
