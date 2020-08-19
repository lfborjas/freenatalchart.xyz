{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Chart where

import Import hiding ((^.), local)
import Diagrams.Prelude
import Diagrams.Backend.SVG
import RIO.List (cycle)
import RIO.List.Partial ((!!))
import Diagrams.TwoD.Vector (e)
import Diagrams.Core.Types (keyVal)
import Calculations (angularDifference, rotateList)
import qualified Graphics.SVGFonts as SF
import System.IO.Unsafe (unsafePerformIO)

defaultGlyphFont = unsafePerformIO $ SF.lin
{-# NOINLINE defaultGlyphFont #-}

--signColor :: (Ord a, Floating a) => ZodiacSign -> Colour a
signColor :: ZodiacSign -> Colour Double
signColor (ZodiacSign _ _ zElement) =
    case zElement of
        Earth -> darkgreen
        Air -> yellow
        Fire -> red
        Water -> lightblue

-- TODO: this function takes forever, I believe it's because of `textSVG`
-- having to load the font in _every_ invocation. We need a reader monad
-- or something to provide it, so we only need to load it once:
-- http://hackage.haskell.org/package/SVGFonts-1.7.0.1/docs/src/Graphics.SVGFonts.Text.html#line-41
-- also, calculating each path takes quite a bit, so we may even need to have a map
-- of pre-rendered paths.
signGlyph :: (Read n, RealFloat n) => ZodiacSign -> Path V2 n
signGlyph (ZodiacSign zName _ _) =
    SF.textSVG' (SF.TextOpts defaultGlyphFont SF.INSIDE_H SF.KERN False 1 1) unicodeRepr
    where
    unicodeRepr =
        case zName of
            Aries  -> "♈️"
            Taurus -> "♉️"
            Gemini -> "♊️"
            Cancer -> "♋️"
            Leo    -> "♌️"
            Virgo  -> "♍️"
            Libra  -> "♎️"
            Scorpio -> "♏️"
            Sagittarius -> "♐️"
            Capricorn -> "♑️"
            Aquarius -> "♒️"
            Pisces -> "♓️"

--zodiacBand :: (TrailLike (QDiagram b V2 Longitude m), Semigroup m) => ZodiacSign -> QDiagram b V2 Longitude m
zodiacBand sign@(ZodiacSign signName zLng _) = 
    g <> w # fc (signColor sign)
           # lw thin
           # (href $ "/explanations#zodiac-" <> (show signName))
           -- can set `title`, `id` or `class`:
           -- https://hackage.haskell.org/package/diagrams-svg-1.4.3/docs/src/Diagrams.Backend.SVG.html
           # (keyVal $ ("title", show signName))
    where
        d :: Direction V2 Longitude
        d = rotateBy ((zLng @@ deg) ^. turn) xDir
        a :: Angle Double
        a = 30 @@ deg
        w = annularWedge 1 0.8 d a
        glyphPosition = longitudeToPoint (zLng + 15) 0.9
        -- TODO: these take _forever_ to render on demand. Need to pre-render
        -- in some way! Maybe between these, settings and some images,
        -- we need a reader monad that carries them around?
        -- g = mempty
        g = (stroke $ signGlyph sign)
            # scale 0.15
            # moveTo glyphPosition
            # rotateAround glyphPosition (-70 @@ deg)
            # fc black
            # lw thin

--zodiacCircle :: (Semigroup m, TrailLike (QDiagram b V2 Longitude m)) => QDiagram b V2 Longitude m
zodiacCircle = mconcat $ map zodiacBand westernZodiacSigns

cuspBand (House houseName cuspBegin, House _ cuspEnd) =
    t <> w # lw thin
           # (href $ "/explanations#house-" <> (show houseName))
    where
        d = rotateBy ((cuspBegin @@ deg) ^. turn) xDir
        a = (angularDifference cuspBegin cuspEnd) @@ deg
        w = annularWedge 0.8 0.5 d a
        textPosition :: Point V2 Double
        textPosition = longitudeToPoint (cuspBegin + 5) 0.55
        t = (text $ houseLabel houseName) 
            # moveTo textPosition
            # fontSize (local 0.05)
            # fc gray
            # rotateAround textPosition (-70 @@ deg)

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
longitudeToPoint :: Longitude -> Double -> Point V2 Double
longitudeToPoint longitude magnitude = 
    origin .+^ v
    where
        theta = longitude @@ deg
        v = magnitude *^ e theta


--cuspsCircle :: (Semigroup m, TrailLike (QDiagram b V2 Longitude m)) => [House] -> QDiagram b V2 Longitude m
cuspsCircle c = 
    mconcat $ map cuspBand pairedC
    where
        pairedC = zip c $ rotateList 1 c


--quadrant :: (TrailLike (QDiagram b V2 Longitude m), Semigroup m) => (House, House) -> QDiagram b V2 Longitude m
quadrant (House houseName cuspBegin, House _ cuspEnd) =
    t <> w # lw thin
           # (href $ "/explanations#angle-" <> (show houseName))
    where 
        d = rotateBy ((cuspBegin @@ deg) ^. turn) xDir
        a = (angularDifference cuspBegin cuspEnd) @@ deg
        w = wedge 1 d a
        textPosition = longitudeToPoint (cuspBegin + 4) 0.75
        t = (text $ quadrantLabel houseName) 
            # moveTo textPosition
            # fontSize (local 0.05)
            # fc black
            # rotateAround textPosition (-70 @@ deg)

quadrantLabel :: HouseNumber -> String
quadrantLabel I = "ASC"
quadrantLabel IV = "IC"
quadrantLabel VII = "DC"
quadrantLabel X = "MC"
quadrantLabel _ = ""

--quadrants :: (Semigroup m, TrailLike (QDiagram b V2 Longitude m)) => [House] -> QDiagram b V2 Longitude m
quadrants c = 
    mconcat $ map quadrant angles
    where
        angles = 
            [(c !! 0, c !! 3) -- AC
            ,(c !! 3, c !! 6) -- IC
            ,(c !! 6, c !! 9) -- DC
            ,(c !! 9, c !! 0) -- MC
            ]



cusps_ :: [House]
cusps_ 
    = [
        (House I $ id 112.20189657163523)
    ,   (House II $ id 138.4658382335878)
    ,   (House III $ id 167.69682489058204)
    ,   (House IV $ id 199.79861981778183)
    ,   (House V $ id 232.2797046698429)
    ,   (House VI $ id 263.0249102802477)
    ,   (House VII $ id 292.20189657163525)
    ,   (House VIII $ id 318.46583823358776)
    ,   (House IX $ id 347.69682489058204)
    ,   (House X $ id 19.798619817781823)
    ,   (House XI $ id 52.27970466984291)
    ,   (House XII $ id 83.02491028024768)
    ]


exampleAspect = 
    sunPos ~~ marsPos # lc red
    where
        sunPos = longitudeToPoint 285.64723120365153 0.5
        marsPos = longitudeToPoint 22.784889069947795 0.5
--chart :: (Semigroup m, TrailLike (QDiagram b V2 Longitude m)) => [House] -> QDiagram b V2 Longitude m
chart cusps = zodiacCircle <> cuspsCircle cusps <> quadrants cusps <> exampleAspect

renderChart :: IO ()
renderChart =
  renderSVG
    "circle.svg"
    (mkWidth 400)
    (chart cusps_ # rotateBy ascendantOffset)
   where
       ascendantOffset =  (180 - ((houseCusp . ascendant) cusps_) @@ deg) ^. turn
