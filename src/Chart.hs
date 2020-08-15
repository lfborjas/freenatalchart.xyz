{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Chart where

import Import hiding ((^.), local)
import Diagrams.Prelude
import Diagrams.Backend.SVG
import RIO.List (cycle)
import RIO.List.Partial ((!!))

--signColor :: (Ord a, Floating a) => ZodiacSign -> Colour a
signColor :: ZodiacSign -> Colour Double
signColor (ZodiacSign _ _ zElement) =
    case zElement of
        Earth -> darkgreen
        Air -> yellow
        Fire -> red
        Water -> lightblue

zodiacBand :: (TrailLike b, HasStyle b, N b ~ Longitude, V b ~ V2) => ZodiacSign -> b
zodiacBand sign@(ZodiacSign _ zLng _) = 
    w # fc (signColor sign)
      # lw thin
    where
        d :: Direction V2 Longitude
        d = rotateBy ((zLng @@ deg) ^. turn) xDir
        a :: Angle Double
        a = 30 @@ deg
        w = annularWedge 1 0.8 d a

zodiacCircle :: (Monoid a, TrailLike a, HasStyle a, N a ~ Longitude, V a ~ V2) => a
zodiacCircle = mconcat $ map zodiacBand westernZodiacSigns

cuspBand :: (HasStyle b, TrailLike b, V b ~ V2, N b ~ Longitude) => (House, House) -> b
cuspBand (House houseName cuspBegin, House _ cuspEnd) =
    w # lw thin
      # fc (houseColor houseName) -- houseColor is for debugging, no meaning.
                               
    where
        d = rotateBy ((cuspBegin @@ deg) ^. turn) xDir
        a = (angularDifference cuspBegin cuspEnd) @@ deg
        w = annularWedge 0.8 0.5 d a

angularDifference :: Longitude -> Longitude -> Longitude
angularDifference a b | (b - a) < 1 = (b + 360 - a)
                      | otherwise = b - a

houseColor :: HouseNumber -> Colour Double
houseColor h =
    case h of
        I -> red
        II -> green
        III -> blue
        IV -> yellow
        V -> cyan
        VI -> black
        VII -> brown
        VIII -> magenta
        IX -> darkorange
        X -> orchid
        XI -> forestgreen
        XII -> white

cuspsCircle :: (Monoid a, HasStyle a, TrailLike a, V a ~ V2, N a ~ Longitude) => [House] -> a
cuspsCircle c = 
    mconcat $ map cuspBand pairedC
    where
        pairedC = zip c $ rotateList 1 c

quadrant :: (TrailLike b, HasStyle b, N b ~ Longitude, V b ~ V2) => (House, House) -> b
quadrant (House _ cuspBegin, House _ cuspEnd) =
    w # lw thin
    where 
        d = rotateBy ((cuspBegin @@ deg) ^. turn) xDir
        a = (angularDifference cuspBegin cuspEnd) @@ deg
        w = wedge 1 d a

quadrants :: (Monoid a, TrailLike a, HasStyle a, N a ~ Longitude, V a ~ V2) => [House] -> a
quadrants c = 
    mconcat $ map quadrant angles
    where
        angles = 
            [(c !! 0, c !! 3)
            ,(c !! 3, c !! 6)
            ,(c !! 6, c !! 9)
            ,(c !! 9, c !! 0)
            ]

ascendant :: [House] -> Longitude
ascendant h =  h !! 0 & houseCusp

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

chart :: (Monoid a, TrailLike a, HasStyle a, N a ~ Longitude, V a ~ V2) => [House] -> a
chart cusps = zodiacCircle <> cuspsCircle cusps <> quadrants cusps

-- from: https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n  xs = zipWith const (drop n (cycle xs)) xs

renderChart :: IO ()
renderChart =
  renderSVG
    "circle.svg"
    (mkWidth 400)
    (chart cusps_ # rotateBy ascendantOffset)
   where
       ascendantOffset = (180 - (ascendant cusps_) @@ deg) ^. turn
