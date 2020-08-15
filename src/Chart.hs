{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Chart where

import Import hiding ((^.))
import Diagrams.Prelude
import Diagrams.Backend.SVG

--signColor :: (Ord a, Floating a) => ZodiacSign -> Colour a
signColor :: ZodiacSign -> Colour Double
signColor (ZodiacSign _ _ zElement) =
    case zElement of
        Earth -> darkgreen
        Air -> yellow
        Fire -> lightsalmon
        Water -> lightblue


zodiacBand :: (TrailLike b, HasStyle b, N b ~ Longitude, V b ~ V2) => ZodiacSign -> b
zodiacBand sign@(ZodiacSign _ zLng _) = 
    w # fc (signColor sign)
    where
        d :: Direction V2 Longitude
        d = rotateBy ((zLng @@ deg) ^. turn) xDir
        a :: Angle Double
        a = 30 @@ deg
        w = annularWedge 1 0.8 d a

zodiacCircle :: (Monoid a, TrailLike a, HasStyle a, N a ~ Longitude, V a ~ V2) => a
zodiacCircle = mconcat $ map zodiacBand westernZodiacSigns

renderChart :: IO ()
renderChart = renderSVG "circle.svg" (mkWidth 400) zodiacCircle
