module Ephemeris.AspectSpec (spec) where

import Ephemeris.Aspect
import Ephemeris.Types
import Test.Hspec (shouldBe, it, describe, Spec)
import RIO ((&), (<&>), forM_)
import Text.Printf (printf)

phaseAndOrb :: AspectAngle -> (AspectPhase, Double)
phaseAndOrb (AspectAngle _aspecting _aspected phase orb') = (phase, orb')

spec :: Spec
spec = do
  describe "findAspectAngle" $ do
    let assertions = 
          [
            ((Longitude 175, Longitude 270), Just (Applying, 5))
          , ((Longitude 185, Longitude 270), Just (Separating, 5))
          , ((Longitude 270, Longitude 270), Nothing)
          , ((Longitude 355, Longitude 270), Just (Applying, 5))
          , ((Longitude 5,   Longitude 270), Just (Separating, 5))
          , ((Longitude 270, Longitude 10),  Just (Applying, 10))
          , ((Longitude 290, Longitude 10),  Just (Separating, 10))
          , ((Longitude 95,  Longitude 10),  Just (Applying, 5))
          , ((Longitude 100, Longitude 10),  Just (Exact, 0))
          , ((Longitude 130, Longitude 10),  Nothing)
          ]
        sq = Aspect{ aspectType = Major, aspectName = Square, angle = 90.0, maxOrb = 10.0, temperament = Analytical }
    forM_ assertions $ \((aspecting', aspected'), expected) ->
      it (printf "Determines square aspect angle and apparent phase between %.2f and %.2f" (aspecting' & getLongitudeRaw) (aspected' & getLongitudeRaw)) $
        (findAspectAngle sq aspecting' aspected' <&> phaseAndOrb) `shouldBe` expected
