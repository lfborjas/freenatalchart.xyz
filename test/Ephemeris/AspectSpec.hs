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
            (sq, (Longitude 175, Longitude 270), Just (Applying, 5))
          , (sq, (Longitude 185, Longitude 270), Just (Separating, 5))
          , (sq, (Longitude 270, Longitude 270), Nothing)
          , (sq, (Longitude 355, Longitude 270), Just (Applying, 5))
          , (sq, (Longitude 5,   Longitude 270), Just (Separating, 5))
          , (sq, (Longitude 270, Longitude 10),  Just (Applying, 10))
          , (sq, (Longitude 290, Longitude 10),  Just (Separating, 10))
          , (sq, (Longitude 95,  Longitude 10),  Just (Applying, 5))
          , (sq, (Longitude 100, Longitude 10),  Just (Exact, 0))
          , (sq, (Longitude 130, Longitude 10),  Nothing)
          , (cnj, (Longitude 270, Longitude 270), Just (Exact, 0) )
          ]
        sq = Aspect{ aspectType = Major, aspectName = Square, angle = 90.0, maxOrb = 10.0, temperament = Analytical }
        cnj = Aspect{ aspectType = Major, aspectName = Conjunction, angle = 0.0, maxOrb = 10.0, temperament = Analytical }
    forM_ assertions $ \(aspect', (aspecting', aspected'), expected) ->
      it (printf "Determines aspect angle and apparent phase between %.2f and %.2f" (aspecting' & getLongitudeRaw) (aspected' & getLongitudeRaw)) $
        (findAspectAngle aspect' aspecting' aspected' <&> phaseAndOrb) `shouldBe` expected
