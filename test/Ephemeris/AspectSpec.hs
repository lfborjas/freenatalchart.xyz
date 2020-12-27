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
            ]
          sq = Aspect{ aspectType = Major, aspectName = Square, angle = 90.0, maxOrb = 10.0, temperament = Analytical }
      forM_ assertions $ \((aspecting', aspected'), expected) ->
        it "Finds aspect angles" $
          (findAspectAngle sq aspecting' aspected' <&> phaseAndOrb) `shouldBe` expected
