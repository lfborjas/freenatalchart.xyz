import Test.Hspec

import qualified ValidationSpec
import qualified Views.ChartSpec as ChartSpec
import qualified Views.IndexSpec as IndexSpec
import qualified Views.TransitsSpec as TransitsSpec
import qualified Server.HandlersSpec as HandlersSpec
import qualified Ephemeris.AspectSpec as AspectSpec
import qualified Ephemeris.HoroscopeSpec as HoroscopeSpec
import qualified Ephemeris.TransitSpec as TransitSpec
import qualified Chart.PrerenderedSpec as PrerenderedSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ValidationSpec" ValidationSpec.spec
  describe "Views.ChartSpec" ChartSpec.spec
  describe "Views.IndexSpec" IndexSpec.spec
  describe "Views.TransitsSpec" TransitsSpec.spec
  describe "Server.HandlersSpec" HandlersSpec.spec
  describe "Ephemeris.AspectSpec" AspectSpec.spec
  describe "Ephemeris.HoroscopeSpec" HoroscopeSpec.spec
  describe "Ephemeris.TransitSpec" TransitSpec.spec
  describe "Chart.PrerenderedSpec" PrerenderedSpec.spec
