module TestUtil where

import Test.Hspec.Golden ( Golden(..) )
import RIO.Text.Lazy (unpack)
import qualified Lucid.Base as L


goldenFixture :: String -> String -> Golden String
goldenFixture name output_ =
  Golden {
    output = output_
  , encodePretty = show
  , testName = name
  , writeToFile = writeFile
  , readFromFile = readFile
  , directory = "test/files"
  , failFirstTime = False
  }

renderHtmlToString :: L.Html a -> String
renderHtmlToString = unpack . L.renderText
