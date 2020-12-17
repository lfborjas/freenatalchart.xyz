module Dev where

import Lucid (renderToFile)
import qualified Views.Index as Index
import Views.Common (fixtureRenderContext)

-- | render the index page to a known test location
renderTestIndex :: IO ()
renderTestIndex = renderToFile "test/files/index.html" $ Index.render fixtureRenderContext Nothing
