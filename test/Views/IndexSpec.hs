module Views.IndexSpec (spec) where

import TestUtil (goldenFixture,  renderHtmlToString )
import Views.Common (fixtureRenderContext)
import Views.Index (render)
import Test.Hspec ( context, describe, it, Spec )

spec :: Spec
spec =
  describe "Index" $ do
    context "When initially landing, one should see the form, no errors" $ do
      it "renders html" $ do
        let rendered = renderHtmlToString $ render fixtureRenderContext Nothing
        goldenFixture "landing" rendered
