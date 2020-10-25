{-# LANGUAGE OverloadedStrings #-}

module Chart.PrerenderedSpec (spec) where

import Chart.Prerendered
import TestUtil (goldenFixture)
import Test.Hspec ( context, describe, it, Spec )
import qualified Graphics.Svg as Svg
import Diagrams.Backend.SVG
import Diagrams (mkWidth, renderDia)
import RIO.Text.Lazy (unpack)

renderSVGString :: String
renderSVGString = 
  unpack $ Svg.renderText dia
  where
  dia =
    renderDia SVG
      (SVGOptions (mkWidth 400) Nothing "" [] True)
      allPrerendered

spec :: Spec
spec =
  describe "Prerendered Glyphs" $ do
    context "Signs, planets and aspects" $ do
      it "renders icons based on the Symbola font" $ do
        let rendered = renderSVGString
        goldenFixture "prerenderedIcons" rendered
