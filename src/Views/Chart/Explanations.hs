{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Views.Chart.Explanations where

import CMark
import Data.String.Interpolate.IsString
import Import
import Lucid

markdownToHtml :: Text -> Html ()
markdownToHtml = toHtmlRaw . commonmarkToHtml []

--explainSign :: ZodiacSignName -> Html ()
--explainHouse :: HouseNumber -> Html ()
--explainPlanet :: Planet -> Html ()
--explainAspect :: Aspect -> Html ()
