{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Views.Chart (render) where

import Chart.Graphics (renderChart)
import qualified Graphics.Svg as Svg
import Import
import Lucid
import RIO.Text (pack, toLower)
import RIO.Time (rfc822DateFormat, formatTime, defaultTimeLocale)
import Ephemeris
    ( majorAspects,
      minorAspects,
      defaultPlanets,
      isRetrograde,
      findAspectBetweenPlanets,
      findAspectWithAngle,
      findAspectsByName,
      housePosition,
      planetsByHouse,
      planetsInHouse,
      planetsBySign,
      planetsInSign,
      housesBySign,
      housesInSign,
      findSunSign,
      findMoonSign,
      splitDegrees,
      splitDegreesZodiac,
      findAscendant,
      zodiacSignElement
      )
import Views.Common
    ( broughtToYou, metaCeremony, otherLinks )
import Views.Chart.Explanations
    ( attribution,
      generalAspectsExplanation,
      generalHousesExplanation,
      generalPlanetsExplanation,
      generalSignsExplanation,
      Explicable(explain) )
import Text.Printf (printf)
import Ephemeris.Types

render :: HasStaticRoot a => a -> BirthData -> HoroscopeData -> Html ()
render renderCtx BirthData {..} h@HoroscopeData {..} = html_ $ do
  head_ $ do
    title_ "Your Natal Chart"
    metaCeremony renderCtx
    style_ $ do
      "svg { height: auto; width: auto}\
      \.scrollable-container {overflow: auto !important;}\
      \.planet text{ fill: #c8ad85; }\
      \.container-circle{ stroke: white; }\
      \.flex-container{ display: flex; justify-content: space-between;}\
      \.flex-container span{ flex: auto; }\
      \"

  body_ $ do
    header_ [class_ "navbar bg-dark navbar-fixed navbar-fixed-top"] $ do
      section_ [class_ "navbar-section"] $ do
        a_ [href_ "/", class_ "mr-2"] $ do
          i_ [class_ "icon icon-refresh", title_ "Draw Another Chart"] ""
      section_ [class_ "navbar-section navbar-center navbar-brand"] $ do
         a_ [href_ "/", class_ "brand-text"] "FreeNatalChart.xyz"
      section_ [class_ "navbar-section"] $ do
        a_ [href_ "#chart"] $ do
          i_ [class_ "icon icon-upward", title_ "Back to Top"] ""
        -- a_ [href_ "https://github.com/lfborjas/freenatalchart.xyz/issues/new/choose"
        --    , class_ "btn btn-link text-error"
        --    , target_ "_blank"] $ do
        --   "Report an issue"
    div_ [id_ "main", class_ "container grid-xl mx-4"] $ do
      div_ [id_ "chart", class_ "under-navbar"] $ do
        div_ [class_ "blue-stars-bg text-center", style_ "padding-bottom: 9px"] $ do
          --h1_ [class_ "text-primary"] "Your Natal Chart"
          p_ $ do
            toHtml $ birthLocalTime & formatTime defaultTimeLocale rfc822DateFormat
            br_ []
            toHtml $ birthLocation & locationInput
          div_ [class_ "flex-container text-large"] $ do
            span_ $ do
              span_ [class_ $ elementClassM sunSign] $ do
                maybe mempty asIcon sunSign
              " Sun"
            span_ $ do
              span_ [class_ $ elementClassM moonSign] $ do
                maybe mempty asIcon moonSign
              " Moon"
            span_ $ do
              span_ [class_ $ elementClassM asc] $ do
                maybe mempty asIcon asc
              " Asc"

        figure_ [class_ "figure p-centered my-2", style_ "max-width: 600px;"] $ do
          div_ [] $ do
            -- unfortunately, the underlying library assigns `height` and `width` attributes to the SVG:
            -- https://github.com/circuithub/diagrams-svg/blob/master/src/Graphics/Rendering/SVG.hs#L92-L93
            -- and any attempt to replace them simply prepends or appends instead:
            -- https://hackage.haskell.org/package/svg-builder-0.1.1/docs/src/Graphics.Svg.Core.html#with
            -- so instead we simply set them to invalid strings (sorry console sleuths,)
            -- and then set the attributes via CSS, since that's allowed (they're Geometry Properties:)
            -- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/height#svg
            (toHtmlRaw $ Svg.renderBS $ renderChart [Svg.makeAttribute "height" "not", Svg.makeAttribute "width" "not"] 600 h)

          -- div_ [class_ "tile tile-centered text-center"] $ do
          --   div_ [class_ "tile-icon"] $ do
          --     div_ [class_ "px-2"] $ do
          --       maybe mempty asIcon sunSign
          --       br_ []
          --       span_ [class_ "text-tiny", title_ "Sun Sign"] "Sun"
          --   div_ [class_ "tile-content"] $ do
          --     div_ [class_ "tile-title text-dark"] $ do
          --       toHtml $ birthLocalTime & formatTime defaultTimeLocale rfc822DateFormat
          --       "  ·  "                
          --       toHtml $ birthLocation & locationInput
          --     small_ [class_ "tile-subtitle text-gray"] $ do
          --       toHtml $ horoscopeUniversalTime & formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z"
          --       "  ·  "
          --       latLngHtml birthLocation
          --   div_ [class_ "tile-action"] $ do
          --     div_ [class_ "px-2"] $ do
          --       maybe mempty asIcon asc
          --       br_ []
          --       span_ [class_ "text-tiny", title_ "Ascendant"] "Asc"
        ul_ [class_ "tab tab-block"] $ do
          li_ [class_ "tab-item active"] $ do
            a_ [href_ "#analyze"] "Analyze"
          li_ [class_ "tab-item"] $ do
            a_ [href_ "#understand"] "Understand"
          li_ [class_ "tab-item"] $ do
            a_ [href_ "#introspect"] "Introspect"

        div_ [class_ "divider", id_ "analyze"] ""

        details_ [id_ "planet-positions", class_ "accordion my-2", open_ ""] $ do
          summary_ [class_ "accordion-header"] $ do
            headerIcon
            sectionHeading $ do
              "Planet Positions"

          div_ [class_ "accordion-body scrollable-container"] $ do
            table_ [class_ "table table-no-borders"] $ do
              thead_ [class_ "text-light"] $ do
                tr_ [] $ do
                  th_ [] "Planet"
                  th_ [] "House"
                  th_ [class_ "tooltip tooltip-bottom", data_ "tooltip" "Where in the ecliptic\n(zodiac band as seen from Earth)\n the planet is."] $ do
                    "Longitude"
                  th_ [class_ "tooltip tooltip-bottom", data_ "tooltip" "How many degrees a planet is moving per day.\nNegative speed means retrograde motion."] $ do
                    "Speed"
                  th_ [class_ "tooltip tooltip-bottom", data_ "tooltip" "Position above or below the ecliptic plane;\nmost planets appear to be 'on' the ecliptic,\nbut not all are."] $ do
                     "Latitude"
                  th_ [class_ "tooltip tooltip-bottom", data_ "tooltip" "Angle between the planet's position in the sky\nand the Earth's equator."] $ do
                    "Declination"
              tbody_ [] $ do
                forM_ (horoscopePlanetPositions) $ \pp@PlanetPosition {..} -> do
                  tr_ [] $ do
                    td_ $ do
                      span_ [class_ "text-light"] $ do
                        asIcon planetName
                      planetLabel planetName
                      if isRetrograde pp then
                       span_ [class_ "text-light tooltip", data_ "tooltip" "Retrograde"] " (r)"
                      else
                        ""

                    td_ $ do
                      housePositionHtml $ housePosition horoscopeHouses planetLng

                    td_ $ do
                      htmlDegreesZodiac planetLng

                    td_ $ do
                      htmlDegrees planetLngSpeed

                    td_ $ do
                      htmlDegreesLatitude planetLat

                    td_ $ do
                      htmlDegreesLatitude $ Latitude planetDeclination

        div_ [class_ "divider"] ""

        details_ [id_ "house-cusps", class_ "accordion my-2", open_ ""] $ do
          summary_ [class_ "accordion-header "] $ do
            headerIcon
            sectionHeading "House Cusps"
          div_ [class_ "accordion-body scrollable-container"] $ do
            p_ $ do
              "System Used: "
              span_ [class_ "text-primary"] $ toHtml $ toText horoscopeSystem
              " (to learn more about house systems and the meaning of each house, see the "
              a_ [href_ "#houses"] "Houses"
              " section.)"
            table_ [class_ "table table-no-borders"] $ do
              thead_ [class_ "text-light"] $ do
                tr_ [] $ do
                  th_ [] "House"
                  th_ [] "Cusp"
                  th_ [] "Declination"
              tbody_ [] $ do
                forM_ (horoscopeHouses) $ \hc@House {..} -> do
                  tr_ [] $ do
                    td_ $ do
                      housePositionHtml (Just hc)
                      houseLabel houseNumber
                    td_ $ do
                      htmlDegreesZodiac houseCusp
                    td_ $ do
                      htmlDegreesLatitude $ Latitude houseDeclination

        div_ [class_ "divider"] ""

        details_ [id_ "aspects-summary", class_ "accordion my-2", open_ ""] $ do
          summary_ [class_ "accordion-header"] $ do
            headerIcon
            sectionHeading "Aspects Summary"
          div_ [class_ "accordion-body scrollable-container"] $ do
            p_ $ do
              "For more detailed descriptions of aspects, see the "
              a_ [href_ "#aspects"] "Aspects"
              " section."
            table_ [class_ "table table-scroll"] $ do
              forM_ defaultPlanets $ \rowPlanet -> do
                tr_ [] $ do
                  td_ [] $ do
                    if rowPlanet == Sun
                      then mempty
                      else asIcon rowPlanet
                  forM_ (takeWhile (not . (== rowPlanet) . planetName) horoscopePlanetPositions) $ \PlanetPosition {..} -> do
                    td_ [style_ "border: 1px solid", class_ "text-small"] $ do
                      aspectCell $ findAspectBetweenPlanets horoscopePlanetaryAspects rowPlanet planetName
                  td_ [style_ "border-bottom: 1px solid"] $ do
                    asIcon rowPlanet
              tr_ [] $ do
                td_ [] $ do
                  span_ [class_ "tooltip", data_ "tooltip" "Ascendant"] "AC"
                forM_ (horoscopePlanetPositions) $ \PlanetPosition {..} -> do
                  td_ [style_ "border: 1px solid", class_ "text-small"] $ do
                    aspectCell $ findAspectWithAngle horoscopeAngleAspects planetName I
              tr_ [] $ do
                td_ [] $ do
                  span_ [class_ "tooltip", data_ "tooltip" "Midheaven"] "MC"
                forM_ (horoscopePlanetPositions) $ \PlanetPosition {..} -> do
                  td_ [style_ "border: 1px solid", class_ "text-small"] $ do
                    aspectCell $ findAspectWithAngle horoscopeAngleAspects planetName X

        div_ [class_ "divider", id_ "understand"] ""

        details_ [id_ "signs", class_ "accordion my-2", open_ ""] $ do
          summary_ [class_ "accordion-header"] $ do
            headerIcon
            sectionHeading "Zodiac Signs"
          
          div_ [] $ do
            generalSignsExplanation
            -- forM_ [Aries .. Pisces] $ \zodiacSign -> do
            --   h4_ [id_ $ toText zodiacSign] $ do
            --     asIcon zodiacSign
            --     " "
            --     toHtml . toText $ zodiacSign
            --   backToChart
              
            --   explain zodiacSign
            --   let
            --     planets' = planetsInSign' zodiacSign
            --     in do
            --         h5_ "Planets Contained: "
            --         if null planets' then
            --           p_ $ do
            --             em_ "Your chart doesn't have any planets in this sign."
            --         else
            --           ul_ [] $ do
            --             forM_ planets' $ \p -> do
            --               li_ $ do
            --                 planetDetails p
            --   let
            --     houses' = housesInSign' zodiacSign
            --     in do
            --         h5_ "House cusps contained: "
            --         if null houses' then
            --           p_ $ do
            --             em_ "Your chart doesn't have any house cusps in this sign."
            --         else
            --           ul_ [] $ do
            --             forM_ houses' $ \hs -> do
            --               li_ $ do
            --                 houseDetails hs
        divider_ 
        details_ [id_ "houses", class_ "accordion my-2", open_ ""] $ do
          summary_ [class_ "accordion-header"] $ do
            headerIcon
            sectionHeading "Houses"
          div_ [] $ do
            generalHousesExplanation
            -- forM_ horoscopeHouses $ \huis@House{..} -> do
            --   h4_ [id_ $ "house-" <> toText houseNumber] $ do
            --     toHtml $ "House " <> (toText houseNumber)
            --   backToChart
            --   p_ [] $ do
            --     b_ "Starts at: "
            --     zodiacLink huis
            --   explain houseNumber

            --   let
            --     planets'  = planetsInHouse' huis
            --     in do
            --       h5_ "Planets contained: "
            --       if null planets' then
            --         p_ $ do
            --           em_ "Your chart doesn't have any planets in this house."
            --       else
            --         ul_ [] $ do
            --           forM_ planets' $ \p -> do
            --             li_ $ do
            --               planetDetails p
        divider_ 
        details_ [id_ "planets", class_ "accordion my-2", open_ ""] $ do
          summary_ [class_ "accordion-header"] $ do
            headerIcon
            sectionHeading "Planets"

          div_ [] $ do
            generalPlanetsExplanation
            -- forM_ horoscopePlanetPositions $ \p -> do
            --   h4_ [id_ $ pack . label . planetName $ p] $ do
            --     asIcon . planetName $ p
            --     " "
            --     toHtml . label . planetName $ p
            --   backToChart
            --   p_ [] $ do
            --     b_ "Located in: "
            --     zodiacLink . planetLng $ p
            --     if (isRetrograde p) then
            --       b_ "(retrograde)"
            --     else
            --       mempty
            --   p_ [] $ do
            --     b_ "House: "
            --     maybe mempty houseDetails (housePosition' . planetLng $ p)

            --   explain . planetName $ p

            --   let
            --     aspects' = p & planetName & aspectsForPlanet' & catMaybes
            --     axes'    = p & planetName & axesAspectsForPlanet' & catMaybes
            --     in do
            --       h5_ "Aspects: "
            --       if (null aspects' && null axes') then
            --         p_ $ do
            --           em_ "This planet is unaspected. Note that not having any aspects is rare, which means this planet's sole influence can be quite significant."
            --       else
            --         aspectsList aspects' axes'
        divider_
        details_ [id_ "aspects", class_ "accordion my-2", open_ ""] $ do
          summary_ [class_ "accordion-header"] $ do
            headerIcon
            sectionHeading "Aspects"

          div_ [] $ do
            generalAspectsExplanation
            h4_ "Orbs we use"
            p_ "All aspects you see in this page are calculated using the following orbs:"
            table_ [id_ "orbs-used", class_ "table table-no-borders"] $ do
              thead_ [] $ do
                tr_ [] $ do
                  th_ "Aspect"
                  th_ "Angle"
                  th_ "Orb"
              tbody_ [] $ do
                forM_ (majorAspects <> minorAspects) $ \Aspect {..} -> do
                  tr_ [] $ do
                    td_ $ do
                      asIcon aspectName
                      " "
                      toHtml $ toText aspectName
                    td_ $ do
                      toHtml $ toText angle
                    td_ $ do
                      toHtml $ toText maxOrb

            -- h4_ "Major Aspects: "
            -- forM_ majorAspects $ \a -> do
            --   aspectDetails' a 

            -- h4_ "Minor Aspects: "
            -- forM_ minorAspects $ \a -> do
            --   aspectDetails' a
        divider_ 
        details_ [id_ "references", class_ "accordion my-2"] $ do
          summary_ [class_ "accordion-header"] $ do
            headerIcon
            sectionHeading "References"
          div_ [class_ "accordion-body"] $ do
            attribution

        div_ [class_ "divider", id_ "introspect"] ""
              

    link_ [rel_ "stylesheet", href_ "https://unpkg.com/spectre.css/dist/spectre-icons.min.css"]
    footer_ [class_ "navbar bg-secondary"] $ do
      section_ [class_ "navbar-section"] $ do
        otherLinks
      section_ [class_ "navbar-center"] $ do
        broughtToYou
      section_ [class_ "navbar-section"] $ do
        a_ [href_ "https://github.com/lfborjas/freenatalchart.xyz", title_ "Made in Haskell with love and a bit of insanity.", class_ "btn btn-link"] "Source Code"
  where
    -- markup helpers
    headerIcon = i_ [class_ "icon icon-arrow-right mr-1 c-hand icon-right icon-light"] ""
    sectionHeading = h3_ [class_ "d-inline text-primary"]
    sunSign = (findSunSign horoscopePlanetPositions)
    moonSign = (findMoonSign horoscopePlanetPositions)
    asc = (findAscendant horoscopeHouses)
    planetsByHouse' = planetsByHouse horoscopeHouses horoscopePlanetPositions
    planetsInHouse' = planetsInHouse planetsByHouse'
    planetsBySign'  = planetsBySign horoscopePlanetPositions
    planetsInSign'  = planetsInSign planetsBySign'
    housesBySign'   = housesBySign horoscopeHouses
    housesInSign'   = housesInSign housesBySign'
    --housePosition'  = housePosition horoscopeHouses
    aspectsForPlanet' p = map (findAspectBetweenPlanets horoscopePlanetaryAspects p) [Sun .. Chiron]
    axesAspectsForPlanet' p = map (findAspectWithAngle horoscopeAngleAspects p)  [I, X]
    aspectDetails' = aspectDetails horoscopePlanetaryAspects horoscopeAngleAspects
    divider_ = div_ [class_ "divider"] ""



aspectDetails :: [HoroscopeAspect PlanetPosition PlanetPosition] -> [HoroscopeAspect PlanetPosition House] -> Aspect -> Html ()
aspectDetails allPlanetAspects allAxesAspects Aspect {..} = do
  h5_ [id_ $ toText aspectName] $ do
    asIcon aspectName
    " "
    toHtml . toText $ aspectName
  backToChart
  dl_ $ do
    dt_ "Classification"
    dd_ . toHtml . toText $ aspectType
    dt_ "Temperament"
    dd_ . toHtml . toText $ temperament
    dt_ "Traditional color"
    dd_ . toHtml . aspectColor $ temperament
    dt_ "Angle"
    dd_ . toHtml . toText $ angle
    dt_ "Orb used"
    dd_ . toHtml . toText $ maxOrb
  p_ [] $ do
    explain aspectName  
  h6_ . toHtml $ (toText aspectName) <> "s in your chart:"  
  if (null planetAspects && null axesAspects) then
    em_ . toHtml $ "No " <> (toText aspectName) <> "s appear in your chart."
  else
    aspectsList planetAspects axesAspects
  where
    planetAspects = findAspectsByName allPlanetAspects aspectName
    axesAspects   = findAspectsByName allAxesAspects aspectName

aspectsList :: [HoroscopeAspect PlanetPosition PlanetPosition] -> [HoroscopeAspect PlanetPosition House] -> Html ()
aspectsList aspects' axes'= do
  ul_ [] $ do
    forM_ aspects' $ \pa -> do
      li_ $ do
        planetAspectDetails pa  
    forM_ axes' $ \aa -> do
      li_ $ do
        axisAspectDetails aa

planetAspectDetails :: (HoroscopeAspect PlanetPosition PlanetPosition) -> Html ()
planetAspectDetails HoroscopeAspect{..} = do
  span_ [aspectColorStyle aspect] $ do
    bodies & fst & planetName & asIcon
    aspect & aspectName & asIcon
    bodies & snd & planetName & asIcon
  " — "
  bodies & fst & planetName & planetLink
  " "
  strong_ $ aspect & aspectName & aspectLink
  " "
  bodies & snd & planetName & planetLink
  "; orb: "
  htmlDegrees' (True, True) orb

axisAspectDetails :: (HoroscopeAspect PlanetPosition House) -> Html ()
axisAspectDetails HoroscopeAspect{..} = do
  span_ [aspectColorStyle aspect] $ do
    bodies & fst & planetName & asIcon
    aspect & aspectName & asIcon
    bodies & snd & houseNumber & houseLabel
  " — "
  bodies & fst & planetName & planetLink
  " "
  strong_ $ aspect & aspectName & aspectLink
  " "
  bodies & snd & houseNumber & houseLink
  "; orb: "
  htmlDegrees' (True, True) orb

planetDetails :: PlanetPosition -> Html ()
planetDetails PlanetPosition{..} = 
  span_ [] $ do
    asIcon planetName
    a_ [href_ $ "#" <> (pack . label) planetName] $ do
      planetLabel planetName
    " — located in: "
    zodiacLink planetLng

houseDetails :: House -> Html ()
houseDetails House{..} =
  span_ [] $ do
    a_ [href_ $ "#house-" <> toText houseNumber] $ do
      toHtml $ "House " <> toText houseNumber
      houseLabel houseNumber
    " — starting at: "
    zodiacLink houseCusp

asIcon :: HasLabel a => a -> Html ()
asIcon z =
  i_ [class_ ("fnc-" <> shown <> " tooltip"), title_ shown, data_ "tooltip" label'] ""
  where
    label' = pack . label $ z
    shown  = toText z

formatDouble :: Double -> String
formatDouble = printf "%.4f"

htmlDegreesZodiac :: HasLongitude a => a -> Html ()
htmlDegreesZodiac p =
  span_ [title_ . pack . formatDouble $ pl] $ do
    span_ [class_ $ elementClassM (split & longitudeZodiacSign)] $ do
      maybe mempty asIcon (split & longitudeZodiacSign)
    toHtml $ (" " <> (toText $ longitudeDegrees split)) <> "° "
    toHtml $ (toText $ longitudeMinutes split) <> "\' "
    toHtml $ (toText $ longitudeSeconds split) <> "\""
  where
    pl = getLongitudeRaw p
    split = splitDegreesZodiac pl

htmlDegreesLatitude :: Latitude -> Html ()
htmlDegreesLatitude l =
  span_ [title_ . pack . formatDouble . unLatitude $ l] $ do
    toHtml $ (toText $ longitudeDegrees split) <> "° "
    toHtml $ (toText $ longitudeMinutes split) <> "\' "
    toHtml $ (toText $ longitudeSeconds split) <> "\" "
    toHtml direction
  where
    split = splitDegrees $ unLatitude l
    direction :: Text
    direction = if (unLatitude l) < 0 then "S" else "N"

htmlDegrees :: Double -> Html ()
htmlDegrees = htmlDegrees' (True, True)

htmlDegrees' :: (Bool, Bool) -> Double -> Html ()
htmlDegrees' (includeMinutes, includeSeconds) l =
  span_ [title_ . pack . formatDouble $ l] $ do
    toHtml sign
    toHtml $ (toText $ longitudeDegrees split) <> "° "
    if includeMinutes then
      toHtml $ (toText $ longitudeMinutes split) <> "\' "
    else
      mempty
    if includeSeconds then
      toHtml $ (toText $ longitudeSeconds split) <> "\""
    else
      mempty
  where
    split = splitDegrees l
    sign :: Text
    sign = if l < 0 then "-" else ""

-- TODO: this is just htmlDegrees with a hat!
zodiacLink :: HasLongitude a => a -> Html ()
zodiacLink p =
  a_  [href_ $ "#" <> link'] $ do
    maybe mempty asIcon (split & longitudeZodiacSign)
    toHtml $ (" " <> (toText $ longitudeDegrees split)) <> "° "
    toHtml $ (toText $ longitudeMinutes split) <> "\' "
    toHtml $ (toText $ longitudeSeconds split) <> "\""
  where
    link' = maybe "chart" toText (split & longitudeZodiacSign)
    pl = getLongitudeRaw p
    split = splitDegreesZodiac pl  

planetLink :: Planet -> Html ()
planetLink p =
  a_ [href_ $ "#" <> textLabel] $ do
    toHtml textLabel
  where
    textLabel = p & label & pack

houseLink :: HouseNumber -> Html ()
houseLink h =
  a_ [href_ $ "#house-" <> textLabel] $ do
    houseToAxis h
  where
    textLabel = h & label & pack
  
aspectLink :: AspectName -> Html ()
aspectLink a =
  a_ [href_ $ "#" <> textLabel] $ do
    toHtml textLabel
  where
    textLabel = a & toText

backToChart :: Html  ()
backToChart =
  p_ [] $ do
    a_ [href_ "#chart"] "(Back to chart)" 

housePositionHtml :: Maybe House -> Html ()
housePositionHtml Nothing = mempty
housePositionHtml (Just House {..}) =
  toHtml . toText . (+ 1) . fromEnum $ houseNumber

planetLabel :: Planet -> Html ()
planetLabel MeanNode = toHtml (" Mean Node" :: Text)
planetLabel MeanApog = toHtml (" Lilith" :: Text)
planetLabel p = toHtml . (" " <>) . toText $ p

houseLabel :: HouseNumber -> Html ()
houseLabel I = toHtml (" (Asc)" :: Text)
houseLabel IV = toHtml (" (IC)" :: Text)
houseLabel VII = toHtml (" (Desc)" :: Text)
houseLabel X = toHtml (" (MC)" :: Text)
houseLabel _ = mempty

houseToAxis :: HouseNumber -> Html ()
houseToAxis I = toHtml ("Ascendant"::Text)
houseToAxis X = toHtml ("Midheaven"::Text)
houseToAxis _ = mempty

aspectCell :: Maybe (HoroscopeAspect a b) -> Html ()
aspectCell Nothing = mempty
aspectCell (Just HoroscopeAspect {..}) =
  span_ [aspectColorStyle aspect] $ do
    asIcon . aspectName $ aspect
    " "
    htmlDegrees' (True, False) orb

aspectColor :: AspectTemperament -> Text
aspectColor Analytical = "red"
aspectColor Synthetic = "blue"
aspectColor Neutral = "green"

aspectColorStyle :: Aspect -> Attribute
aspectColorStyle aspect = 
  class_ ("text-" <> (aspectClass . temperament $ aspect))
  where
    aspectClass Analytical = "analytic"
    aspectClass Synthetic  = "synthetic"
    aspectClass Neutral = "neutral"

latLngHtml :: Location -> Html ()
latLngHtml Location {..} =
  toHtml $ " (" <> lnText <> ", " <> ltText <> ")"
  where
    lnSplit = splitDegrees . unLongitude $ locationLongitude
    lnText = pack $ (show $ longitudeDegrees lnSplit) <> (if locationLongitude > 0 then "e" else "w") <> (show $ longitudeMinutes lnSplit)
    ltSplit = splitDegrees . unLatitude $ locationLatitude
    ltText = pack $ (show $ longitudeDegrees ltSplit) <> (if locationLatitude > 0 then "n" else "s") <> (show $ longitudeMinutes ltSplit)

toText :: Show a => a -> Text
toText = pack . show

elementClass :: ZodiacSignName -> Text
elementClass signName = 
  case (zodiacSignElement signName) of
    Nothing -> "text-white"
    Just e -> "text-" <> (toLower . pack . show $ e)

elementClassM :: Maybe ZodiacSignName -> Text
elementClassM =
  maybe "" elementClass
