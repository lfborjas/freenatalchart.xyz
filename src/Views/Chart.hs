{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Views.Chart (render, renderText) where

import Chart.Graphics (renderChart)
import qualified Graphics.Svg as Svg
import Import
import Lucid hiding (renderText)
import RIO.Text (intercalate, justifyLeft, pack, toLower)
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
    (footerNav, metaCeremony)
import Views.Chart.Explanations
    ( attribution,
      generalAspectsExplanation,
      generalHousesExplanation,
      generalPlanetsExplanation,
      generalSignsExplanation,
      Explicable(..), explanationAttribute )
import Text.Printf (printf)
import Ephemeris.Types
import RIO.Writer (MonadWriter(tell), Writer, execWriter)
import RIO.List.Partial (maximum)
import qualified RIO.Text as T

renderText :: a -> BirthData -> HoroscopeData -> Text
renderText _ BirthData {..} HoroscopeData{..}= 
  -- Writer Text ()
  execWriter $ do
    ln_ "Freenatalchart.xyz"
    ln_ "=================="
    ln_ ""
    ln_ "Horoscope for:"
    ln_ . pack $ horoscopeUniversalTime & formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z"
    ln_ $ (birthLocation & locationInput) <> " " <> (latLngTxt birthLocation)
    ln_ ""
    ln_ $ "Sun sign: " <> maybe mempty labelText sunSign
    ln_ $ "Moon: " <> maybe mempty labelText moonSign
    ln_ $ "Ascendant: " <> maybe mempty labelText asc
    ln_ ""
    ln_ "Planet Positions"
    ln_ "----------------"
    -- TODO: justifyLeft?
    ln_ . heading $ 
      [
        justifyPlanetPos "Planet", justifyHouseNum "House",
        justifyLongitude "Longitude", justifyDouble "Speed",
        justifyLatitude "Latitude", justifyDeclination "Declination",
        "Zodiac Longitude"
      ]
    forM_ horoscopePlanetPositions $ \pp@PlanetPosition {..} -> do
      if isRetrograde pp then
        tell . justifyPlanetPos $ (labelText planetName)  <> " (r)"
      else
        tell . justifyPlanetPos $ (labelText planetName)
      tell "|"
      tell . justifyHouseNum $ housePositionText $ housePosition horoscopeHouses planetLng
      tell "|"
      tell . justifyLongitude . pack $ formatDouble $ unLongitude planetLng
      tell "|"
      tell . justifyDouble . pack $ formatDouble $ planetLngSpeed
      tell "|"
      tell . justifyLatitude . pack $ formatDouble $ unLatitude planetLat
      tell "|"
      tell . justifyDeclination . pack $ formatDouble $ planetDeclination
      tell "|"
      tell $ textDegreesZodiac planetLng
      ln_ ""
    ln_ ""
    ln_ $ "House Cusps" <> "( " <> (pack . show $ horoscopeSystem) <> " )"
    ln_ "-----------"
    ln_ . heading $
      [
        justifyHouseNum "House", justifyDouble "Cusp", justifyDeclination "Declination",
        "Zodiac Longitude"
      ] 
    forM_ horoscopeHouses $ \hc@House {..} -> do
      tell . justifyHouseNum . housePositionText $ (Just hc)
      tell "|"
      tell . justifyDouble . pack . formatDouble . unLongitude $ houseCusp
      tell "|"
      tell . justifyDeclination . pack . formatDouble $ houseDeclination
      tell "|"
      tell $ textDegreesZodiac houseCusp
      ln_ ""
    ln_ ""
    ln_ "Planetary Aspects"
    ln_ "-----------------"
    ln_ aspectsHeading
    forM_ horoscopePlanetaryAspects $ \(HoroscopeAspect aspect (aspecting, aspected) angle orb) -> do
      tell . justifyAspecting . labelText . planetName $ aspecting
      tell "|"
      tell . justifyAspect . labelText . aspectName $ aspect
      tell "|"
      tell . justifyAspected . labelText . planetName $ aspected
      tell "|"
      tell . justifyDouble . pack $ formatDouble angle
      tell "|"
      tell . justifyDouble . pack $ formatDouble orb
      ln_ ""
    ln_ ""
    ln_ "Axes Aspects"
    ln_ "-----------------"
    ln_ aspectsHeading
    forM_ horoscopeAngleAspects $ \(HoroscopeAspect aspect (aspecting, aspected) angle orb) -> do
      tell . justifyAspecting . labelText . planetName $ aspecting
      tell "|"
      tell . justifyAspect . labelText . aspectName $ aspect
      tell "|"
      tell . justifyAspected . houseAspectLabel . houseNumber $ aspected
      tell "|"
      tell . justifyDouble . pack $ formatDouble angle
      tell "|"
      tell . justifyDouble . pack $ formatDouble orb
      ln_ ""
  where
    ln_ = tellLine
    sunSign = (findSunSign horoscopePlanetPositions)
    moonSign = (findMoonSign horoscopePlanetPositions)
    asc = (findAscendant horoscopeHouses)
    labelText :: (HasLabel a) => a -> Text
    labelText = pack . label
    houseAspectLabel :: HouseNumber -> Text
    houseAspectLabel I = "Asc"
    houseAspectLabel X = "MC"
    houseAspectLabel h = labelText h
    colWidth :: HasLabel a => [a] -> Int
    colWidth = maximum . map (length . label)
    planetWidth = colWidth defaultPlanets
    doubleWidth = 8
    aspectWidth = colWidth $ map aspectName (majorAspects <> minorAspects)
    justifyPlanetPos = justifyLeft (planetWidth + 4) ' '
    justifyHouseNum  = justifyLeft (length ("House"::String)) ' '
    justifyLongitude = justifyLeft (length ("Longitude"::String)) ' '
    justifyDouble    = justifyLeft doubleWidth ' '
    justifyLatitude  = justifyLeft (length ("Latitude"::String)) ' '
    justifyDeclination = justifyLeft (length ("Declination"::String)) ' '
    heading = intercalate "|"
    justifyAspecting = justifyLeft (maximum [length ("Aspecting"::String), planetWidth]) ' '
    justifyAspected  = justifyLeft (maximum [length ("Aspected"::String), planetWidth]) ' '
    justifyAspect    = justifyLeft (maximum [length ("Aspect"::String), aspectWidth]) ' '
    aspectsHeading = 
      heading [
          justifyAspecting "Aspecting", justifyAspect "Aspect"
        , justifyAspected "Aspected", justifyDouble "Angle"
        , justifyDouble "Orb"
        ]


tellLine :: Text -> (Writer Text ())
tellLine t = tell (t <> "\n")

render :: HasStaticRoot a => a -> BirthData -> HoroscopeData -> Html ()
render renderCtx BirthData {..} h@HoroscopeData {..} = html_ $ do
  head_ $ do
    title_ "Your Natal Chart"
    metaCeremony renderCtx
    style_ $ do
      "svg { height: auto; width: auto}\
      \.table-hover-dark tr:hover{ border-bottom: .05rem solid #9da8ff !important; }\
      \.flex-container{margin-top: 10px; margin-bottom: 10px;}\
      \.light-links a{ color: white !important; }\
      \"

  body_ $ do
    navbar_

    div_ [id_ "main", class_ "container grid-xl mx-4"] $ do
      div_ [id_ "chart", class_ "under-navbar"] $ do
        div_ [class_ "blue-stars-bg text-center", style_ "padding-bottom: 9px"] $ do
          p_ [class_ "text-small text-light"] $ do
            toHtml $ horoscopeUniversalTime & formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z"
            "  ·  "
            latLngHtml birthLocation
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

        ul_ [class_ "tab tab-block tab-block-dark"] $ do
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
            planetPositionsTable horoscopePlanetPositions horoscopeHouses

        div_ [class_ "divider"] ""

        details_ [id_ "house-cusps", class_ "accordion my-2", open_ ""] $ do
          summary_ [class_ "accordion-header "] $ do
            headerIcon
            sectionHeading "House Cusps"
          div_ [class_ "accordion-body scrollable-container"] $ do
            houseSystemDetails horoscopeSystem
            houseCuspsTable horoscopeHouses

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
            aspectDetailsTable horoscopePlanetPositions horoscopePlanetaryAspects horoscopeAngleAspects

        div_ [class_ "divider", id_ "understand"] ""

        details_ [id_ "signs", class_ "accordion my-2", open_ ""] $ do
          summary_ [class_ "accordion-header"] $ do
            headerIcon
            sectionHeading "Zodiac Signs"
          
          div_ [] $ do
            generalSignsExplanation

        divider_ 
        details_ [id_ "houses", class_ "accordion my-2", open_ ""] $ do
          summary_ [class_ "accordion-header"] $ do
            headerIcon
            sectionHeading "Houses"
          div_ [] $ do
            generalHousesExplanation

        divider_ 
        details_ [id_ "planets", class_ "accordion my-2", open_ ""] $ do
          summary_ [class_ "accordion-header"] $ do
            headerIcon
            sectionHeading "Planets"

          div_ [] $ do
            generalPlanetsExplanation

        divider_
        details_ [id_ "aspects", class_ "accordion my-2", open_ ""] $ do
          summary_ [class_ "accordion-header"] $ do
            headerIcon
            sectionHeading "Aspects"

          div_ [] $ do
            generalAspectsExplanation
            h4_ "Orbs we use"
            p_ "All aspects you see in this page are calculated using the following orbs:"
            orbsTable


        divider_ 
        details_ [id_ "references", class_ "accordion my-2"] $ do
          summary_ [class_ "accordion-header"] $ do
            headerIcon
            sectionHeading "References"
          div_ [class_ "accordion-body"] $ do
            attribution

        div_ [class_ "divider", id_ "introspect"] ""
        
        details_ [id_ "my-zodiac", class_ "accordion my-2", open_ ""] $ do
          summary_ [class_ "accordion-header"] $ do
            headerIcon
            sectionHeading "My Zodiac Signs"

          div_ [] $ do  
            zodiacCards horoscopePlanetPositions horoscopeHouses

        divider_
        details_ [id_ "my-houses", class_ "accordion my-2", open_ ""] $ do
          summary_ [class_ "accordion-header"] $ do
            headerIcon
            sectionHeading "My Houses"

          div_ [] $ do
            houseCards horoscopePlanetPositions horoscopeHouses

        divider_
        details_ [id_ "my-planets", class_ "accordion my-2", open_ ""] $ do
          summary_ [class_ "accordion-header"] $ do
            headerIcon
            sectionHeading "My Planets"

          div_ [] $ do
            planetCards horoscopePlanetPositions horoscopeHouses horoscopePlanetaryAspects horoscopeAngleAspects

        divider_
        details_ [id_ "my-major-aspects", class_ "accordion my-2", open_ "" ] $ do
          summary_ [class_ "accordion-header"] $ do
            headerIcon
            sectionHeading "My Major Aspects"

          div_ [] $ do
            forM_ majorAspects $ \a -> do
              aspectDetails' a

        divider_
        details_ [id_ "my-minor-aspects", class_ "accordion my-2", open_ "" ] $ do
          summary_ [class_ "accordion-header"] $ do
            headerIcon
            sectionHeading "My Minor Aspects"

          div_ [] $ do
            forM_ minorAspects  $ \a -> do
              aspectDetails' a

    link_ [rel_ "stylesheet", href_ "https://unpkg.com/spectre.css/dist/spectre-icons.min.css"]
    footerNav

  where
    -- markup helpers
    headerIcon = i_ [class_ "icon icon-arrow-right mr-1 c-hand icon-right icon-light"] ""
    sectionHeading = h5_ [class_ "d-inline text-primary"]
    sunSign = (findSunSign horoscopePlanetPositions)
    moonSign = (findMoonSign horoscopePlanetPositions)
    asc = (findAscendant horoscopeHouses)
    aspectDetails' = aspectDetails horoscopePlanetaryAspects horoscopeAngleAspects
    divider_ = div_ [class_ "divider"] ""

--
-- "COMPONENTS"
--
navbar_ :: Html ()
navbar_ =
  header_ [class_ "navbar bg-dark navbar-fixed navbar-fixed-top"] $ do
    section_ [class_ "navbar-section"] $ do
      a_ [href_ "/", class_ "mr-2"] $ do
        i_ [class_ "icon icon-refresh", title_ "Draw Another Chart"] ""
        span_ [class_ "hide-sm"] " Draw Another Chart"
    section_ [class_ "navbar-section navbar-center navbar-brand"] $ do
       a_ [href_ "/", class_ "brand-text"] "FreeNatalChart.xyz"
    section_ [class_ "navbar-section"] $ do
      a_ [href_ "#chart"] $ do
        span_ [class_ "hide-sm"] "Back to Top "
        i_ [class_ "icon icon-upward", title_ "Back to Top"] ""

planetPositionsTable :: [PlanetPosition] -> [House] ->  Html ()
planetPositionsTable planetPositions houses =
  table_ [class_ "table table-no-borders table-hover-dark"] $ do
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
      forM_ (planetPositions) $ \pp@PlanetPosition {..} -> do
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
            housePositionHtml $ housePosition houses planetLng 
          td_ $ do
            htmlDegreesZodiac planetLng 
          td_ $ do
            htmlDegrees planetLngSpeed  
          td_ $ do
            htmlDegreesLatitude planetLat 
          td_ $ do
            htmlDegreesLatitude $ Latitude planetDeclination  

houseSystemDetails :: HouseSystem -> Html ()
houseSystemDetails sys =
  p_ $ do
    "System Used: "
    span_ [class_ "text-primary"] $ toHtml $ toText sys
    " (to learn more about house systems and the meaning of each house, see the "
    a_ [href_ "#houses"] "Houses"
    " section.)"

houseCuspsTable :: [House] -> Html ()
houseCuspsTable houses =
  table_ [class_ "table table-no-borders table-hover-dark"] $ do
    thead_ [class_ "text-light"] $ do
      tr_ [] $ do
        th_ [] "House"
        th_ [] "Cusp"
        th_ [] "Declination"
    tbody_ [] $ do
      forM_ (houses) $ \hc@House {..} -> do
        tr_ [] $ do
          td_ $ do
            housePositionHtml (Just hc)
            houseLabel houseNumber
          td_ $ do
            htmlDegreesZodiac houseCusp
          td_ $ do
            htmlDegreesLatitude $ Latitude houseDeclination

aspectDetailsTable :: [PlanetPosition] -> [PlanetaryAspect] -> [AngleAspect] -> Html () 
aspectDetailsTable planetPositions planetAspects angleAspects =
  table_ [class_ "table table-scroll"] $ do
    forM_ defaultPlanets $ \rowPlanet -> do
      tr_ [] $ do
        td_ [] $ do
          if rowPlanet == Sun
            then mempty
            else asIcon rowPlanet
        forM_ (takeWhile (not . (== rowPlanet) . planetName) planetPositions) $ \PlanetPosition {..} -> do
          td_ [style_ "border: 1px solid", class_ "text-small"] $ do
            aspectCell $ findAspectBetweenPlanets planetAspects rowPlanet planetName
        td_ [style_ "border-bottom: 1px solid"] $ do
          asIcon rowPlanet
    tr_ [] $ do
      td_ [] $ do
        span_ [class_ "tooltip", data_ "tooltip" "Ascendant"] "AC"
      forM_ (planetPositions) $ \PlanetPosition {..} -> do
        td_ [style_ "border: 1px solid", class_ "text-small"] $ do
          aspectCell $ findAspectWithAngle angleAspects planetName I
    tr_ [] $ do
      td_ [] $ do
        span_ [class_ "tooltip", data_ "tooltip" "Midheaven"] "MC"
      forM_ (planetPositions) $ \PlanetPosition {..} -> do
        td_ [style_ "border: 1px solid", class_ "text-small"] $ do
          aspectCell $ findAspectWithAngle angleAspects planetName X

orbsTable :: Html ()
orbsTable =
  table_ [id_ "orbs-used", class_ "table table-no-borders"] $ do
   thead_ [] $ do
     tr_ [] $ do
       th_ "Aspect"
       th_ "Angle"
       th_ "Orb"
   tbody_ [] $ do
     forM_ (majorAspects <> minorAspects) $ \a@Aspect {..} -> do
       tr_ [] $ do
         td_ $ do
           span_ [aspectColorStyle a] $ do
             asIcon aspectName
           " "
           toHtml $ toText aspectName
         td_ $ do
           toHtml $ toText angle
         td_ $ do
           toHtml $ toText maxOrb


zodiacCards :: [PlanetPosition] -> [House] -> Html ()
zodiacCards planetPositions houseCusps =
  forM_ [Aries .. Pisces] $ \zodiacSign -> do
    div_ [cardDark_] $ do
      div_ [class_ "card-header"] $ do
        div_ [class_ "card-title"] $ do
          h4_ [id_ (toText zodiacSign), class_ $ elementClass zodiacSign] $ do
            asIcon zodiacSign
            " "
            toHtml . toText $ zodiacSign  
      div_ [class_ "card-body"] $ do
        div_ [class_ "flex-container"] $ do
          div_ [class_ "flex-item"] $ do
            attributeTitle_ "Strengths"
            span_ [class_ "text-italic"] $ do
              explanationAttribute zodiacSign "Strengths" 
          div_ [class_ "flex-item"] $ do
            attributeTitle_ "Weaknesses"
            span_ [class_ "text-italic"] $ do
              explanationAttribute zodiacSign "Weaknesses"  
        div_ [class_ "flex-container"] $ do
          div_ [class_ "flex-item"] $ do
            attributeTitle_ "Element"
            span_ [class_ $ "text-large " <> (elementClass zodiacSign)] $ do
              explanationAttribute zodiacSign "Element" 
          div_ [class_ "flex-item"] $ do
            attributeTitle_ "Quality"
            span_ [class_ "text-large"] $ do
              explanationAttribute zodiacSign "Quality" 
          div_ [class_ "flex-item"] $ do
            attributeTitle_ "Ruler"
            span_ [class_ "text-large"] $ do
              explanationAttribute zodiacSign "Ruler" 
        div_ [class_ "flex-container"] $ do
          div_ [class_ "flex-item"] $ do
            attributeTitle_ "Related House"
            span_ [class_ "text-large"] $ do
              explanationAttribute zodiacSign "Related house" 
          div_ [class_ "flex-item"] $ do
            attributeTitle_ "Motto"
            span_ [class_ "text-large text-quoted"] $ do
              explanationAttribute zodiacSign "Motto" 
        div_ [class_ "divider divider-dark"] "" 
        attributeTitle_ ("My Planets in " <> (toHtml . toText $ zodiacSign))
        let
          planets' = planetsInSign' zodiacSign
          in do
            if null planets' then
              p_ $ do
                em_ "Your chart doesn't have any planets in this sign."
            else
              table_ [class_ "table table-no-borders table-hover-dark text-center"] $ do
                tbody_ [] $ do
                  forM_ planets' $ \p -> do
                    planetDetails p 
        attributeTitle_ ("My Houses in " <> (toHtml . toText $ zodiacSign))
        let
          houses' = housesInSign' zodiacSign
          in do
            if null houses' then
              p_ $ do 
                em_ "Your chart doesn't have any house cusps in this sign."
              else
                table_ [class_ "table table-no-borders table-hover-dark text-center"] $ do
                  tbody_ [] $ do
                    forM_ houses' $ \hs -> do
                      houseDetails hs
  where
    planetsBySign' = planetsBySign planetPositions
    planetsInSign' = planetsInSign planetsBySign'
    housesBySign'  = housesBySign houseCusps
    housesInSign'  = housesInSign housesBySign'                   

houseCards :: [PlanetPosition] -> [House] -> Html ()
houseCards planetPositions houseCusps =
  forM_ houseCusps $ \huis@House{..} -> do
    div_ [cardDark_] $ do
      div_ [class_ "card-header"] $ do
        div_ [class_ "card-title"] $ do
          h4_ [id_ $ "house-" <> toText houseNumber] $ do
            span_ [class_ "text-light"] $ do 
              toHtml $ toText houseNumber
              ". "
            explanationAttribute houseNumber "Alias"  
      div_ [class_ "card-body"] $ do
        p_ [class_ "text-italic"] $ do
          explanationAttribute houseNumber "Keywords" 
        p_ [] $ do
          explain houseNumber 
        attributeTitle_ $ do
          explanationAttribute houseNumber "Quadrant"
          " Quadrant" 
        p_ [] $ do
          explanationAttribute houseNumber "LatitudeHemisphere"
          explanationAttribute houseNumber "LongitudeHemisphere"  
        div_ [class_ "divider divider-dark"] "" 
        attributeTitle_ ("My House " <> (toHtml . toText $ houseNumber) <> " Cusp")
        span_ [class_ "text-large"] $ do
          zodiacLink' True huis 
        attributeTitle_ ("My Planets in House " <> (toHtml . toText $ houseNumber))
        let
          planets' = planetsInHouse' huis
          in do
            if null planets' then
              p_ $ do
                em_ "Your chart doesn't have any planets in this house."
            else
              table_ [class_ "table table-no-borders table-hover-dark text-center"] $ do
                tbody_ [] $ do
                  forM_ planets' $ \p -> do
                    planetDetails p
  where
    planetsByHouse' = planetsByHouse houseCusps planetPositions
    planetsInHouse' = planetsInHouse planetsByHouse'

planetCards :: [PlanetPosition] -> [House] -> [PlanetaryAspect] -> [AngleAspect] -> Html ()
planetCards planetPositions houseCusps planetaryAspects angleAspects =
  forM_ planetPositions $ \p -> do
    div_ [cardDark_] $ do
      div_ [class_ "card-header"] $ do
        div_ [class_ "card-title"] $ do
          h4_ [id_ $ pack . label . planetName $ p] $ do
            asIcon . planetName $ p
            " "
            toHtml . label . planetName $ p
        if (isRetrograde p) then
          div_ [class_ "card-subtitle"] $ do
            span_ [] "(retrograde)"
        else
          mempty  
      div_ [class_ "card-body"] $ do
        p_ [class_ "text-italic"] $ do
          explanationAttribute (planetName p) "Keywords"
        div_ [class_ "flex-container"] $ do
          if (p & planetName & (flip hasAttribute) "Group") then
            div_ [class_ "flex-item"] $ do
              attributeTitle_ "Group"
              span_ [class_ "text-large"] $ do
                explanationAttribute (planetName p) "Group"
          else
            mempty
          if (p & planetName & (flip hasAttribute) "Rulership") then
            div_ [class_ "flex-item"] $ do
              attributeTitle_ "Rulership"
              span_ [class_ "text-large"] $ do
                explanationAttribute (planetName p) "Rulership" 
          else
            mempty
        div_ [class_ "divider divider-dark"] "" 
        div_ [class_ "flex-container"] $ do
          div_ [class_ "flex-item"] $ do
            attributeTitle_ "House"
            span_ [class_ "text-large"] $ do
              maybe mempty (houseLinkFull . houseNumber) (housePosition' . planetLng $ p)
          div_ [class_ "flex-item"] $ do
            attributeTitle_ "Position"
            span_ [class_ "text-large"] $ do
              (zodiacLink' True) . planetLng $ p  
        attributeTitle_ ("My Aspects to " <> (toHtml . label . planetName $ p))
        let
          aspects' = p & planetName & aspectsForPlanet' & catMaybes
          axes' = p & planetName & axesAspectsForPlanet' & catMaybes
          in do
            if (null aspects' && null axes') then
              p_ $ do
                em_ "This planet is unaspected. Note that not having any aspects is rare, which means this planet's sole influence can be quite significant."
            else
              aspectsTable' (Just p) aspects' axes'   
  where
    housePosition'  = housePosition houseCusps
    aspectsForPlanet' p = map (findAspectBetweenPlanets planetaryAspects p) [Sun .. Chiron]
    axesAspectsForPlanet' p = map (findAspectWithAngle angleAspects p)  [I, X]

cardDark_ :: Attribute
cardDark_ = class_ "card card-dark mx-2 my-2 text-center"
attributeTitle_ :: Html () -> Html ()
attributeTitle_ = h5_ [class_ "text-light"] 

aspectDetails :: [HoroscopeAspect PlanetPosition PlanetPosition] -> [HoroscopeAspect PlanetPosition House] -> Aspect -> Html ()
aspectDetails allPlanetAspects allAxesAspects a@Aspect {..} = do
  div_ [cardDark_] $ do
    div_ [class_ "card-header"] $ do
      div_ [class_ "card-title"] $ do
        h4_ [id_ $ toText aspectName] $ do
          asIcon aspectName
          " "
          toHtml . toText $ aspectName
    div_ [class_ "card-body"] $ do
      p_ [class_ "text-small"] $ do
        explain aspectName

      div_ [class_ "flex-container"] $ do
        div_ [class_ "flex-item"] $ do
          attributeTitle_ "Classification"
          span_ [class_ "text-large"] $ do
            toHtml . toText $ aspectType
        div_ [class_ "flex-item"] $ do
          attributeTitle_ "Temperament"
          span_ [class_ "text-large"] $ do
            span_ [aspectColorStyle a] $ do
              toHtml . toText $ temperament

      div_ [class_ "flex-container"] $ do
        div_ [class_ "flex-item"] $ do
          attributeTitle_ "Angle"
          span_ [class_ "text-large"] $ do
            toHtml . toText $ angle
        div_ [class_ "flex-item"] $ do
          attributeTitle_ "Orb Used"
          span_ [class_ "text-large"] $ do
            toHtml . toText $ maxOrb

      attributeTitle_ . toHtml $ (toText aspectName) <> "s in your chart:"
      if (null planetAspects && null axesAspects) then
        em_ . toHtml $ "No " <> (toText aspectName) <> "s appear in your chart."
      else
        aspectsTable planetAspects axesAspects
      where
        planetAspects = findAspectsByName allPlanetAspects aspectName
        axesAspects = findAspectsByName allAxesAspects aspectName


aspectsTable' :: Maybe PlanetPosition -> [PlanetaryAspect] -> [AngleAspect] -> Html ()
aspectsTable' aspectedPosition aspects' axes'= do
  table_ [class_ "table table-no-borders table-hover-dark text-center"] $ do
    tbody_ $ do
      forM_ aspects' $ \pa -> do
        planetAspectDetails aspectedPosition pa  
      forM_ axes' $ \aa -> do
        axisAspectDetails aa

aspectsTable :: [PlanetaryAspect] -> [AngleAspect] -> Html ()
aspectsTable = aspectsTable' Nothing 

planetAspectDetails :: (Maybe PlanetPosition) -> PlanetaryAspect  -> Html ()
planetAspectDetails aspectedPosition HoroscopeAspect{..} = do
  tr_ [class_ "light-links"] $ do
    td_ [] $ do
      span_ [class_ "text-light"] $ do
        first' & planetName & asIcon
      " "
      first' & planetName & planetLink
    td_ [] $ do
      span_ [aspectColorStyle aspect] $ do
        aspect & aspectName & asIcon
      " "
      aspect & aspectName & aspectLink
    td_ [] $ do
      span_ [class_ "text-light"] $ do
        second' & planetName & asIcon
      " "
      second' & planetName & planetLink
    td_ [] $ do
      "with orb "
      htmlDegrees' (True, True) orb
  where
    (first', second') = 
      case aspectedPosition of
        Nothing -> (bodies & fst, bodies & snd)
        Just aspected -> 
          if ((bodies & fst) == aspected) then
            (bodies & fst, bodies & snd)
          else
            (bodies & snd, bodies & fst)

axisAspectDetails :: (HoroscopeAspect PlanetPosition House) -> Html ()
axisAspectDetails HoroscopeAspect{..} = do
  tr_ [class_ "light-links"] $ do
    td_ [] $ do
      span_ [class_ "text-light"] $ do
        bodies & fst & planetName & asIcon
        " "
      bodies & fst & planetName & planetLink
    td_ [] $ do
      span_ [aspectColorStyle aspect] $ do
        aspect & aspectName & asIcon
      " "
      aspect & aspectName & aspectLink
    td_ [] $ do
      bodies & snd & houseNumber & asIcon
      bodies & snd & houseNumber & houseLinkFull
    td_ [] $ do
      "orb: "
      htmlDegrees' (True, True) orb

planetDetails :: PlanetPosition -> Html ()
planetDetails PlanetPosition{..} = tr_ [] $ do
  td_ [] $ do
    asIcon planetName
    a_ [href_ $ "#" <> (pack . label) planetName] $ do
      planetLabel planetName
  td_ [] $ do
    "located in "
    zodiacLink planetLng

houseDetails :: House -> Html ()
houseDetails House{..} =
  tr_ [] $ do
    td_ [] $ do
      a_ [href_ $ "#house-" <> toText houseNumber] $ do
        toHtml $ "House " <> toText houseNumber
        houseLabel houseNumber
    td_ [] $ do
      "with cusp in "
      zodiacLink houseCusp

---
--- HELPERS
---


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

textDegreesZodiac :: HasLongitude a => a -> Text
textDegreesZodiac p =
  T.concat [
    maybe mempty asEmoji (split & longitudeZodiacSign)
    , ((" " <> (toText $ longitudeDegrees split)) <> "° ")
    , ((toText $ longitudeMinutes split) <> "\' ")
    , ((toText $ longitudeSeconds split) <> "\"")
  ]
  where
    pl = getLongitudeRaw p
    split = splitDegreesZodiac pl
    asEmoji :: ZodiacSignName -> Text
    asEmoji z =
      case z of
        Aries ->
            "♈️"

        Taurus ->
            "♉️"

        Gemini ->
            "♊️"

        Cancer ->
            "♋️"

        Leo ->
            "♌️"

        Virgo ->
            "♍️"

        Libra ->
            "♎️"

        Scorpio ->
            "♏️"

        Sagittarius ->
            "♐️"

        Capricorn ->
            "♑️"

        Aquarius ->
            "♒️"

        Pisces ->
            "♓️"

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

latLngTxt :: Location -> Text
latLngTxt Location {..} =
  " (" <> lnText <> ", " <> ltText <> ")"
  where
    lnSplit = splitDegrees . unLongitude $ locationLongitude
    lnText = pack $ (show $ longitudeDegrees lnSplit) <> (if locationLongitude > 0 then "e" else "w") <> (show $ longitudeMinutes lnSplit)
    ltSplit = splitDegrees . unLatitude $ locationLatitude
    ltText = pack $ (show $ longitudeDegrees ltSplit) <> (if locationLatitude > 0 then "n" else "s") <> (show $ longitudeMinutes ltSplit)

latLngHtml :: Location -> Html ()
latLngHtml = toHtml . latLngTxt

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
zodiacLink' :: HasLongitude a => Bool -> a -> Html ()
zodiacLink' elementColor p =
  a_  [href_ $ "#" <> link', class_ colorClass] $ do
    maybe mempty asIcon (split & longitudeZodiacSign)
    toHtml $ (" " <> (toText $ longitudeDegrees split)) <> "° "
    toHtml $ (toText $ longitudeMinutes split) <> "\' "
    toHtml $ (toText $ longitudeSeconds split) <> "\""
  where
    link' = maybe "chart" toText (split & longitudeZodiacSign)
    pl = getLongitudeRaw p
    split = splitDegreesZodiac pl
    sign = split & longitudeZodiacSign
    colorClass = 
      if elementColor then
        elementClassM sign
      else
        mempty

zodiacLink :: HasLongitude a => a -> Html ()
zodiacLink = zodiacLink' False

planetLink :: Planet -> Html ()
planetLink p =
  a_ [href_ $ "#" <> textLabel] $ do
    toHtml textLabel
  where
    textLabel = p & label & pack
  
houseLinkFull :: HouseNumber -> Html ()
houseLinkFull houseNumber =
  a_ [href_ $ "#house-" <> toText houseNumber] $ do
    toHtml $ "House " <> toText houseNumber
    houseLabel houseNumber

aspectLink :: AspectName -> Html ()
aspectLink a =
  a_ [href_ $ "#" <> textLabel] $ do
    toHtml textLabel
  where
    textLabel = a & toText 

housePositionText :: Maybe House -> Text 
housePositionText Nothing = mempty
housePositionText (Just House {..}) =
   toText . (+ 1) . fromEnum $ houseNumber

housePositionHtml :: Maybe House -> Html ()
housePositionHtml = toHtml . housePositionText

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

aspectCell :: Maybe (HoroscopeAspect a b) -> Html ()
aspectCell Nothing = mempty
aspectCell (Just HoroscopeAspect {..}) =
  span_ [aspectColorStyle aspect] $ do
    asIcon . aspectName $ aspect
    " "
    htmlDegrees' (True, False) orb

aspectColorStyle :: Aspect -> Attribute
aspectColorStyle aspect = 
  class_ ("text-" <> (aspectClass . temperament $ aspect))
  where
    aspectClass Analytical = "analytic"
    aspectClass Synthetic  = "synthetic"
    aspectClass Neutral = "neutral"

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
