{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Views.Transits (render, renderText) where

import Import
import RIO.Time (rfc822DateFormat, defaultTimeLocale, formatTime, UTCTime)
import Ephemeris
import Lucid hiding (renderText)
import Views.Chart.Common
import RIO.Writer (Writer, MonadWriter(tell), execWriter)
import RIO.Text (justifyLeft, pack)
import Data.Foldable (Foldable(maximum))
import RIO.List (sort, repeat, headMaybe)
import qualified RIO.Text as T
import Views.Common
import qualified Graphics.Svg as Svg
import Chart.Graphics (renderTransitChart)
import Data.Time.Format.ISO8601 (iso8601Show)
import Views.Chart.Explanations (Explicable(explanationAttribute, explain), generalTransitsExplanation)


renderText :: a -> BirthData -> UTCTime -> TransitData -> Text
renderText _ BirthData {..} transitMoment TransitData {..} =
  execWriter $ do
    ln_ "Freenatalchart.xyz"
    ln_ "=================="
    ln_ "Transits for:"
    ln_ . pack $ transitMoment & formatUTCTimestamp
    ln_ $ (birthLocation & locationInput) <> " " <> (latLngTxt birthLocation)
    ln_ ""
    ln_ "Natal Planet Positions"
    ln_ "----------------------"
    ln_ . heading $ 
      [
        justifyPlanetPos "Planet", justifyHouseNum "House",
        justifyLongitude "Longitude", justifyDouble "Speed",
        justifyLatitude "Latitude", justifyDeclination "Declination",
        "Zodiac Longitude"
      ]
    forM_ natalPlanetPositions $ \np@PlanetPosition{..} -> do
      if isRetrograde np then
        tell . justifyPlanetPos $ (labelText planetName)  <> " (r)"
      else
        tell . justifyPlanetPos $ (labelText planetName)
      tell "|"
      tell . justifyHouseNum $ housePositionText $ housePosition natalHouses planetLng
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
    ln_ "Transiting Planet Positions"
    ln_ "---------------------------"
    ln_ . heading $ 
      [
        justifyPlanetPos "Planet", justifyHouseNum "House",
        justifyLongitude "Longitude", justifyDouble "Speed",
        justifyLatitude "Latitude", justifyDeclination "Declination",
        "Zodiac Longitude"
      ]
    forM_ transitingPlanetPositions $ \tp@PlanetPosition{..} -> do
      if isRetrograde tp then
        tell . justifyPlanetPos $ (labelText planetName)  <> " (r)"
      else
        tell . justifyPlanetPos $ (labelText planetName)
      tell "|"
      -- NOTE(luis) this is intentional: we want to show which natal house a transiting
      -- planet is in.
      tell . justifyHouseNum $ housePositionText $ housePosition natalHouses planetLng
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
    ln_ "Planetary Transits"
    ln_ "------------------"
    transitActivity "to Natal Planets" transitMoment planetaryTransits
    ln_ "Axes Transits"
    ln_ "-------------"
    transitActivity "to Natal Axes" transitMoment angleTransits

aspectsHeading :: Text
aspectsHeading = 
  heading [
      justifyAspecting "Aspecting", justifyAspect "Aspect"
    , justifyAspected "Aspected", justifyDouble "Angle"
    , justifyDouble "Orb", "Phase"
    ]

transitActivity :: (HasLabel a, HasLongitude a) => Text -> UTCTime -> [(TransitAspect a, Transit a)] -> (Writer Text ())
transitActivity extraHeading moment transits' = do
  ln_ ""
  ln_ $ "All Aspects " <> extraHeading
  ln_ $ "~~~~~~~~~~~~" <> pack (repeat '~' & take (T.length extraHeading))
  ln_ aspectsHeading
  forM_  (transitAspects transits') $ \a@(HoroscopeAspect aspect (aspecting, aspected) angle orb) -> do
    tell . justifyAspecting . labelText . planetName $ aspecting
    tell "|"
    tell . justifyAspect . labelText . aspectName $ aspect
    tell "|"
    tell . justifyAspected . labelText  $ aspected
    tell "|"
    tell . justifyDouble . pack $ formatDouble angle
    tell "|"
    tell . justifyDouble . pack $ formatDouble orb
    tell "|"
    tell . pack . show $ aspectPhase a
    ln_ ""
  ln_ ""
  ln_ $ "Active Transits " <> extraHeading
  ln_ $ "~~~~~~~~~~~~~~~~" <> pack (repeat '~' & take (T.length extraHeading))
  ln_ . heading $
    [
      justifyTransiting "Transiting", justifyAspect "Aspect", justifyTransited "Transited",
      justifyTimestamp "Starts", justifyTimestamp "Ends",
      justifyTimestamp "Exact At"
    ]
  forM_ (transitActivityAround moment transits') $ \(a,t) -> do
    tell . justifyTransiting . labelText . planetName . transiting $ t
    tell "|"
    tell . justifyAspect . labelText . aspectName . aspect $ a
    tell "|"
    tell . justifyTransited . labelText . transited $ t
    tell "|"
    tell . justifyTimestamp . pack . (maybe "N/A" formatUTCTimestamp) . transitStarts $ t
    tell "|"
    tell . justifyTimestamp . pack . (maybe "N/A" formatUTCTimestamp) . transitEnds $ t
    tell "|"
    tell . justifyTimestamp . pack . (maybe "N/A" formatUTCTimestamp) . headMaybe . immediateTriggers  $ t
    ln_ ""
  ln_ ""
  where
    justifyPlanetOrString :: String -> (Text -> Text)
    justifyPlanetOrString s = justifyLeft (maximum [length s, planetWidth]) ' '
    justifyTransiting = justifyPlanetOrString "Transiting"
    justifyTransited  = justifyPlanetOrString "Transited"
    justifyTimestamp  = justifyLeft 24 ' '


render :: HasStaticRoot a => a -> BirthData -> UTCTime -> TransitData -> Html ()
render renderCtx bd@BirthData {..} transitMoment t@TransitData{..} = html_ $ do
  head_ $ do
    title_ "Your Current Transits"
    metaCeremony renderCtx
    style_ $ do
      "svg { height: auto; width: auto}\
      \.section{ border-top: .05rem solid #3c4feb; }\
      \"

  body_ $ do
    navbar_

    main_ [id_ "main", class_ "container grid-xl mx-4 under-navbar"] $ do
      div_ [class_ "blue-stars-bg text-center"] $ do
        p_ [class_ "text-small text-light"] $ do
          a_ [href_ $ natalChartLink bd] $ do
            toHtml $ birthLocalTime & formatTime defaultTimeLocale rfc822DateFormat
            br_ []
            toHtml $ birthLocation & locationInput
        p_ $ do
          "Transits as of: "
          localTime_ transitMoment

      figure_ [class_ "figure p-centered my-2", style_ "max-width: 600px;"] $ do
        div_ [] $ do
          (toHtmlRaw $ Svg.renderBS $ renderTransitChart [Svg.makeAttribute "height" "not", Svg.makeAttribute "width" "not"] 600 t)

      ul_ [class_ "tab tab-block tab-block-dark"] $ do
        li_ [class_ "tab-item active"] $ do
          a_ [href_ "#analyze"] "Analyze"
        li_ [class_ "tab-item"] $ do
          a_ [href_ "#understand"] "Understand"
        li_ [class_ "tab-item"] $ do
          a_ [href_ "#introspect"] "Introspect"

      article_ [id_ "analyze"] $ do
        section_ [class_ "section"] $ do
          details_ [id_ "natal-planet-positions", class_ "accordion my-2", open_ ""] $ do
            summary_ [class_ "accordion-header"] $ do
              headerIcon
              sectionHeading $ do
                "Natal Planet Positions"      
            div_ [class_ "accordion-body scrollable-container"] $ do
              planetPositionsTable natalPlanetPositions natalHouses

        section_ [class_ "section"] $ do
          details_ [id_ "transiting-planet-positions", class_ "accordion my-2", open_ ""] $ do
            summary_ [class_ "accordion-header"] $ do
              headerIcon
              sectionHeading $ do
                "Transiting Planet Positions"      
            div_ [class_ "accordion-body scrollable-container"] $ do
              planetPositionsTable transitingPlanetPositions natalHouses
        section_ [class_ "section"] $ do
          details_ [id_ "aspects-summary", class_ "accordion my-2", open_ ""] $ do
            summary_ [class_ "accordion-header"] $ do
              headerIcon
              sectionHeading "Aspects Summary"
            div_ [class_ "accordion-body scrollable-container"] $ do
              transitAspectDetailsTable transitingPlanetPositions planetaryTransits angleTransits

      article_ [id_ "understand"] $ do
        section_ [class_ "section"] $ do
          details_ [id_ "transits-explanation", class_ "accordion my-2", open_ ""] $ do
            summary_ [class_ "accordion-header"] $ do
              headerIcon
              sectionHeading "Transits"
            div_ [] $ do
              generalTransitsExplanation 
              h4_ "Orbs we use"
              p_ "All aspects you see in the summary table are calculated using the following orbs: "
              orbsTable aspectsForTransits

      article_ [id_ "introspect"] $ do
        section_ [class_ "section"] $ do
          details_ [id_ "my-active-transits", class_ "accordion my-2", open_ ""] $ do
            summary_ [class_ "accordion-header"] $ do
              headerIcon
              sectionHeading "Your Active Transits"
            div_ [] $ do
              transitCards planetaryActivity
              transitCards angleActivity

              div_ [class_ "divider"] ""
              nav_ [id_ "active-transit-list", class_ "light-links"] $ do
                h5_ "Transit Index"
                ul_ [class_ "nav text-small"] $ do
                  transitSummaries planetaryActivity
                  transitSummaries angleActivity

    link_ [rel_ "stylesheet", href_ "https://unpkg.com/spectre.css/dist/spectre-icons.min.css"]
    footerNav
    script_ [src_ . pack $ (renderCtx ^. staticRootL) <> "js/date.js"] (""::Text)
  where
    planetaryActivity = transitActivityAround transitMoment planetaryTransits
    angleActivity     = transitActivityAround transitMoment angleTransits

transitAspectDetailsTable :: [PlanetPosition] -> [(PlanetaryAspect, PlanetaryTransit)] -> [(AngleAspect, AngleTransit)] -> Html ()
transitAspectDetailsTable  transitingPlanets planetTransits angleTransits =
  table_ [class_ "table table-hover-dark table-scroll table-borders-dark"] $ do
    tr_ [class_ "text-primary" ]$ do
      td_ [style_ "border-bottom-style: none !important;"] ""
      forM_ (defaultPlanets & sort) $ \natalPlanet -> do
        td_ [] $ do
          asIcon natalPlanet
          " (nat)"
      td_ [] "Asc (nat)"
      td_ [] "MC (nat)"
    forM_ (transitingPlanets & map planetName & sort) $ \transitingPlanet -> do
      tr_ [] $ do
        td_ [class_ "text-earth", style_ "border-bottom-style: none !important;"] $ do
          asIcon transitingPlanet
          " (tr)"
        forM_ (defaultPlanets & sort) $ \transitedPlanet -> do
          td_ [style_ "border: 1px solid", class_ "text-small"] $ do
            planetaryAspectCell $ findAspectWithPlanet (transitAspects planetTransits) transitingPlanet transitedPlanet

        td_ [style_ "border: 1px solid", class_ "text-small"] $ do
          planetaryAspectCell $ findAspectWithAngle (transitAspects angleTransits) transitingPlanet I
  
        td_ [style_ "border: 1px solid", class_ "text-small"] $ do
          planetaryAspectCell $ findAspectWithAngle (transitAspects angleTransits) transitingPlanet X

transitSummaries ::  (HasLabel a) => [(TransitAspect a, Transit a)] -> Html ()
transitSummaries activity =
  forM_ activity $ \a@(activeTransit, activityData) ->
    li_ [class_ "nav-item"] $ do
      a_ [href_ $ "#" <> (transitId a)] $ do
        span_ [activeTransit & aspect & aspectColorStyle] $ do
          activityData & transiting & asIcon'
          " "
          activeTransit & aspect & aspectName & asIcon
          " "
          activityData & transited & asIcon'   
          " "
        span_ [] $ do
          activityData & transiting & labelText & toHtml
          " "
          strong_ $ do
            activeTransit & aspect & aspectName & labelText & toHtml
          " "
          activityData & transited & labelText & toHtml
        span_ [class_ "text-italic"] $ do
          if (activityData & immediateTriggers & null & not) then do
            " (exact: "
            maybe mempty localTime_ (activityData & immediateTriggers & headMaybe)
            ") "
          else
            mempty

transitCards :: (HasLongitude a, HasLabel a, Explicable a) => [(TransitAspect a, Transit a)] -> Html ()
transitCards activity =
  forM_ activity $ \a@(activeTransit, activityData) ->
    div_ [id_ (transitId a), cardDark_] $ do
      div_ [class_ "card-header"] $ do
        div_ [class_ "card-title"] $ do
          h4_ [activeTransit & aspect & aspectColorStyle] $ do
            activityData & transiting & asIcon'
            " "
            activeTransit & aspect & aspectName & asIcon
            " "
            activityData & transited & asIcon'
          div_ [class_ "card-subtitle"] $ do
            "Transiting "
            activityData & transiting & labelText & toHtml
            " "
            strong_ $ do
              activeTransit & aspect & aspectName & labelText & toHtml
            " Natal "
            activityData & transited & labelText & toHtml
      div_ [class_ "card-body"] $ do
        p_ [class_ "text-small"] $ do
          a_ [href_ "#active-transit-list"] "Back to list"

        div_ [class_ "text-small"] $ do
          attributeTitle_ (activeTransit & aspect & aspectName & labelText & toHtml)
          p_ $ do
            activeTransit & aspect & aspectName & explain

          attributeTitle_ (activityData & transiting & labelText & toHtml)
          p_ $ do
            explanationAttribute (activityData & transiting) "Keywords"

          -- don't show keywords for transited if they're the same!
          if ((activityData & transiting & labelText) == (activityData & transited & labelText)) then
            mempty
          else do
            attributeTitle_ (activityData & transited & labelText & toHtml)
            p_ $ do
              explanationAttribute (activityData & transited) "Keywords"


        div_ [class_ "flex-container"] $ do
          if (activityData & transitStarts & isJust) then
            div_ [class_ "flex-item"] $ do
              attributeTitle_ "Starts"
              span_ [class_ "text-medium"] $ do
                maybe mempty localDate_ (activityData & transitStarts)
          else
            mempty 

          if (activityData & transitEnds & isJust) then
            div_ [class_ "flex-item"] $ do
              attributeTitle_  $ do
                if (activityData & transitStarts & isNothing ) then
                  "Active During"
                else
                  "Ends"
              span_ [class_ "text-medium"] $ do
                maybe mempty localDate_ (activityData & transitEnds)
          else
            mempty
        
        if (activityData & immediateTriggers & null & not) then
          div_ [class_ "flex-container"] $ do
            div_ [class_ "flex-item"] $ do
              attributeTitle_ "Exact At"
              span_ [class_ "text-large"] $ do
                maybe mempty localTime_ (activityData & immediateTriggers & headMaybe)
        else
          mempty

        div_ [class_ "divider divider-dark"] ""

        attributeTitle_ "In this transit"
        table_ [class_ "table table-no-borders table-hover-dark text-center"] $ do
          tbody_ [] $ do
            (bodyDetails "Transiting") . transiting $ activityData
            (bodyDetails "Natal") . transited $ activityData

transitId :: HasLabel a => (TransitAspect a, Transit a) -> Text
transitId (activeTransit, activityData) =
  (activityData & transiting & labelText) <>
  (activeTransit & aspect & aspectName & labelText) <>
  (activityData & transited & labelText)

bodyDetails :: (HasLabel a, HasLongitude a) => Text -> a -> Html ()
bodyDetails extraLabel body =
  tr_ [] $ do
    td_ [] $ do
      asIcon' body
      " "
      (toHtml extraLabel)
      " "
      toHtml $ labelText body
    td_ [] $ do
      zodiacLink body 

localTime_ :: UTCTime -> Html ()
localTime_ moment = 
  time_ [class_ "local-datetime", datetime_ $ moment & iso8601Show & pack] $ do
    toHtml $ moment & formatTime defaultTimeLocale rfc822DateFormat 

localDate_ :: UTCTime -> Html ()
localDate_ moment = 
  time_ [class_ "local-date", datetime_ $ moment & iso8601Show & pack] $ do
    toHtml $ moment & formatTime defaultTimeLocale "%a, %_d %b %Y" 

asIcon' :: HasLabel a => a -> Html ()
asIcon' z =
  i_ [class_ ("fnc-" <> shown <> " tooltip"), title_ shown, data_ "tooltip" label'] $ do
    toHtml alt
  where
    label' = pack . label $ z
    alt :: Text
    alt =
      case label' of
        "MC" -> "MC"
        "Asc" -> "Asc"
        _ -> ""
    shown  = 
      case label' of
        "Mean Node" -> "MeanNode"
        "Lilith" -> "MeanApog"
        _ -> label'
