{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Chart.Common where

import Import
import RIO.Text (intercalate, justifyLeft,  pack, toLower )
import RIO.Writer (MonadWriter(tell), Writer)
import Ephemeris
import Lucid
import qualified RIO.Text as T
import Text.Printf (printf)
import Data.Foldable (Foldable(maximum))
import RIO.Time (defaultTimeLocale, formatTime, UTCTime)

labelText :: (HasLabel a) => a -> Text
labelText = pack . label

houseAspectLabel :: HouseNumber -> Text
houseAspectLabel I = "Asc"
houseAspectLabel X = "MC"
houseAspectLabel h = labelText h

colWidth :: HasLabel a => [a] -> Int
colWidth = maximum . map (length . label)

planetWidth, doubleWidth, aspectWidth :: Int
planetWidth = colWidth defaultPlanets
doubleWidth = 8
aspectWidth = colWidth $ map aspectName (majorAspects <> minorAspects)

justifyPlanetPos, justifyHouseNum, justifyLongitude
  , justifyDouble, justifyLatitude, justifyDeclination
  , justifyAspecting, justifyAspected, justifyAspect :: Text -> Text

justifyPlanetPos = justifyLeft (planetWidth + 4) ' '
justifyHouseNum  = justifyLeft (length ("House"::String)) ' '
justifyLongitude = justifyLeft (length ("Longitude"::String)) ' '
justifyDouble    = justifyLeft doubleWidth ' '
justifyLatitude  = justifyLeft (length ("Latitude"::String)) ' '
justifyDeclination = justifyLeft (length ("Declination"::String)) ' '
justifyAspecting = justifyLeft (maximum [length ("Aspecting"::String), planetWidth]) ' '
justifyAspected  = justifyLeft (maximum [length ("Aspected"::String), planetWidth]) ' '
justifyAspect    = justifyLeft (maximum [length ("Aspect"::String), aspectWidth]) ' '

heading :: [Text] -> Text
heading = intercalate "|"

tellLine :: Text -> (Writer Text ())
tellLine t = tell (t <> "\n")

ln_ :: Text -> Writer Text ()
ln_ = tellLine


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

-- | aspect cell, but specialized to the aspecting body being a planet.
planetaryAspectCell :: HasLongitude a => Maybe (HoroscopeAspect PlanetPosition a) -> Html ()
planetaryAspectCell Nothing = mempty
planetaryAspectCell (Just (a@HoroscopeAspect {..})) =
  span_ [aspectColorStyle aspect] $ do
    asIcon . aspectName $ aspect
    " "
    htmlDegrees' (True, False) orb
    span_ [class_ "text-tiny tooltip", data_ "tooltip" (pack . show $ phase)] $ do
      toHtml . pack . label $ phase
  where
    phase = aspectPhase a

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

formatUTCTimestamp :: UTCTime -> String
formatUTCTimestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z"

headerIcon :: Html ()
headerIcon = i_ [class_ "icon icon-arrow-right mr-1 c-hand icon-right icon-light"] ""

sectionHeading :: Html () -> Html ()
sectionHeading = h5_ [class_ "d-inline text-primary"]

divider_ :: Html ()
divider_ = div_ [class_ "divider"] ""


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

orbsTable :: [Aspect] -> Html ()
orbsTable aspectList =
  table_ [id_ "orbs-used", class_ "table table-no-borders"] $ do
   thead_ [] $ do
     tr_ [] $ do
       th_ "Aspect"
       th_ "Angle"
       th_ "Orb"
   tbody_ [] $ do
     forM_ aspectList $ \a@Aspect {..} -> do
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

cardDark_ :: Attribute
cardDark_ = class_ "card card-dark mx-2 my-2 text-center"
attributeTitle_ :: Html () -> Html ()
attributeTitle_ = h5_ [class_ "text-light"] 

navbar_ :: Html ()
navbar_ =
  header_ [class_ "navbar bg-dark navbar-fixed navbar-fixed-top"] $ do
    section_ [class_ "navbar-section"] $ do
      -- NOTE: there's a bit of JS in date.js that will find this element
      -- and replace its click event with navigating to transits at the 
      -- moment of click. In the absence of JS, it simply refreshes the page.
      a_ [id_ "moment-link", href_ "", class_ "mr-2"] $ do
        i_ [class_ "icon icon-time", title_ "Show transits as of right now"] ""
        span_ [class_ "hide-sm"] " Current Transits"
    section_ [class_ "navbar-section navbar-center navbar-brand"] $ do
       a_ [href_ "/", class_ "brand-text"] "FreeNatalChart.xyz"
    section_ [class_ "navbar-section"] $ do
      a_ [href_ "#main"] $ do
        span_ [class_ "hide-sm"] "Back to Top "
        i_ [class_ "icon icon-upward", title_ "Back to Top"] ""
