{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, QuasiQuotes#-}

module Views.Index (render) where

import Import hiding (for_)
import Lucid
import RIO.Text (null, intercalate, pack)
import Data.String.Interpolate.IsString
import Views.Common
import Server.Types
import RIO.List (nub)
import Servant (toQueryParam, ToHttpApiData)

render :: (HasStaticRoot ctx, HasGeocodeApiKey ctx) => ctx -> Maybe FailedChartForm -> Html ()
render ctx maybeForm = do
    doctype_
    html_ [lang_ "en"] $ do
        head_ $ do
            title_ "Free Natal Chart"
            metaCeremony ctx
            geocodeCityStyle

        body_ $ do
            div_ [id_ "main", class_ "container grid-sm"] $ do
                header_ [class_ "navbar bg-dark navbar-fixed navbar-fixed-top"] $ do
                    section_ [class_ "navbar-section navbar-brand hide-sm"] $ do
                        a_ [href_ "/", class_ "brand-text"] "FreeNatalChart.xyz"
                    section_ [class_ "navbar-section navbar-center"] ""
                    section_ [class_ "navbar-section"] $ do
                        a_ [id_ "chart-of-the-moment", class_ "text-white d-invisible", href_ ""] "Chart of the Moment"


                h1_ [class_ "under-navbar text-primary text-center hero-title gold-stars-bg"] $ do
                    "Get your natal chart"

                div_ [id_ "err", class_ "my-2 toast toast-error d-none"] $ do
                    p_ [id_ "errMsg"] ""
                    a_ [href_ "https://github.com/lfborjas/freenatalchart.xyz/issues/new/choose"] $ do
                        "Report an issue"

                form_ [action_ "/full-chart", method_ "get", style_ "min-height: 50vh;"] $ do
                    div_ [class_ (formGroupClass (val formLocation) (err InvalidLocation))] $ do
                        label_ [class_ "form-label", for_ "location"] "Born in"
                        geocodeCityAutocomplete ctx
                        errorHint (err InvalidLocation)
                        noscript_ [class_ "bg-warning"]
                                  "You seem to have disabled JavaScript. We use a little bit of scripting to determine your birth location based on what you type in this box, without scripting, we're unable to!"

                    -- we could use the native `date` and `time` inputs,
                    -- or even a single `datetime-local`, but browser support
                    -- is wonky nowadays, and IMO it's annoying to look up one's birth date
                    -- in such controls: they're optimized for selecting today and days around
                    -- today, not ~30 years in the past! (showing ma age, heh)
                    -- if we want them tho, e.g.:
                    -- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/date
                    fieldset_ [class_ isDateInvalidClass ] $ do
                        div_ [class_ "form-group form-group-flex"] $ do
                            numberInput "month" "Month" (1, 12) (val formMonth) (err InvalidMonth) "MM"
                            numberInput "day" "Day" (1, 31) (val formDay) (err InvalidDay) "DD"
                            numberInput "year" "Year" (1800, 2399) (val formYear) (err InvalidYear) "YYYY"
                        errorHint (groupErr [InvalidMonth, InvalidDay, InvalidYear])

                        div_ [class_ "form-group form-group-flex"] $ do
                            numberInput "hour" "Hour" (1, 12) (val formHour) (err InvalidHour) "HH"
                            numberInput "minute" "Minute" (0, 60) (val formMinute) (err InvalidMinute) "MM"

                            div_ [class_ "form-group", style_ "margin-top: 1.75rem; margin-left: .5rem"] $ do
                                label_ [class_ "form-radio form-inline"] $ do
                                    input_ $ [type_ "radio", name_ "day-part", value_ "am"] <> isChecked "am"
                                    i_ [class_ "form-icon"] ""
                                    "AM"
                                label_ [class_ "form-radio form-inline"] $ do
                                    input_ $ [type_ "radio", name_ "day-part", value_ "pm"] <> isChecked "pm"
                                    i_ [class_ "form-icon"] ""
                                    "PM"

                        errorHint (groupErr [InvalidDateTime, InvalidHour, InvalidMinute, InvalidDayPart])

                    -- meant to be filled by the JS for geolocation,
                    -- the server should fall back to "best effort" location if these aren't available.
                    input_ [id_ "lat", name_ "lat", type_ "hidden", value_ (val formLatitude)]
                    input_ [id_ "lng", name_ "lng", type_ "hidden", value_ (val formLongitude)]


                    div_ [class_ "form-group text-center"] $ do
                        button_ [class_ "btn btn-primary btn-round btn-lg"] "Show me my chart"
                    div_ [class_ "form-group text-center"] $ do
                        a_ [class_ "btn btn-link", href_ "/"] "Start Over"

                    -- TODO: add checkboxes for preferences?
                    -- e.g. monochrome -- though ideally the tables would also help:
                    -- https://webaim.org/articles/visual/colorblind
                    -- audit accessibility: https://webaim.org/techniques/forms/controls
                    -- aria described by and invalid: https://webaim.org/techniques/formvalidation/

                footerNav
            -- TODO: host this ourselves.
            script_ [src_ "https://cdn.jsdelivr.net/gh/geocode-city/autocomplete@0.1.0.1/dist/geocode_city.min.js"] (""::Text)
            script_ [src_ . pack $ assetPath <> "js/location.js"] (""::Text)
            geolocationInit
    where
        assetPath = ctx ^. staticRootL
        isDateInvalidClass =
            maybe "" (const "has-error") (err InvalidDateTime)
        val :: ToHttpApiData a => (ChartForm -> ParsedParameter a) -> Text
        val = val' maybeForm
        err :: ChartFormValidationError -> Maybe Text
        err = err' maybeForm
        groupErr :: [ChartFormValidationError] -> Maybe Text
        groupErr = errors' maybeForm
        isChecked :: Text -> [Attribute]
        isChecked dayP
            | (not $ isEmpty $ val formDayPart) && (val formDayPart) == dayP = [checked_]
            | (isEmpty $ val formDayPart) && dayP == "am" = [checked_]
            | otherwise = []

geolocationInit :: Html ()
geolocationInit =
    script_ $ do
           [i|
               initGeolocation();
           |]

geocodeCityAutocomplete :: HasGeocodeApiKey ctx => ctx -> Html ()
geocodeCityAutocomplete ctx =
    let appKey = ctx ^. geocodeApiKeyL
        props' = [ id_ "geocode-city-autocomplete"
                 , data_ "input-name" "location"
                 , data_ "input-class" "form-input input-transparent"
                 ]
        props = if Import.null appKey then props' else props' <> [data_ "api-key" (pack appKey)]
    in
        div_ props mempty

-- See:
-- https://github.com/natal-chart/firmament/blob/3458af40324c285cd8fec72e85bc09167e13ffc3/src/_variables.scss
-- https://github.com/geocode-city/autocomplete/blob/130046d2cd8731a258bf926bddd18092ad9c919e/src/Autocomplete/Component.purs
-- https://github.com/geocode-city/web/blob/82b8c50b2d7c30193bf3b3fe0eecfafcbc71294d/src/_autocomplete.scss
geocodeCityStyle :: Html ()
geocodeCityStyle = let prefix = ("geocode-city__" :: String) in style_ $ do
  [i|
    .multiline-p{ white-space: pre-line; hyphens: auto; word-break: break-word; overflow-wrap: anywhere;}

   .#{prefix}autocomplete-container { background-color: #fff; border-radius: 5px; margin-top: 5px; color: #000950; }
   .#{prefix}autocomplete-item-highlighted { background-color: #3c4feb; }
   .#{prefix}autocomplete-item { cursor: pointer; padding: 5px; }
   .#{prefix}city-location { color: #bcc3ce; }
   .#{prefix}powered-by { color: #000063; }
  |]

isEmpty :: Text -> Bool
isEmpty = RIO.Text.null

numberInput :: Text -> Text -> (Int, Int) -> Text -> (Maybe Text) -> Text -> Html ()
numberInput name' label' (start, end) value e placeholder'=
    div_ [class_ (formGroupClass value e)] $ do
        label_ [class_ "form-label", for_ name'] (toHtml label')
        input_ [ class_ "form-input input-transparent"
               , type_ "number"
               , id_ name'
               , name_ name'
               , required_ ""
               , min_ (asText' start)
               , max_ (asText' end)
               , value_ value
               , placeholder_ placeholder'
               ]
    where
        asText' = pack . show

errorHint :: Maybe Text -> Html ()
errorHint = maybe mempty (\e -> p_ [class_ "form-input-hint input-hint-highlighted text-error multiline-p"] (toHtml e))

formGroupClass :: Text -> Maybe Text -> Text
formGroupClass value e
    | (isJust e) = "form-group has-error"
    | (not . isEmpty $ value) && (isNothing e) = "form-group has-success"
    | otherwise = "form-group"

val' :: ToHttpApiData a => Maybe FailedChartForm -> (ChartForm -> ParsedParameter a) -> Text
val' f = asInputValue . (valueFromForm f)

asInputValue :: ToHttpApiData a => Maybe a -> Text
asInputValue Nothing = ""
asInputValue (Just x) = toQueryParam x

valueFromForm :: Maybe FailedChartForm -> (ChartForm -> ParsedParameter a) -> Maybe a
valueFromForm Nothing _ = Nothing
valueFromForm (Just failedForm) valueFn =
    either (const Nothing)
           Just
           (failedForm & originalForm & valueFn)

err' :: Maybe FailedChartForm -> ChartFormValidationError -> Maybe Text
err' Nothing _ = Nothing
err' (Just failedForm) errorType =
    errorMessagesFor (failedForm & validationErrors) errorType

-- | Given a specific error (e.g. InvalidDateTime,) find any applicable error messages.
errorMessagesFor :: ChartFormErrors -> ChartFormValidationError -> Maybe Text
errorMessagesFor errors errorT =
    if (Import.null allErrors) then
        Nothing
    else
        Just $ intercalate ", " allErrors
    where
        allErrors =
            filter (\(e, _) -> e == errorT) (toList errors)
                & map snd
                & nub

errors' :: Maybe FailedChartForm -> [ChartFormValidationError] -> Maybe Text
errors' Nothing _ = Nothing
errors' (Just failedForm) errorTypes =
    map (errorMessagesFor $ failedForm & validationErrors) errorTypes
    & catMaybes
    & (\l -> if Import.null l then Nothing else Just $ intercalate "\n" l)
