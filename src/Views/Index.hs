{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, QuasiQuotes#-}

module Views.Index (render, renderTestIndex) where

import Import hiding (for_)
import Lucid
import RIO.Text (pack)
import Data.String.Interpolate.IsString
import Views.Common


numberInput :: Text -> Text -> (Int, Int) -> Html ()
numberInput name' label (start, end) = 
    div_ [class_ "form-group"] $ do
        label_ [class_ "form-label", for_ name'] (toHtml label)
        input_ [class_ "form-input", type_ "number", id_ name', name_ name', required_ "", min_ (asText start), max_ (asText end)]
    where
        asText = pack . show

render :: (Maybe AppContext) -> Html ()
render ctx = html_ $ do
    head_ $ do
        title_ "Free Natal Chart"
        metaCeremony
        
    body_ $ do
        div_ [id_ "main", class_ "container"] $ do
            div_ [class_ "hero hero-sm bg-primary"] $ do
                div_ [class_ "hero-body text-center"] $ do
                    h1_ "Get your free natal chart"
                    -- TODO: maybe link to a sample?

            form_ [] $ do
                div_ [class_ "form-group"] $ do
                    label_ [class_ "form-label", for_ "location"] "Born in"
                    input_ [class_ "form-input", type_ "search", id_ "location", name_ "location", placeholder_ "City or town"]

                -- we could use the native `date` and `time` inputs,
                -- or even a single `datetime-local`, but browser support
                -- is wonky nowadays, and IMO it's annoying to look up one's birth date
                -- in such controls: they're optimized for selecting today and days around
                -- today, not ~30 years in the past! (showing ma age, heh)
                -- if we want them tho, e.g.:
                -- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/date
                fieldset_ $ do
                    numberInput "day" "Day" (1, 31)
                    numberInput "month" "Month" (1, 12)
                    numberInput "year" "Year" (1800, 2399)
                    numberInput "hour" "Hour" (1, 12)
                    numberInput "minute" "Minute" (0, 60)
                
                    div_ [class_ "form-group"] $ do
                        label_ [class_ "form-radio form-inline"] $ do
                            input_ [type_ "radio", name_ "am-or-pm", value_ "am", checked_]
                            i_ [class_ "form-icon"] ""
                            "AM"
                        label_ [class_ "form-radio form-inline"] $ do
                            input_ [type_ "radio", name_ "am-or-pm", value_ "pm"]
                            i_ [class_ "form-icon"] ""
                            "PM"

                -- meant to be filled by the JS for geolocation,
                -- the server should fall back to "best effort" location if these aren't available.
                input_ [id_ "lat", name_ "lat", type_ "hidden"]
                input_ [id_ "lng", name_ "lng", type_ "hidden"]


                button_ [class_ "btn btn-primary"] "Show me my chart"

                -- TODO: add checkboxes for preferences?
                -- e.g. monochrome -- though ideally the tables would also help:
                -- https://webaim.org/articles/visual/colorblind
                -- audit accessibility: https://webaim.org/techniques/forms/controls
                -- aria described by and invalid: https://webaim.org/techniques/formvalidation/

            footer_ [class_ "navbar bg-secondary"] $ do
                section_ [class_ "navbar-section"] $ do
                    a_ [href_ "/about", class_ "btn btn-link", title_ "tl;dr: we won't sell you anything, or store your data."] "About"
                section_ [class_ "navbar-center"] $ do
                    -- TODO: add a lil' icon?
                    span_ "Brought to you by a ♑"
                section_ [class_ "navbar-section"] $ do
                    a_ [href_ "https://github.com/lfborjas/freenatalchart.xyz", title_ "Made in Haskell with love and a bit of insanity.",  class_ "btn btn-link"] "Source Code"

        script_ [src_ "https://cdn.jsdelivr.net/npm/places.js@1.19.0"] (""::Text)
        (geolocationScript ctx)


geolocationScript :: (Maybe AppContext) -> Html ()
geolocationScript Nothing =
    mempty

geolocationScript (Just ctx) =
    let appId = ctx ^. algoliaAppIdL
        appKey = ctx ^. algoliaAppKeyL
    in
    script_ $ do
        [i|(function() {
            var placesAutocomplete = places({
                appId: '#{appId}',
                apiKey: '#{appKey}',
                container: document.getElementById('location')
            }).configure({
                type: 'city',
                aroundLatLngViaIP: false,
            });
            var $lat = document.getElementById('lat');
            var $lng = document.getElementById('lng');
            placesAutocomplete.on('change', function(e) {
                $lat.value = e.suggestion.latlng.lat;
                $lng.value = e.suggestion.latlng.lng;
            });

            placesAutocomplete.on('clear', function() {
                $lat.value = '';
                $lng.value = '';             
            });
        })();|]
        

-- | Render to a file on disk, purely for debugging.

renderTestIndex :: IO ()
renderTestIndex = renderToFile "test/files/index.html" $ render Nothing
