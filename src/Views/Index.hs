{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Views.Index (render, renderTestIndex) where

import Import hiding (for_)
import Lucid
import RIO.Text (pack)

stylesheets :: Html ()
stylesheets = do
    -- TODO: serve from our own assets:
    -- https://picturepan2.github.io/spectre/getting-started/installation.html
    link_ [rel_ "stylesheet", type_ "text/css", href_ "https://unpkg.com/spectre.css@0.5.9/dist/spectre.min.css"]

numberInput :: Text -> Text -> (Int, Int) -> Html ()
numberInput name' label (start, end) = 
    div_ [class_ "form-group"] $ do
        label_ [class_ "form-label", for_ name'] (toHtml label)
        input_ [class_ "form-input", type_ "number", id_ name', name_ name', required_ "", min_ (asText start), max_ (asText end)]
    where
        asText = pack . show

render :: Html ()
render = html_ $ do
    head_ $ do
        title_ "Free Natal Chart"
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        meta_ [name_ "description", content_ "Get your free natal chart, with all the information you need for your own discoveries of the self!"]
        stylesheets
        
    body_ $ do
        div_ [id_ "main", class_ "container"] $ do
            div_ [class_ "hero hero-sm bg-primary"] $ do
                div_ [class_ "hero-body text-center"] $ do
                    h1_ "Free Natal Chart"
                    p_ "Fill out the form below"

            form_ [] $ do
                div_ [class_ "form-group"] $ do
                    label_ [class_ "form-label", for_ "location"] "Born in"
                    input_ [class_ "form-input", type_ "text", id_ "location", name_ "location", placeholder_ "City or town"]

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


                button_ [class_ "btn btn-primary"] "Submit"

            footer_ [class_ "navbar bg-secondary"] $ do
                section_ [class_ "navbar-section"] $ do
                    a_ [href_ "/about", class_ "btn btn-link", title_ "tl;dr: we won't sell you anything, or store your data."] "About"
                section_ [class_ "navbar-center"] $ do
                    -- TODO: add a lil' icon?
                    a_ [href_ "#"] ""
                section_ [class_ "navbar-section"] $ do
                    a_ [href_ "https://github.com/lfborjas/freenatalchart.xyz", title_ "Made in Haskell with love and a bit of insanity.",  class_ "btn btn-link"] "Source Code"


-- | Render to a file on disk, purely for debugging.

renderTestIndex :: IO ()
renderTestIndex = renderToFile "test/files/index.html" render
