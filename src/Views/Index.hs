{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Views.Index (render) where

import Import
import Lucid

stylesheets :: Html ()
stylesheets = do
    --link_ [rel_ "stylesheet", type_ "text/css", href_ "https://unpkg.com/tachyons@4.12.0/css/tachyons.min.css"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "https://unpkg.com/purecss@2.0.3/build/pure-min.css"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/assets/base.css"]

render :: Html ()
render = html_ $ do
    head_ $ do
        title_ "Free Natal Chart"
        --   <meta name="viewport" content="width=device-width, initial-scale=1">
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        meta_ [name_ "description", content_ "Get your free natal chart, with all the information you need for your own discoveries of the self!"]
        stylesheets

        
    body_ $ do
        div_ [id_ "main"] $ do
            div_ [class_ "header"] $ do
                h1_ "Free Natal Chart"

            div_ [class_ "content"] $ do
                p_ "Soon"
