{-# LANGUAGE NoImplicitPrelude, OverloadedStrings#-}


module Views.Common where

import Import
import Lucid
import RIO.Text (pack)

data RenderContext = RenderContext
    { staticRoot :: FilePath
    } deriving (Eq, Show)

instance HasStaticRoot RenderContext where
    staticRootL = lens staticRoot (\x y -> x {staticRoot = y})

instance HasAlgoliaAppId RenderContext where
    algoliaAppIdL = lens (const "") (const . id)

instance HasAlgoliaAppKey RenderContext where
    algoliaAppKeyL = lens (const "") (const . id)

assetRef :: HasStaticRoot ctx => ctx -> FilePath -> Attribute
assetRef renderCtx = 
    href_ . pack . (mappend assetPath)
    where
        assetPath = renderCtx ^. staticRootL

stylesheets :: Html ()
stylesheets = do
    -- TODO: serve from our own assets:
    -- https://picturepan2.github.io/spectre/getting-started/installation.html
    link_ [rel_ "stylesheet", type_ "text/css", href_ "https://unpkg.com/spectre.css@0.5.9/dist/spectre.min.css"]

favicon :: HasStaticRoot ctx => ctx -> Html ()
favicon renderCtx = do
    -- from https://favicon.io/emoji-favicons/capricorn/
    link_ [rel_ "apple-touch-icon", sizes_ "180x180", assetRef' "apple-touch-icon.png"]
    link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", assetRef' "favicon-32x32.png"]
    link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", assetRef' "favicon-16x16.png"]
    link_ [rel_ "manifest", assetRef' "site.webmanifest"] 
    where
        assetRef' = assetRef renderCtx

metaCeremony :: HasStaticRoot ctx => ctx -> Html ()
metaCeremony renderCtx = do
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    meta_ [name_ "description", content_ "Get your free natal chart, with all the information you need for your own discoveries of the self!"]
    meta_ [charset_ "UTF-8"]
    favicon renderCtx
    stylesheets

broughtToYou :: Html ()
broughtToYou = do
    a_ [class_ "text-muted", href_ "https://lfborjas.com"] "Brought to you by a ♑"
