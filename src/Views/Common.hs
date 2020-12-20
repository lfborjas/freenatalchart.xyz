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

fixtureRenderContext :: RenderContext
fixtureRenderContext = RenderContext "../../static/"

assetRef :: HasStaticRoot ctx => ctx -> FilePath -> Attribute
assetRef renderCtx = 
    href_ . pack . (mappend assetPath)
    where
        assetPath = renderCtx ^. staticRootL

stylesheets :: Html ()
stylesheets = do
    -- we're using our own theme:
    -- https://github.com/natal-chart/firmament
    link_ [rel_ "stylesheet", type_ "text/css", href_ "https://cdn.jsdelivr.net/gh/natal-chart/firmament@v0.0.3.0/dist/base.min.css"]

fonts :: Html ()
fonts = do
    link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com/"]
    link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css2?family=Quattrocento&family=Roboto&display=swap"]

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
    fonts

broughtToYou :: Html ()
broughtToYou = do
    a_ [class_ "text-muted", href_ "https://www.lfborjas.com"] $ do
        "Brought to you by a "
        i_ [class_ "fnc-Capricorn"] ""


otherLinks :: Html ()
otherLinks = do
    a_ [href_ "/about", class_ "btn btn-link text-white", title_ "tl;dr: your data is private, our source is free."] "About"
    a_ [href_ "https://blog.freenatalchart.xyz/", class_ "btn btn-link text-white", title_ "Announcements, technical writeups, notes on learning about astrology."] "Blog"

sourceCode :: Html ()
sourceCode = 
    a_ 
        [ href_ "https://github.com/lfborjas/freenatalchart.xyz"
        , title_ "Made in Haskell with love and a bit of insanity."
        ,  class_ "btn btn-link text-white"] "Source Code"


footerNav :: Html ()
footerNav =
    footer_ [class_ "navbar navbar-border-top"] $ do
        section_ [class_ "navbar-section"] $ do
            otherLinks 
        section_ [class_ "navbar-center"] $ do
            broughtToYou
        section_ [class_ "navbar-section"] $ do
            sourceCode 
