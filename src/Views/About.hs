{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, QuasiQuotes #-}

module Views.About (render) where

import Import
import Views.Common
import Lucid
import CMark
import Data.String.Interpolate.IsString

render :: HasStaticRoot ctx => ctx -> Html ()
render renderCtx = do
    head_ $ do
        title_ "About Free Natal Chart"
        metaCeremony renderCtx

    body_ $ do
        div_ [id_ "main", class_ "container grid-sm"] $ do
            header_ [class_ "navbar bg-dark"] $ do
                section_ [class_ "navbar-section navbar-brand"] $ do
                    a_ [href_ "/", class_ "brand-text"] "FreeNatalChart.xyz"
            toHtmlRaw $ 
                commonmarkToHtml [] pablum
            
            footerNav


pablum :: Text
pablum = [i|
# About

Thanks for using freenatalchart.xyz!

As a software engineer interested in astrology as a tool for insight
and individuation through understanding symbols that embody the cycles of our lives,
I wanted to learn everything I could from my birth chart: the aspects, the positions of planets,
and what this whole houses and house systems deal was about--and I wanted a good rendering
of my chart, with full tables and lists of data. 

This proved a bit hard to obtain easily online.


I'm extremely indebted to fantastic sites like astrodienst and cafe astrology for offering a ton of information
for free, but their free chart renderings are a bit out of date, and one can get lost in the wealth of resources they offer
for free, or for a fee. Astrologers and artists work hard to produce that stuff, so of course they need to get paid!
My purpose here is to offer much less by way of interpretation, beyond simple concepts, 
but to instead give you your chart and numeric data in an easy to get and consume format. That way you can draw your own
conclusions, or go read those and the many other more sophisticated astrology websites with your free data in hand!


## On the shoulders of giants

I try to use free resources for this website, and the [full code for this website](https://github.com/lfborjas/freenatalchart.xyz) is also free to peruse, use, but hopefully
not abuse! 

* For the ephemerides data (used to compute planet positions and house cusps,)
I wrote [a little library](https://github.com/lfborjas/swiss-ephemeris) to interface with 
[the fantastic Swiss Ephemeris library made available by the scientists behind astro.com](https://www.astro.com/swisseph/swephinfo_e.htm).
* For Timezone detection, I wrote [another little library](https://github.com/lfborjas/timezone-detect) to interface with
  [the also fantastic ZoneDetect library](https://github.com/BertoldVdb/ZoneDetect). Thanks to the author for making available the timezone geography files!
* For geolocation, I use [Geocode.city](https://geocode.city/). If you want to run a copy of this website, follow the setup instructions there. Contact me if you need more than 1000 request/browser per day
|]
