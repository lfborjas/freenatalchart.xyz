{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, QuasiQuotes#-}

module Views.Chart (render) where

import Import
import Lucid
import Views.Common
import Server.Types

render :: BirthData -> Html ()
render BirthData{..} = html_ $ do
    head_ $ do
        title_ "Your Natal Chart"
        metaCeremony

    body_ $ do
        p_ "SOON!"
