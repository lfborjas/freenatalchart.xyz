{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Server.Handlers where

import Import
import Server.Types
import Servant
import Lucid

service :: ServerT Service AppM
service = 
    root
    :<|> (serveDirectoryWebApp "static")

root :: AppM (Html ())
root = do
    pure $ p_ "hello world"
