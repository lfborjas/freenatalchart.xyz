{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Server.Handlers where

import Import
import Server.Types
import Servant
import Lucid
import qualified Views.Index as Index
import qualified Views.About as About

service :: ServerT Service AppM
service = 
    root
    :<|> about
    :<|> (serveDirectoryWebApp "static")

root :: AppM (Html ())
root = do
    env <- ask
    return $ Index.render $ Just env

about :: AppM (Html ())
about = return $ About.render
