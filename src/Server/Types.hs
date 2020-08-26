{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Server.Types where

import Import
import Servant
import Servant.HTML.Lucid
import Lucid.Base (Html)

type Service = 
    Get '[HTML] (Html ())
    :<|> Raw

type AppM = ReaderT AppContext Servant.Handler
