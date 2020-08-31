{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    :<|> "about" :> Get '[HTML] (Html ())
    :<|> Raw

type AppM = ReaderT AppContext Servant.Handler

data ChartFormValidationError 
    = EmptyLocation
    | InvalidLocation
    | InvalidDateTime
    deriving (Eq, Show)

newtype Year = Year Integer 
    deriving  (Eq, Show, Num)

newtype Month = Month Integer
    deriving (Eq, Show, Num)

newtype Day = Day Integer
    deriving (Eq, Show, Num)

newtype Hour = Hour Integer
    deriving (Eq, Show, Num)

newtype Minute = Minute Integer
    deriving (Eq, Show, Num)

data DateParts = DateParts 
    {
        year :: Year
    ,   month :: Month
    ,   day :: Day
    ,   hour :: Hour
    ,   minute :: Minute
    ,   isMorning :: Bool
    } deriving (Eq, Show)

-- TODO: add instances from query params
