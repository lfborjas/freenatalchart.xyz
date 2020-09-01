{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Server.Types where

import Import
import Servant
import Servant.HTML.Lucid
import Lucid.Base (Html)
import Validation (Validation)
import RIO.Text (pack)
import Data.Time.LocalTime.TimeZone.Detect (TimeZoneName)
import Data.Time.LocalTime (LocalTime)

type Service = 
    Get '[HTML] (Html ())
    :<|> "about" :> Get '[HTML] (Html ())
    :<|> Raw

type AppM = ReaderT AppContext Servant.Handler

-- Domain types:

readInRange :: (Read a, Ord a) => (a, a) -> String -> Maybe a
readInRange (begin, end) s = do 
    parsed <- readMaybe s
    if (parsed >= begin && parsed <= end) then
        return parsed
    else
        Nothing

newtype Year = Year Integer 
    deriving  (Eq, Show, Num)

-- inspired by: https://github.com/haskell-servant/servant/issues/796
mkYear :: Text -> Either Text Year
mkYear a = do
    s <- parseUrlPiece a
    -- this year range is for the only years between which we have ephemerides data;
    -- astro.com offers larger files for wider ranges, but we're good with
    -- these for now. Don't pretend you know Jesus's birth time!
    case readInRange (1800, 2399) s of
        Nothing -> Left $ pack $ s <> " is not a valid year."
        Just y -> return $ Year y

instance FromHttpApiData Year where
    parseUrlPiece = mkYear

newtype Month = Month Integer
    deriving (Eq, Show, Num)

mkMonth :: Text -> Either Text Month
mkMonth a = do
    s <- parseUrlPiece a
    case readInRange (1, 12) s of
        Nothing -> Left $ pack $ s <> " is not a valid month."
        Just m -> return $ Month m

instance FromHttpApiData Month where
    parseUrlPiece = mkMonth

newtype Day = Day Integer
    deriving (Eq, Show, Num)

mkDay :: Text -> Either Text Day
mkDay a = do
    s <- parseUrlPiece a
    case readInRange (1, 31) s of
        Nothing -> Left $ pack $ s <> " is not a valid day of the month."
        Just d -> return $ Day d

instance FromHttpApiData Day where
    parseUrlPiece = mkDay

newtype Hour = Hour Integer
    deriving (Eq, Show, Num)

mkHour :: Text -> Either Text Hour
mkHour a = do
    s <- parseUrlPiece a
    case readInRange (1, 12) s of
        Nothing -> Left $ pack $ s <> " is not a valid hour (use 1-12)."
        Just h -> return $ Hour h

instance FromHttpApiData Hour where
    parseUrlPiece = mkHour

newtype Minute = Minute Integer
    deriving (Eq, Show, Num)

mkMinute :: Text -> Either Text Minute
mkMinute a = do
    s <- parseUrlPiece a
    case readInRange (0, 60) s of
        Nothing -> Left $ pack $ s <> " is not a valid minute."
        Just m -> return $ Minute m

-- TODO(luis): replace the `Longitude` in `types` with this newtype!
newtype Latitude = Latitude {unLatitude :: Double}
    deriving (Eq, Show, Num)

-- ranges from this wrong answer that turned out to be right for me:
-- https://stackoverflow.com/a/23914607
mkLatitude :: Text -> Either Text Latitude
mkLatitude a = do
    s <- parseUrlPiece a
    case readInRange ((-90.0), 90.0) s of
        Nothing -> Left $ pack $ s <> " is an invalid latitude."
        Just l -> return $ Latitude l

instance FromHttpApiData Latitude where
    parseUrlPiece = mkLatitude

newtype Longitude = Longitude {unLongitude :: Double}
    deriving (Eq, Show, Num)

mkLongitude :: Text -> Either Text Server.Types.Longitude
mkLongitude a = do
    s <- parseUrlPiece a
    case readInRange (-180.0, 180.0) s of
        Nothing -> Left $ pack $ s <> " is an nvalid longitude.)"
        Just l -> return $ Longitude l

-- Form types:

data DateParts = DateParts 
    {
        year :: Year
    ,   month :: Month
    ,   day :: Day
    ,   hour :: Hour
    ,   minute :: Minute
    ,   isMorning :: Bool
    } deriving (Eq, Show)

data Location = Location
    {
        locationInput :: Text
    ,   locationLatitude :: Latitude
    ,   locationLongitude :: Server.Types.Longitude
    ,   locationTimeZone :: TimeZoneName
    } deriving (Eq, Show)

data ChartFormValidationError 
    = InvalidLocation
    | InvalidDateTime
    deriving (Eq, Show)

type ChartFormErrors = NonEmpty (ChartFormValidationError, Text)
type ChartFormValidation a = Validation ChartFormErrors a
type ParsedParameter a = Either Text a

data BirthData = BirthData
    {
        birthLocation :: Location
    ,   birthLocalTime :: LocalTime
    } deriving (Eq, Show)

data ChartForm = ChartForm
    {
        formLocation :: ParsedParameter Text
    ,   formLatitude :: ParsedParameter Latitude
    ,   formLongitude :: ParsedParameter Server.Types.Longitude
    ,   formYear :: ParsedParameter Year
    ,   formMonth :: ParsedParameter Month
    ,   formDay :: ParsedParameter Day
    ,   formHour :: ParsedParameter Hour
    ,   formMinute :: ParsedParameter Minute
    ,   formIsAm :: ParsedParameter Bool
    } deriving (Eq, Show)
