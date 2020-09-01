{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}


module Server.Types where

import Import hiding (Longitude)
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

instance ToHttpApiData Year where
    toUrlPiece (Year v) = pack $ show v

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

instance ToHttpApiData Month where
    toUrlPiece (Month m) = pack $ show m

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

instance ToHttpApiData Day where
    toUrlPiece (Day d) = pack $ show d

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

instance ToHttpApiData Hour where
    toUrlPiece (Hour h) = pack $ show h

newtype Minute = Minute Integer
    deriving (Eq, Show, Num)

mkMinute :: Text -> Either Text Minute
mkMinute a = do
    s <- parseUrlPiece a
    case readInRange (0, 60) s of
        Nothing -> Left $ pack $ s <> " is not a valid minute."
        Just m -> return $ Minute m

instance FromHttpApiData Minute where
    parseUrlPiece = mkMinute

instance ToHttpApiData Minute where
    toUrlPiece (Minute m) = pack $ show m

newtype DayPart = DayPart { unDayPart :: String }
    deriving (Eq, Show, IsString)

mkDayPart :: Text -> Either Text DayPart
mkDayPart a = do
    s <- parseUrlPiece a
    if (s `elem` ["AM", "PM"]) then
        return $ DayPart s
    else
        Left $ pack "Please choose part of day (AM or PM)"

instance FromHttpApiData DayPart where
    parseUrlPiece = mkDayPart

instance ToHttpApiData DayPart where
    toUrlPiece = pack . unDayPart 

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

instance ToHttpApiData Latitude where
    toUrlPiece = pack . show . unLatitude

newtype Longitude = Longitude {unLongitude :: Double}
    deriving (Eq, Show, Num)

instance FromHttpApiData Longitude where
    parseUrlPiece = mkLongitude

instance ToHttpApiData Longitude where
    toUrlPiece = pack . show . unLongitude

mkLongitude :: Text -> Either Text Server.Types.Longitude
mkLongitude a = do
    s <- parseUrlPiece a
    case readInRange (-180.0, 180.0) s of
        Nothing -> Left $ pack $ s <> " is an invalid longitude."
        Just l -> return $ Longitude l

-- Form types:

data DateParts = DateParts 
    {
        year :: Year
    ,   month :: Month
    ,   day :: Day
    ,   hour :: Hour
    ,   minute :: Minute
    ,   dayPart :: DayPart
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
    | InvalidYear
    | InvalidMonth
    | InvalidDay
    | InvalidHour
    | InvalidMinute
    | InvalidDayPart
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
    ,   formDayPart :: ParsedParameter DayPart
    } deriving (Eq, Show)

data FailedChartForm = FailedChartForm
    {
        originalForm :: ChartForm
    ,   validationErrors :: ChartFormErrors
    } deriving (Eq, Show)
