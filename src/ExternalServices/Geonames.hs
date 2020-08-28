{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module ExternalServices.Geonames where

import Import
import Data.Aeson
import Network.HTTP.Req
import RIO.Time (parseTimeM, defaultTimeLocale, UTCTime)
import Data.Aeson.Types (Parser)

-- | Known exceptions from Geonames
-- from:
-- http://www.geonames.org/export/webservice-exception.html
-- example:
-- 
data GeonamesErrorCode = 
    AuthorizationException
    | RecordDoesNotExist
    | OtherError
    | DatabaseTimeout
    | InvalidParameter
    | NoResultFound
    | DuplicateException
    | PostalCodeNotFound
    | DailyLimitOfCreditsExceeded
    | HourlyLimitOfCreditsExceeded
    | WeeklyLimitOfCreditsExceeded
    | InvalidInput
    | ServerOverloadedException
    | ServiceNotImplemented
    | RadiusTooLarge
    | MaxRowsTooLarge
    | UnknownError
    deriving (Show, Eq)

errorCodeToEnum :: Int -> GeonamesErrorCode
errorCodeToEnum code =
    case code of
        10 -> AuthorizationException
        11 -> RecordDoesNotExist
        12 -> OtherError
        13 -> DatabaseTimeout
        14 -> InvalidParameter
        15 -> NoResultFound
        16 -> DuplicateException
        17 -> PostalCodeNotFound
        18 -> DailyLimitOfCreditsExceeded
        19 -> HourlyLimitOfCreditsExceeded
        20 -> WeeklyLimitOfCreditsExceeded
        21 -> InvalidParameter
        22 -> ServerOverloadedException
        23 -> ServiceNotImplemented
        24 -> RadiusTooLarge
        27 -> MaxRowsTooLarge
        _ -> UnknownError

data GeonamesError = GeonamesError 
    {
        errorCode :: GeonamesErrorCode
    ,   errorMessage :: String
    } deriving (Eq, Show, Generic)

data APIResult a = 
    APIResponse a
    | APIError GeonamesError
    deriving (Eq, Show)

parseSuccess :: FromJSON a => Value -> Parser (APIResult a)
parseSuccess v = APIResponse <$> parseJSON v

parseError :: Value -> Parser (APIResult a)
parseError = withObject "GeonamesError" $ \o -> do
    status <- o .: "status"
    eCode  <- status .: "value"
    eMessage <- status .: "message"
    return $ APIError $ GeonamesError (errorCodeToEnum eCode) eMessage

-- from:
-- https://stackoverflow.com/a/22515058
instance (FromJSON a) => FromJSON (APIResult a) where
    parseJSON v = parseError v <|> parseSuccess v

-- | Timezone response:
-- http://www.geonames.org/export/web-services.html#timezone
-- e.g.
-- {\"sunrise\":\"2020-08-28 05:37\",\"lng\":-87.2750137,\"countryCode\":\"HN\",\"gmtOffset\":-6,\"rawOffset\":-6,\"sunset\":\"2020-08-28 18:02\",\"timezoneId\":\"America/Tegucigalpa\",\"dstOffset\":-6,\"countryName\":\"Honduras\",\"time\":\"2020-08-27 19:39\",\"lat\":14.0839053}⏎
--- TODO: will need a newtype for the time data (sunrise, sunset, time), because they come in the above weird format:
-- maybe something like DotNetTime:
-- https://hackage.haskell.org/package/aeson-1.5.3.0/docs/src/Data.Aeson.Types.FromJSON.html#line-1998
-- https://hackage.haskell.org/package/aeson-1.5.3.0/docs/Data-Aeson.html#t:DotNetTime
-- vs.
-- https://hackage.haskell.org/package/aeson-1.5.3.0/docs/src/Data.Aeson.Types.FromJSON.html#line-2065
data TimezoneResponse = TimezoneResponse
    {
        sunrise :: UTCTime,
        sunset :: UTCTime,
        lat :: Double,
        lng :: Double,
        countryCode :: Text,
        gmtOffset :: Double,
        rawOffset :: Double,
        timezoneId :: Text,
        dstOffset :: Double,
        countryName :: Text,
        time :: UTCTime
    } deriving (Eq, Show, Generic)

-- inspired by: https://github.com/bos/aeson/issues/197
-- also: https://hackage.haskell.org/package/rio-0.1.18.0/docs/RIO-Time.html#v:formatTime
parseGeonamesTime :: Parser String -> Parser UTCTime
parseGeonamesTime s = do 
    val <- s
    parsed <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %R" val
    return parsed

instance FromJSON TimezoneResponse where
    parseJSON (Object v) = 
        TimezoneResponse <$> (parseGeonamesTime $ v .: "sunrise")
                         <*> (parseGeonamesTime $ v .: "sunset")
                         <*> v .: "lat"
                         <*> v .: "lng"
                         <*> v .: "countryCode"
                         <*> v .: "gmtOffset"
                         <*> v .: "rawOffset"
                         <*> v .: "timezoneId"
                         <*> v .: "dstOffset"
                         <*> v .: "countryName"
                         <*> (parseGeonamesTime $ v .: "time")
    parseJSON _ = fail "Invalid TimezoneResponse"

instance ToJSON TimezoneResponse
