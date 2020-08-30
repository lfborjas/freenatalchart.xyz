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

-- TODO: write handler for looking up a given city given a fulltext search:
-- curl "http://api.geonames.org/searchJSON?q=tegucigalpa&maxRows=10&username=yawish"
-- {"totalResultsCount":2767,"geonames":[{"adminCode1":"08","lng":"-87.20681","geonameId":3600949,"toponymName":"Tegucigalpa","countryId":"3608932","fcl":"P","population":850848,"countryCode":"HN","name":"Tegucigalpa","fclName":"city, village,...","adminCodes1":{"ISO3166_2":"FM"},"countryName":"Honduras","fcodeName":"capital of a political entity","adminName1":"Francisco Morazán","lat":"14.0818","fcode":"PPLC"},{"adminCode1":"08","lng":"-87.16667","geonameId":3609672,"toponymName":"Departamento de Francisco Morazán","countryId":"3608932","fcl":"A","population":1680700,"countryCode":"HN","name":"Departamento de Francisco Morazán","fclName":"country, state, region,...","adminCodes1":{"ISO3166_2":"FM"},"countryName":"Honduras","fcodeName":"first-order administrative division","adminName1":"Francisco Morazán","lat":"14.25","fcode":"ADM1"},{"adminCode1":"08","lng":"-87.24806","geonameId":3613870,"toponymName":"Distrito Central","countryId":"3608932","fcl":"A","population":850227,"countryCode":"HN","name":"Distrito Central","fclName":"country, state, region,...","adminCodes1":{"ISO3166_2":"FM"},"countryName":"Honduras","fcodeName":"second-order administrative division","adminName1":"Francisco Morazán","lat":"14.14975","fcode":"ADM2"},{"adminCode1":"08","lng":"-87.2172","geonameId":6299822,"toponymName":"Toncontín International Airport","countryId":"3608932","fcl":"S","population":0,"countryCode":"HN","name":"Tegucigalpa Airport","fclName":"spot, building, farm","adminCodes1":{"ISO3166_2":"FM"},"countryName":"Honduras","fcodeName":"airport","adminName1":"Francisco Morazán","lat":"14.06088","fcode":"AIRP"},{"adminCode1":"08","lng":"-87.16377","geonameId":9199858,"toponymName":"Villa Olímpica de Tegucigalpa","countryId":"3608932","fcl":"S","population":0,"countryCode":"HN","name":"Villa Olímpica de Tegucigalpa","fclName":"spot, building, farm","adminCodes1":{"ISO3166_2":"FM"},"countryName":"Honduras","fcodeName":"athletic field","adminName1":"Francisco Morazán","lat":"14.09077","fcode":"ATHF"},{"adminCode1":"08","lng":"-87.20398","geonameId":9204039,"toponymName":"Estadio Nacional de Tegucigalpa","countryId":"3608932","fcl":"S","population":0,"countryCode":"HN","name":"Estadio Tiburcio Carías Andino","fclName":"spot, building, farm","adminCodes1":{"ISO3166_2":"FM"},"countryName":"Honduras","fcodeName":"athletic field","adminName1":"Francisco Morazán","lat":"14.0984","fcode":"ATHF"},{"adminCode1":"08","lng":"-87.4","geonameId":3600450,"toponymName":"Vallecillo","countryId":"3608932","fcl":"P","population":1986,"countryCode":"HN","name":"Vallecillo","fclName":"city, village,...","adminCodes1":{"ISO3166_2":"FM"},"countryName":"Honduras","fcodeName":"populated place","adminName1":"Francisco Morazán","lat":"14.51667","fcode":"PPL"},{"adminCode1":"06","lng":"-88.25","geonameId":3600947,"toponymName":"Tegucigalpa","countryId":"3608932","fcl":"P","population":0,"countryCode":"HN","name":"Tegucigalpa","fclName":"city, village,...","adminCodes1":{"ISO3166_2":"CR"},"countryName":"Honduras","fcodeName":"populated place","adminName1":"Cortés","lat":"15.63333","fcode":"PPL"},{"adminCode1":"18","lng":"-87.46667","geonameId":3600948,"toponymName":"Tegucigalpa","countryId":"3608932","fcl":"P","population":0,"countryCode":"HN","name":"Tegucigalpa","fclName":"city, village,...","adminCodes1":{"ISO3166_2":"YO"},"countryName":"Honduras","fcodeName":"populated place","adminName1":"Yoro","lat":"15.13333","fcode":"PPL"},{"adminCode1":"08","lng":"-87.06977","geonameId":3600972,"toponymName":"Tatumbla","countryId":"3608932","fcl":"A","population":4702,"countryCode":"HN","name":"Tatumbla","fclName":"country, state, region,...","adminCodes1":{"ISO3166_2":"FM"},"countryName":"Honduras","fcodeName":"second-order administrative division","adminName1":"Francisco Morazán","lat":"13.98467","fcode":"ADM2"}]}
-- and remove the timezone stuff? We have our offline solution in TimezoneUtil.hs
-- take the example request-making implementation in TimezoneDB.hs... and then get rid of that file!
