{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module ExternalServices.TimezoneDB where

import Import
import Data.Aeson
import RIO.Time (parseTimeM, defaultTimeLocale, UTCTime)
import Data.Aeson.Types
import Data.Time.Clock.POSIX
import Network.HTTP.Req

-- | Successful response from TimezoneDB's get-time-zone endpoint:
-- https://timezonedb.com/references/get-time-zone
data TimezoneData = TimezoneData    
    {
        countryCode :: Text,
        countryName :: Text,
        zoneName :: Text,
        abbreviation :: Text,
        gmtOffset :: POSIXTime,
        dst :: Bool,
        zoneStart :: POSIXTime,
        zoneEnd :: POSIXTime,
        timestamp :: POSIXTime,
        formatted :: UTCTime
        -- totalPage and currentPage also exist
    } deriving (Eq, Show, Generic)

parseFormattedTime :: Parser String -> Parser UTCTime
parseFormattedTime s = do 
    val <- s
    parsed <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" val
    return parsed

parseIsDST :: Parser String -> Parser Bool
parseIsDST s = do
    val <- s
    return $ (val == "1")

data APIResult
 = APIError Text
 | APIResponse TimezoneData 
 deriving (Eq, Show)


-- inspired by:
-- https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html
instance FromJSON APIResult where
    parseJSON = withObject "APIResult" $ \v -> do
        status <- (v .: "status") :: Parser Text
        if status == "FAILED" then do
            APIError <$> v .: "message"
        else
           let tzd = TimezoneData <$> v .: "countryCode"
                                 <*> v .: "countryName"
                                 <*> v .: "zoneName"
                                 <*> v .: "abbreviation"
                                 <*> v .: "gmtOffset"
                                 <*> (parseIsDST $ v .: "dst")
                                 <*> v .: "zoneStart"
                                 <*> v .: "zoneEnd"
                                 <*> v .: "timestamp"
                                 <*> (parseFormattedTime $ v .: "formatted")
            in
                APIResponse <$> tzd

{-
Example failure:
{\"status\":\"FAILED\",\"message\":\"Invalid API key.\",\"countryCode\":\"\",\"countryName\":\"\",\"zoneName\":\"\",\"abbreviation\":\"\",\"gmtOffset\":0,\"dst\":0,\"zoneStart\":0,\"zoneEnd\":null,\"nextAbbreviation\":null,\"timestamp\":0,\"formatted\":\"\"}

Example success:
{\"status\":\"OK\",\"message\":\"\",\"countryCode\":\"US\",\"countryName\":\"United States\",\"zoneName\":\"America\\/New_York\",\"abbreviation\":\"EST\",\"gmtOffset\":-18000,\"dst\":\"0\",\"zoneStart\":1572760800,\"zoneEnd\":1583650800,\"nextAbbreviation\":\"EDT\",\"timestamp\":1577818800,\"formatted\":\"2019-12-31 19:00:00\"}
-}

-- | Make a request to the timezone DB get-time-zone service:
-- https://timezonedb.com/references/get-time-zone
-- To use from e.g. IO:
-- import Network.HTTP.Req
-- 
-- r <- runReq defaultHttpConfig $ tz "YILP3UAHHNMT" 40.7831 (-73.9712)
-- responseBody r
-- where:
-- :t r
-- r :: JsonResponse ExternalServices.TimezoneDB.APIResult
timezoneRequest :: (MonadHttp m) => Text -> Double -> Double -> m (JsonResponse APIResult)
timezoneRequest k lt ln = 
    req GET (http "api.timezonedb.com" /: "v2.1" /: "get-time-zone") NoReqBody jsonResponse $ 
        "key" =: k 
        <> "format" =: ("json" :: Text)
        <> "by" =: ("position" :: Text)
        <> "lat" =: lt
        <> "lng" =: ln
        -- TODO: accept the reference time as NominalDiffTime/POSIXTime

-- getTimestamp :: Text -> Double -> Double -> Req TimezoneData 
-- getTimestamp k lt ln = do
--     resp <- runReq defaultHttpConfig $ tz k lt ln
--     return $ responseBody resp
