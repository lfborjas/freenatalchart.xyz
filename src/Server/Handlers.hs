{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Server.Handlers where

import Import
import Server.Types
import Servant
import Lucid
import qualified Views.Index as Index
import qualified Views.About as About
import RIO.Time (defaultTimeLocale, parseTimeM, LocalTime)
import Validation (failure, Validation(..))
import RIO.Text (pack)
import Data.Coerce (coerce)
import Data.Time.LocalTime.TimeZone.Detect (TimeZoneName, lookupTimeZoneName)
import Control.Selective (ifS)

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

-- HANDLER HELPERS

-- TODO: should these participate in the AppM monad so they can log and such?
-- params should be `Required Lenient`
-- as per: http://hackage.haskell.org/package/servant-0.15/docs/Servant-API-Modifiers.html#t:RequestArgument
validateDate :: 
    Either Text Year 
    -> Either Text Month
    -> Either Text Day
    -> Either Text Hour
    -> Either Text Minute
    -> Either Text Bool
    -> ChartFormValidation LocalTime
validateDate y m d h mn isAm =
    parseTime dateParts
    where
        invalidDateTimeFailure err = 
            Failure ( (InvalidDateTime, err) :| [])
        validateDateComponent = either invalidDateTimeFailure Success
        dateParts = DateParts <$> validateDateComponent y 
                              <*> validateDateComponent m
                              <*> validateDateComponent d
                              <*> validateDateComponent h
                              <*> validateDateComponent mn
                              <*> validateDateComponent isAm
        parseTime (Failure e) = Failure e
        parseTime (Success dp) = 
            maybe 
                (Failure ((InvalidDateTime, (pack $ shown dp) <> " is not a valid date.") :| []))
                Success
                (parseTimeM True defaultTimeLocale "%Y-%-m-%-d %l:%-M:%-S %p" (shown dp))
        show' x = show $ (coerce x :: Integer) 
        shown DateParts{..} = 
            (show' year) <> "-"
            <> (show' month) <> "-"
            <> (show' day) <> " "
            <> (show' hour) <> ":"
            <> (show' minute) <> ":"
            <> "00" <> " "
            <> (if isMorning then "AM" else "PM")

validateLocation ::
    Either Text Text -- raw location input
    -> Either Text Latitude
    -> Either Text Server.Types.Longitude
    -> ChartFormValidation Location
validateLocation loc lt lng = 
    ifS
    (Success $ isLeft lt && isLeft lng)
    (failure (InvalidLocation, "Unable to determine location coordinates"))
    (mkLocation)
    where
        invalidLocationFailure e  = Failure ((InvalidLocation, e) :| [])
        validateLocationComponent = either invalidLocationFailure Success
        tz :: Either Text TimeZoneName
        tz = do
            la <- lt
            lo <- lng
            let tzName = lookupTimeZoneName' (unLatitude la) (unLongitude lo)
            case tzName of
                Nothing -> Left "Unable to determine timezone for your location"
                Just t -> Right t

        mkLocation = Location <$> validateLocationComponent loc
                              <*> validateLocationComponent lt
                              <*> validateLocationComponent lng
                              <*> validateLocationComponent tz

-- TODO: does this belong here?
lookupTimeZoneName' :: Double -> Double -> Maybe TimeZoneName
lookupTimeZoneName' = lookupTimeZoneName "./config/timezone21.bin"
