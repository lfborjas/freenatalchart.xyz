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
    ParsedParameter Year 
    -> ParsedParameter Month
    -> ParsedParameter Day
    -> ParsedParameter Hour
    -> ParsedParameter Minute
    -> ParsedParameter Bool
    -> ChartFormValidation LocalTime
validateDate y m d h mn isAm =
    parseTime dateParts
    where
        -- TODO: do we need a different ChartFormValidationError for _each part_? possible here,
        -- just tedious.
        invalidDateTimeFailure err = failure (InvalidDateTime, err)
        validateDateComponent = either invalidDateTimeFailure Success
        dateParts = DateParts <$> validateDateComponent y 
                              <*> validateDateComponent m
                              <*> validateDateComponent d
                              <*> validateDateComponent h
                              <*> validateDateComponent mn
                              <*> validateDateComponent isAm

parseTime :: ChartFormValidation DateParts -> ChartFormValidation LocalTime
parseTime (Failure e) = Failure e
parseTime (Success dp) = 
    maybe 
        (failure (InvalidDateTime, (pack $ formatDateParts dp) <> " is not a valid date."))
        Success
        (parseTimeM True defaultTimeLocale "%Y-%-m-%-d %l:%-M:%-S %p" (formatDateParts dp))

formatDateParts :: DateParts -> String
formatDateParts DateParts{..} =
    (show' year) <> "-"
    <> (show' month) <> "-"
    <> (show' day) <> " "
    <> (show' hour) <> ":"
    <> (show' minute) <> ":"
    <> "00" <> " "
    <> (if isMorning then "AM" else "PM")
    where
        show' x = show $ (coerce x :: Integer)

validateLocation ::
    ParsedParameter Text -- raw location input
    -> ParsedParameter Latitude
    -> ParsedParameter Server.Types.Longitude
    -> ChartFormValidation Location
validateLocation loc lt lng = 
    ifS (Success $ isLeft lt && isLeft lng)
        (failure (InvalidLocation, "Unable to determine location coordinates."))
        (mkLocation)
    where
        invalidLocationFailure e  = failure (InvalidLocation, e)
        validateLocationComponent = either invalidLocationFailure Success
        -- TODO: validate location to not be empty? Since we're not doing
        -- a fallback to a server-side lookup right now, that's not necessary.
        mkLocation = Location <$> validateLocationComponent loc
                              <*> validateLocationComponent lt
                              <*> validateLocationComponent lng
                              <*> validateLocationComponent (getTimeZone lt lng)

getTimeZone :: ParsedParameter Latitude -> ParsedParameter Server.Types.Longitude -> ParsedParameter TimeZoneName
getTimeZone lt lng = do
    la <- lt
    lo <- lng
    let tzName = lookupTimeZoneName' (unLatitude la) (unLongitude lo)
    case tzName of
        Nothing -> Left "Unable to determine timezone for your location."
        Just t -> Right t

-- | Given the form as it comes from the request, apply all possible validations to ensure:
-- we have legitimate coordinates, timezone and a local time. If at any point we fail,
-- a nonempty list of validation errors will be produced, which we should deal with in the form.
validateChartForm :: ChartForm -> ChartFormValidation BirthData
validateChartForm ChartForm{..} =
    BirthData <$> validatedLocation
              <*> validatedDateTime
    where
        validatedLocation = 
            validateLocation formLocation 
                             formLatitude
                             formLongitude
        validatedDateTime =
            validateDate formYear
                         formMonth
                         formDay
                         formHour
                         formMinute
                         formIsAm

-- TODO: does this belong here?
lookupTimeZoneName' :: Double -> Double -> Maybe TimeZoneName
lookupTimeZoneName' = lookupTimeZoneName "./config/timezone21.bin"
