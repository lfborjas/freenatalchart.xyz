{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Server.Handlers where

import Import
import Server.Types
import Servant
import Lucid
import RIO.Time (defaultTimeLocale, parseTimeM, LocalTime)
import Validation (failure, Validation(..))
import RIO.Text (pack)
import Data.Coerce (coerce)
import Control.Selective (ifS)
import qualified Views.Index as Index
import qualified Views.About as About
import qualified Views.Chart as ChartPage
import Chart.Calculations (horoscope)

service :: ServerT Service AppM
service = 
    root
    :<|> about
    :<|> chart
    :<|> (serveDirectoryWebApp "static")

root :: AppM (Html ())
root = do
    env <- ask
    return $ Index.render (Just env) Nothing

chart :: ParsedParameter Text ->
    ParsedParameter Day ->
    ParsedParameter Month ->
    ParsedParameter Year ->
    ParsedParameter Hour ->
    ParsedParameter Minute ->
    ParsedParameter DayPart ->
    ParsedParameter Latitude ->
    ParsedParameter Longitude ->
    AppM (Html ())
chart loc d m y h min' dp lt lng = do
    env <- ask
    let form = ChartForm loc lt lng y m d h min' dp
        validated = validateChartForm form
    case validated of
        Left f -> do 
            logInfo $ fromString $ show f
            return $ Index.render (Just env) (Just f)
        Right birthData -> do
            renderChartPage birthData

renderChartPage :: BirthData -> AppM (Html ())
renderChartPage birthData = do
    env <- ask
    let ephemerides = env ^. ephePathL
        tzDatabase  = env ^. timeZoneDatabaseL
    horoscopeData <- liftIO $ horoscope tzDatabase ephemerides birthData
    return $ ChartPage.render birthData horoscopeData

about :: AppM (Html ())
about = return $ About.render

-- HANDLER HELPERS

-- TODO: should these participate in the AppM monad so they can log and such?
-- params should be `Required Lenient`
-- as per: http://hackage.haskell.org/package/servant-0.15/docs/Servant-API-Modifiers.html#t:RequestArgument

---
--- DATE VALIDATION
---

validateDateParts :: ParsedParameter Year 
    -> ParsedParameter Month
    -> ParsedParameter Day
    -> ParsedParameter Hour
    -> ParsedParameter Minute
    -> ParsedParameter DayPart
    -> ChartFormValidation DateParts
validateDateParts y m d h mn isAm =
    DateParts <$> validateDateComponent InvalidYear y 
              <*> validateDateComponent InvalidMonth m
              <*> validateDateComponent InvalidDay d
              <*> validateDateComponent InvalidHour h
              <*> validateDateComponent InvalidMinute mn
              <*> validateDateComponent InvalidDayPart isAm
    where
        invalidDateTimeFailure f err = failure (f, err)
        validateDateComponent e = either (invalidDateTimeFailure e) Success

validateDateTime :: ChartFormValidation DateParts -> ChartFormValidation LocalTime
validateDateTime (Failure e) = Failure e
validateDateTime (Success dp) = 
    maybe 
        (failure (InvalidDateTime, (pack $ formatDateParts dp) <> " is not a valid date."))
        Success
        (parseTimeM True defaultTimeLocale "%Y-%-m-%-d %l:%-M:%-S %P" (formatDateParts dp))

formatDateParts :: DateParts -> String
formatDateParts DateParts{..} =
    (show' year) <> "-"
    <> (show' month) <> "-"
    <> (show' day) <> " "
    <> (show' hour) <> ":"
    <> (show' minute) <> ":"
    <> "00" <> " "
    <> (unDayPart dayPart)
    where
        show' x = show $ (coerce x :: Int)

---
--- LOCATION VALIDATION
---

validateLocation ::
    ParsedParameter Text -- raw location input
    -> ParsedParameter Latitude
    -> ParsedParameter Longitude
    -> ChartFormValidation Location
validateLocation loc lt lng = 
    ifS (Success $ isLeft lt && isLeft lng)
        (failure (InvalidLocation, "Unable to determine location coordinates."))
        (mkLocation)
    where
        invalidLocationFailure e  = failure (InvalidLocation, e)
        validateLocationComponent = either invalidLocationFailure Success
        mkLocation = Location <$> validateLocationComponent loc
                              <*> validateLocationComponent lt
                              <*> validateLocationComponent lng

-- | Given the form as it comes from the request, apply all possible validations to ensure:
-- we have legitimate coordinates, and a local time. If we succeed, a `BirthData`
-- is produced with necessary data for calculations. If not, a partial form will be returned.
validateChartForm :: ChartForm -> Either FailedChartForm BirthData
validateChartForm original@(ChartForm{..}) = do
    let validatedLocation = validateLocation formLocation formLatitude formLongitude
        validatedDateParts = validateDateParts formYear formMonth formDay formHour formMinute formDayPart
        validatedTime = validateDateTime validatedDateParts
        validatedForm = BirthData <$> validatedLocation <*> validatedTime
    case validatedForm of
        Failure errs -> Left $ FailedChartForm original errs
        Success bData -> Right bData

---
--- Pièce de Résistance
--- 

--calculateHoroscope :: BirthData -> IO HoroscopeData
--calculateHoroscope 
