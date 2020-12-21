{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Server.Handlers where

import Import
import Server.Types
import Servant
import RIO.Time (defaultTimeLocale, parseTimeM, LocalTime)
import Validation (failure, Validation(..))
import RIO.Text (pack)
import Data.Coerce (coerce)
import Control.Selective (ifS)
import qualified Views.Index as Index
import qualified Views.About as About
import qualified Views.Chart as ChartPage
import Ephemeris ( HoroscopeData, Latitude, Longitude, horoscope )

service :: ServerT Service AppM
service = 
    root
    :<|> about
    :<|> chart
    :<|> (serveDirectoryWebApp "static")

root :: AppM CachedHtml
root = do
    env <- ask
    return $ cacheForOneDay $ Index.render env Nothing

chart :: ParsedParameter Text ->
    ParsedParameter Day ->
    ParsedParameter Month ->
    ParsedParameter Year ->
    ParsedParameter Hour ->
    ParsedParameter Minute ->
    ParsedParameter DayPart ->
    ParsedParameter Latitude ->
    ParsedParameter Longitude ->
    AppM (Cached TextDocument)
chart loc d m y h min' dp lt lng = do
    env <- ask
    let form = ChartForm loc lt lng y m d h min' dp
        validated = validateChartForm form
    case validated of
        Left f -> do 
            logInfo $ fromString $ show f
            return $ cacheForOneDay $ TextDocument {asHtml = (Index.render env (Just f)), asText = (pack . show $ f)}
        Right birthData -> do
            renderedChartHtml <- renderChartPage birthData (ChartPage.render)
            renderedChartText <- renderChartPage birthData (const mempty)
            return $ cacheForOneDay $ TextDocument {asHtml = renderedChartHtml, asText = renderedChartText}

--renderChartPage :: BirthData -> (BirthData -> Html ()) -> AppM (Html ())
renderChartPage :: (MonadReader t m, MonadIO m, HasTimeZoneDatabase t, HasEphePath t) => BirthData -> (t -> BirthData -> HoroscopeData -> b) -> m b
renderChartPage birthData renderer = do
    env <- ask
    let ephemerides = env ^. ephePathL
        tzDatabase  = env ^. timeZoneDatabaseL
    horoscopeData <- liftIO $ horoscope tzDatabase ephemerides birthData
    return $ renderer env birthData horoscopeData

about :: AppM CachedHtml
about = do
    env <- ask
    return $ cacheForOneDay $ About.render env

-- HANDLER HELPERS

-- TODO: should these participate in the AppM monad so they can log and such?
-- params should be `Required Lenient`
-- as per: http://hackage.haskell.org/package/servant-0.15/docs/Servant-API-Modifiers.html#t:RequestArgument

-- | Cache for a day, revalidate, but give 1 hour of "grace" while revalidating
-- TODO: add a last-modified or etag header, but also need a way of returning 304s!
-- informed by: https://csswizardry.com/2019/03/cache-control-for-civilians/
cacheForOneDay :: a -> Cached a
cacheForOneDay = addHeader "public, max-age=86400, stale-while-revalidate=3600"

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
