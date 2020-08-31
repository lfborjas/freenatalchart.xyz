{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Server.Handlers where

import Import
import Server.Types
import Servant
import Lucid
import qualified Views.Index as Index
import qualified Views.About as About
import RIO.Time (defaultTimeLocale, parseTimeM, LocalTime)
import Validation (Validation(..))
import RIO.Text (pack)

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

-- params should be `Required Lenient`
-- as per: http://hackage.haskell.org/package/servant-0.15/docs/Servant-API-Modifiers.html#t:RequestArgument
validateDateParts :: 
    Either Text Year 
    -> Either Text Month
    -> Either Text Day
    -> Either Text Hour
    -> Either Text Minute
    -> Either Text Bool
    -> Validation (NonEmpty (ChartFormValidationError, Text)) DateParts
validateDateParts y m d h mn isAm =
    DateParts <$> validateDateComponent y 
              <*> validateDateComponent m
              <*> validateDateComponent d
              <*> validateDateComponent h
              <*> validateDateComponent mn
              <*> validateDateComponent isAm
    where
        invalidDateTimeFailure err = 
            Failure ( (InvalidDateTime, err) :| [])
        validateDateComponent = either invalidDateTimeFailure Success

validateDate :: 
    DateParts -> Validation (NonEmpty (ChartFormValidationError, Text)) LocalTime
validateDate DateParts{..} =
    maybe 
        (Failure ((InvalidDateTime, (pack shown) <> " is not a valid date.") :| []))
        Success
        parsedTime
    where
        parsedTime :: Maybe LocalTime
        parsedTime = parseTimeM True defaultTimeLocale "%Y-%-m-%-d %l:%-M:%-S %p" shown
        shown = 
            (show year) <> "-"
            <> (show month) <> "-"
            <> (show day) <> " "
            <> (show hour) <> ":"
            <> (show minute) <> ":"
            <> "00" <> " "
            <> (if isMorning then "AM" else "PM")
