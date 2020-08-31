{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Server.Handlers where

import Import
import Server.Types
import Servant
import Lucid
import qualified Views.Index as Index
import qualified Views.About as About
import RIO.Time (LocalTime)
import Validation (Validation(..))

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
validateDate :: 
    Either Text Year 
    -> Either Text Month
    -> Either Text Day
    -> Either Text Hour
    -> Either Text Minute
    -> Either Text Bool
    -> Validation (NonEmpty (ChartFormValidationError, Text)) DateParts
validateDate y m d h mn isAm =
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
