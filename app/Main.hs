{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Server.Run (start)
import System.Envy (decodeWithDefaults)
import Data.Time.LocalTime.TimeZone.Detect (withTimeZoneDatabase)


main :: IO ()
main = do
  lo <- logOptionsHandle stderr False
  env <- decodeWithDefaults defaultConfig
  let logOptions = setLogUseTime True lo
  withLogFunc logOptions $ \lf ->
    withTimeZoneDatabase (timezoneDatabaseFile env) $ \tzdb ->
      let ctx = AppContext 
            {
              appLogFunc = lf
            , appPort = port env
            , appEphePath = ephePath env
            , appAlgoliaAppId = algoliaAppId env
            , appAlgoliaAppKey = algoliaAppKey env
            , appTimeZoneDatabase = tzdb
            }
      in
        start ctx
