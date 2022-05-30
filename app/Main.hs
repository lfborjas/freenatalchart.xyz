{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Server.Run (start)
import System.Envy (decodeWithDefaults)
import Data.Time.LocalTime.TimeZone.Detect (withTimeZoneDatabase)

main :: IO ()
main = do
  lo <- logOptionsHandle stderr False
  env' <- decodeWithDefaults defaultConfig
  let logOptions = setLogUseTime True lo
  withLogFunc logOptions $ \lf ->
    withTimeZoneDatabase (timezoneDatabaseFile env') $ \tzdb ->
      let ctx = AppContext 
            {
              appLogFunc = lf
            , appPort = port env'
            , appEphePath = ephePath env'
            , appGeocodeApiKey = geocodeApiKey env'
            , appTimeZoneDatabase = tzdb
            , appEnvironment = deployEnv env'
            , appStaticRoot = "/"
            , appEphemerisDatabase = epheDbFile env'
            }
      in
        start ctx
