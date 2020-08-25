{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Server.Run (start)
import System.Envy (decodeWithDefaults)

main :: IO ()
main = do
  lo <- logOptionsHandle stderr False
  env <- decodeWithDefaults defaultConfig
  let logOptions = setLogUseTime True lo
  withLogFunc logOptions $ \lf ->
    let ctx = AppContext 
          {
            appLogFunc = lf
          , appPort = port env
          , appEphePath = ephePath env
          }
    in
      start ctx
