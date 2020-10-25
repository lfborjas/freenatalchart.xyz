{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Server.Run where

import Import
import Server.Types
import Server.Handlers
import Servant
import Network.Wai.Handler.Warp (run)

proxyService :: Proxy Service
proxyService = Proxy

-- running an app within a reader context, from:
-- https://docs.servant.dev/en/stable/cookbook/using-custom-monad/UsingCustomMonad.html
server :: AppContext -> Application
server ctx =
    serve proxyService $ hoistServer proxyService (flip runReaderT ctx) service

imAlive :: RIO AppContext ()
imAlive = do
    env <- ask
    let p = env ^. portL
        ephe = env ^. ephePathL
        env' = env ^. environmentL
    logInfo $ fromString $ "[" <> (show env') <> "] " <> "Running on: " <> (show p) <> " with ephe path: " <> ephe

start :: AppContext -> IO ()
start ctx =
    (runRIO ctx imAlive) >> (run (appPort ctx) $ server ctx)
