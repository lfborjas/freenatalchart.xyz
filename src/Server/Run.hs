{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Server.Run where

import Import
    ((==), (&),  ($),
      Monad((>>)),
      Show(show),
      IsString(fromString),
      Semigroup((<>)),
      IO,
      flip,
      (^.),
      logInfo,
      runRIO,
      Proxy(..),
      MonadReader(ask),
      RIO,
      ReaderT(runReaderT),
      AppContext(appPort),
      HasEnvironment(environmentL),
      HasEphePath(ephePathL),
      HasPort(portL) )
import Server.Types ( Service )
import Server.Handlers ( service )
import Server.Middleware ( addHstsHeader, enforceHttps )
import Servant ( Application, hoistServer, serve )
import Network.Wai.Handler.Warp (run)
import Types (Environment(Production))

proxyService :: Proxy Service
proxyService = Proxy

-- running an app within a reader context, from:
-- https://docs.servant.dev/en/stable/cookbook/using-custom-monad/UsingCustomMonad.html
server :: AppContext -> Application
server ctx =
    if shouldEnforceHttps then
        enforceHttps server'
    else
        server'
    where
        shouldEnforceHttps = ctx ^. environmentL & (== Production)
        server' = addHstsHeader $ serve proxyService $ hoistServer proxyService (flip runReaderT ctx) service

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
