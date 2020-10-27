{-# LANGUAGE OverloadedStrings #-}
module Server.Middleware (addHstsHeader,enforceHttps) where

import Import ( Bifunctor(first), ByteString )
import Network.Wai (mapResponseHeaders, Middleware, modifyResponse)
import Network.Wai.Middleware.EnforceHTTPS
    ( withResolver, xForwardedProto )
import qualified Data.CaseInsensitive as CI

-- | Add strict transport security headers,
-- as per:
-- * https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Strict-Transport-Security
-- * https://hstspreload.org/
-- inspired by:
-- https://hackage.haskell.org/package/kawaii-0.0.1.0/docs/src/Network-Wai-Serve-Middleware.html#stsHeadersMiddleware
-- TODO: add `preload` later on.
addHstsHeader :: Middleware
addHstsHeader = 
  addHeaders [("Strict-Transport-Security", "max-age=63072000")]

enforceHttps :: Middleware
enforceHttps = withResolver xForwardedProto

-- from:
-- http://hackage.haskell.org/package/wai-extra-3.1.1/docs/src/Network.Wai.Middleware.AddHeaders.html#addHeaders
addHeaders :: [(ByteString, ByteString)] -> Middleware
addHeaders h = 
  modifyResponse $ addHeaders' (map (first CI.mk) h)
  where
    addHeaders' h' = mapResponseHeaders (\hs -> h' ++ hs)
