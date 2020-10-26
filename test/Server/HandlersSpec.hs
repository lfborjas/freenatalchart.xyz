{-# LANGUAGE OverloadedStrings #-}

module Server.HandlersSpec (spec) where

import Import
    (mkLogFunc,
      LogFunc,
      AppContext(..),
      Environment(Test, Production) )
import TestUtil (toStrict, safeToString,  testEphe, testTzDB )
import Network.Wai (Application)
import Test.Hspec ( describe, it, Spec )
import Test.Hspec.Wai
    (request, MatchHeader, (<:>), Body,  get,
      shouldRespondWith,
      with,
      MatchBody(..),
      ResponseMatcher(ResponseMatcher, matchStatus, matchHeaders,
                      matchBody) )
import Server.Run (server)
import Data.Time.LocalTime.TimeZone.Detect (openTimeZoneDatabase, TimeZoneDatabase)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (isInfixOf)
import Network.HTTP.Types (methodGet)
import Network.HTTP.Types.Header (hHost)

noLog :: LogFunc
noLog = mkLogFunc $ (\_ _ _ _ -> pure ())

-- | Ignore the response body: presumably we're testing
-- its generation elsewhere.
matchAny :: MatchBody
matchAny = MatchBody (\_ _ -> Nothing)


-- Modified matcher from:
-- https://github.com/hspec/hspec-wai/blob/67da96a2d7465a876389c197ba1e1f0cd1270ba0/src/Test/Hspec/Wai/Matcher.hs#L40-L49
bodyContains :: String -> MatchBody
bodyContains substr = MatchBody (\_ actual -> bodyMatcher actual substr)
  where
    bodyMatcher :: Body -> String -> Maybe String
    bodyMatcher actual sub = do
      case safeToString . toStrict $ actual of
         Just responseBody -> do 
           if (sub `isInfixOf` responseBody) then
             Nothing
           else
             pure $ "body does not contain:" <> sub
         Nothing -> pure "Unable to parse response body"


tzDB :: TimeZoneDatabase
tzDB = unsafePerformIO $ openTimeZoneDatabase testTzDB
{-# NOINLINE tzDB #-}

testApp' :: Environment -> IO Application
testApp' runAs = do
   let ctx = AppContext
        {
          appLogFunc = noLog
        , appPort = 3033
        , appEphePath = testEphe
        , appAlgoliaAppId = ""
        , appAlgoliaAppKey = ""
        , appTimeZoneDatabase = tzDB
        , appEnvironment = runAs
        , appStaticRoot = "/"
        }
    in
      pure $ server ctx

testApp :: IO Application
testApp = testApp' Test
prodApp :: IO Application
prodApp = testApp' Production

expectedHeaders :: [MatchHeader]
expectedHeaders = 
  [
    "Content-Type" <:> "text/html;charset=utf-8",
    "Cache-Control" <:> "public, max-age=86400, stale-while-revalidate=3600",
    "Strict-Transport-Security" <:> "max-age=63072000; includeSubdomains; preload"
  ]

testHost = "test.freenatalchart.xyz"

spec :: Spec
spec = do
  describe "General business logic" routesSpec
  describe "Production-specific logic (redirects, caching, etc.)" prodSpec

routesSpec :: Spec
routesSpec =
  with testApp $ do
    describe "GET /" $ do
      it "returns the index page" $ do
        get "/" `shouldRespondWith` ResponseMatcher 
          { matchStatus = 200
          , matchHeaders = expectedHeaders
          , matchBody = matchAny
          }

    describe "GET /full-chart" $ do
      it "returns the chart page" $ do
        let exampleChart = "location=Tegucigalpa%2C+Francisco+Morazán%2C+Honduras&month=1&day=6&year=1989&hour=12&minute=30&day-part=am&lat=14.0932&lng=-87.2013"
        get ("/full-chart?" <> exampleChart) `shouldRespondWith`
          ResponseMatcher
            { matchStatus = 200
            , matchHeaders = expectedHeaders
            -- very simplistic check, see View specs for
            -- more thorough verification of response templates
            , matchBody = bodyContains "Planet Positions"
            }


prodSpec :: Spec
prodSpec =
  with prodApp $ do
    describe "GET / through reverse proxy (i.e. Heroku)" $ do
      it "redirects if no proto header is present" $ do
        request methodGet "/" [(hHost, testHost)] "" `shouldRespondWith`
          ResponseMatcher
            { matchStatus = 301
            , matchHeaders = ["Location" <:> "https://test.freenatalchart.xyz"]
            , matchBody = matchAny
            }
      it "redirects if requested via http" $ do
        req <- pure $ request methodGet "/" [(hHost, testHost), ("X-Forwarded-Proto", "http")] ""
        req `shouldRespondWith`
          ResponseMatcher
            { matchStatus = 301
            , matchHeaders = ["Location" <:> "https://test.freenatalchart.xyz"]
            , matchBody = matchAny
            }        
      it "returns a successful response if requested via https" $ do
        request methodGet "/" [(hHost, testHost), ("X-Forwarded-Proto", "https")] "" `shouldRespondWith`
          ResponseMatcher
            { matchStatus = 200
            , matchHeaders = expectedHeaders
            , matchBody = matchAny
            }
