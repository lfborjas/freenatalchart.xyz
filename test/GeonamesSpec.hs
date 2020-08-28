{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module GeonamesSpec (spec) where

import Import
import Test.Hspec
import ExternalServices.Geonames
import Data.Aeson

spec :: Spec
spec = do
    describe "decode errors" $ do
        it "recognizes an api error as such" $ do
            let apiError = Just (APIError (GeonamesError {errorCode = AuthorizationException, errorMessage = "user does not exist."}))
                decoded :: Maybe (APIResult TimezoneResponse)
                decoded = decode "{\"status\":{\"message\":\"user does not exist.\",\"value\":10}}"
            decoded `shouldBe` apiError

        it "recognizes a successful response as such" $ do
            let apiSuccess = Just (APIResponse (TimezoneResponse {sunrise = "2020-08-28 05:37", sunset = "2020-08-28 18:02", lat = 14.0839053, lng = -87.2750137, countryCode = "HN", gmtOffset = -6.0, rawOffset = -6.0, timezoneId = "America/Tegucigalpa", dstOffset = -6.0, countryName = "Honduras", time = "2020-08-27 19:39"}))
                decoded :: Maybe (APIResult TimezoneResponse)
                decoded = decode "{\"sunrise\":\"2020-08-28 05:37\",\"lng\":-87.2750137,\"countryCode\":\"HN\",\"gmtOffset\":-6,\"rawOffset\":-6,\"sunset\":\"2020-08-28 18:02\",\"timezoneId\":\"America/Tegucigalpa\",\"dstOffset\":-6,\"countryName\":\"Honduras\",\"time\":\"2020-08-27 19:39\",\"lat\":14.0839053}"
            decoded `shouldBe` apiSuccess
