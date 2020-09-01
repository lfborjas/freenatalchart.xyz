{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module ValidationSpec (spec) where

import Import hiding (Longitude)
import Test.Hspec
import Server.Handlers
import Server.Types
import Data.Time
import System.IO.Unsafe (unsafePerformIO)
import Validation

localTimeFromString :: String -> LocalTime
localTimeFromString = 
    unsafePerformIO . parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T"

spec :: Spec
spec = do
    describe "validateChartForm" $ do
        it "produces birth data from parsed parameters" $ do
            let form = ChartForm {
                formLocation = Right "Queens",
                formLatitude = Right $ Latitude 40.7831,
                formLongitude = Right $ Longitude (-73.9712),
                formYear = Right $ Year 2020,
                formMonth = Right $ Month 8,
                formDay = Right $ Day 31,
                formHour = Right $ Hour 8,
                formMinute = Right $ Minute 30,
                formIsAm = Right False
            }
            let expectedBirthData = BirthData {
                birthLocation = (Location "Queens" (Latitude 40.7831) (Longitude $ -73.9712) "America/New_York"),
                birthLocalTime = (localTimeFromString "2020-08-31 20:30:00")
            }
            let parsed = validateChartForm form
            parsed `shouldBe` (Success expectedBirthData)


        it "points out form errors" $ do
            let form = ChartForm {
                formLocation = Right "Queens",
                formLatitude = Left "invalid latitude",
                formLongitude = Left "invalid longitude",
                formYear = Right $ Year 2020,
                formMonth = Right $ Month 8,
                formDay = Right $ Day 31,
                formHour = Right $ Hour 8,
                formMinute = Left "invalid minute",
                formIsAm = Right False
            }
            let parsed = validateChartForm form
            parsed `shouldBe` (Failure ((InvalidLocation,"Unable to determine location coordinates.") :| [(InvalidDateTime,"invalid minute")]))
