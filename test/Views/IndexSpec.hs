{-# LANGUAGE OverloadedStrings #-}

module Views.IndexSpec (spec) where

import TestUtil (goldenFixture,  renderHtmlToString )
import Views.Common (fixtureRenderContext)
import Views.Index (render)
import Test.Hspec ( context, describe, it, Spec )
import Server.Types (ChartFormValidationError(..), FailedChartForm(..), ChartForm(..))
import Import (NonEmpty(..), Text)

spec :: Spec
spec =
  describe "Index" $ do
    context "When initially landing, one should see the form, no errors" $ do
      it "renders the index page" $ do
        let rendered = renderHtmlToString $ render fixtureRenderContext Nothing
        goldenFixture "landing" rendered
    context "When submitting the form, if there were errors, one should see markup for said errors" $ do
      it "renders the index page, with error markup" $ do
        let form = 
              ChartForm 
                { formLocation = Right "Queens"
                , formLatitude = Left "invalid latitude"
                , formLongitude = Left "invalid longitude"
                , formYear = Left "invalid year"
                , formMonth = Left "invalid month"
                , formDay = Left "invalid day"
                , formHour = Left "invalid hour"
                , formMinute = Left "invalid minute"
                , formDayPart = Left "invalid part of day"
                }
            errors = 
              (InvalidLocation, "Unable to determine location coordinates."::Text)
              :| [(InvalidYear, "invalid year"::Text)
              , (InvalidMonth, "invalid month")
              , (InvalidDay, "invalid day")
              , (InvalidHour, "invalid hour")
              , (InvalidMinute, "invalid minute")
              , (InvalidDayPart, "invalid part of day")
              ]
            failedForm =
              FailedChartForm 
                {
                  originalForm = form
                , validationErrors = errors
                }
            rendered = renderHtmlToString $ render fixtureRenderContext (Just failedForm)
        goldenFixture "landingErrors" rendered
