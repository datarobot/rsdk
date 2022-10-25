# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(testthat)
library(stubthat)

test_that("ListFeatureInfo returns correct features", {
  featuresInfoJson <- '[{"featureType": "Numeric", "lowInformation": false, "name": "feature",
                        "uniqueCount": 200, "importance": 1, "id": 34, "naCount": 0,
                        "mean": 1, "median": 1, "stdDev": 1, "min": 1, "max": 10,
                        "timeSeriesEligible": false,
                        "timeSeriesEligibilityReason": "notADate",
                        "timeStep": null, "timeUnit": null, "targetLeakage": "SKIPPED_DETECTION"}]'

  getStub <- stub(httr::GET)
  featuresInfoUrl <- UrlJoin(projectUrl, "features")
  featuresInfoResponse <- httr:::response(
    url = featuresInfoUrl,
    status_code = 200L,
    content = charToRaw(featuresInfoJson)
  )
  getStub$onCall(1)$returns(featuresInfoResponse)
  featuresInfo <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListFeatureInfo(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(length(featuresInfo), 1)
  featureInfo <- featuresInfo[[1]]
  expect_equal(featureInfo$id, 34)
  expect_equal(featureInfo$name, "feature")
  expect_equal(featureInfo$featureType, "Numeric")
  expect_equal(featureInfo$importance, 1)
  expect_false(featureInfo$lowInformation)
  expect_equal(featureInfo$uniqueCount, 200)
  expect_equal(featureInfo$naCount, 0)
  expect_equal(featureInfo$mean, 1)
  expect_equal(featureInfo$median, 1)
  expect_equal(featureInfo$stdDev, 1)
  expect_equal(featureInfo$min, 1)
  expect_equal(featureInfo$max, 10)
  expect_false(featureInfo$timeSeriesEligible)
  expect_equal(featureInfo$timeSeriesEligibilityReason, "notADate")
  expect_true(is.null(featureInfo$timeStep))
  expect_true(is.null(featureInfo$timeUnit))
  expect_equal(featureInfo$targetLeakage, "SKIPPED_DETECTION")

  expect_null(featureInfo$keySummary,
    info = "keySummary only available on summarized categorical features"
  )
})
