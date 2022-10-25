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

test_that("GetFeatureInfo works on a date", {
  fakeDateFeature <- "fake feature date"
  dateFeatureInfoJson <- '{"featureType": "Date", "lowInformation": false,
                         "name": "fake feature date", "uniqueCount": 200, "importance": 1,
                         "id": 35, "naCount": 0, "mean": "2015-05-27",
                         "median": "2015-05-29", "stdDev": "200.504 days",
                         "min": "2014-06-10", "max": "2016-05-06",
                         "timeSeriesEligible": true,
                         "timeSeriesEligibilityReason": "suitable",
                         "timeStep": 1, "timeUnit": "DAY", "targetLeakage": "SKIPPED_DETECTION"}'

  getStub <- stub(httr::GET)
  featureInfoUrl <- UrlJoin(projectUrl, "features", fakeDateFeature)
  featureInfoResponse <- httr:::response(
    url = featureInfoUrl,
    status_code = 200L,
    content = charToRaw(dateFeatureInfoJson)
  )
  getStub$onCall(1)$returns(featureInfoResponse)
  featureInfo <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetFeatureInfo(fakeProject, fakeDateFeature)
  )
  expect_equal(featureInfo$id, 35)
  expect_equal(featureInfo$name, fakeDateFeature)
  expect_equal(featureInfo$featureType, "Date")
  expect_equal(featureInfo$importance, 1)
  expect_false(featureInfo$lowInformation)
  expect_equal(featureInfo$uniqueCount, 200)
  expect_equal(featureInfo$naCount, 0)
  expect_equal(featureInfo$mean, "2015-05-27")
  expect_equal(featureInfo$median, "2015-05-29")
  expect_equal(featureInfo$stdDev, "200.504 days")
  expect_equal(featureInfo$min, "2014-06-10")
  expect_equal(featureInfo$max, "2016-05-06")
  expect_true(featureInfo$timeSeriesEligible)
  expect_equal(featureInfo$timeSeriesEligibilityReason, "suitable")
  expect_equal(featureInfo$timeStep, 1)
  expect_equal(featureInfo$timeUnit, "DAY")
  expect_equal(featureInfo$targetLeakage, "SKIPPED_DETECTION")
})

test_that("GetFeatureInfo returns keySummary for a summarized categorical variable", {
  sumCatFeatureName <- "bids[device] (counts) (by bidder_id)"
  sumCatFeatureJson <- fileToChar("responses/featureSummarizedCategorical.json")

  getStub <- stub(httr::GET)
  featureInfoUrl <- UrlJoin(projectUrl, "features", sumCatFeatureName)
  featureInfoResponse <- httr:::response(
    url = featureInfoUrl,
    status_code = 200L,
    content = charToRaw(sumCatFeatureJson)
  )
  getStub$onCall(1)$returns(featureInfoResponse)

  featureInfo <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetFeatureInfo(fakeProject, sumCatFeatureName)
  )

  keySummary <- featureInfo$keySummary
  expect_false(is.null(keySummary))
  expect_s3_class(keySummary, "data.frame")
  expect_setequal(
    names(keySummary),
    c("key", "summary")
  )

  expect_type(keySummary$key, "character")
  expect_true(Contains(keySummary$key, c("phone1", "phone2")),
    info = "The key summary should contain 'phone1' and 'phone2' stat keys"
  )
  expect_true(Contains(keySummary$key, " "),
    info = "Per SAFER-1028 the key summary can contain keys that are blank strings"
  )
  expect_true(Contains(keySummary$key, "%"),
    info = "Per SAFER-1028 the key summary can contain
                      keys that are not alphanumeric strings"
  )

  summary <- keySummary$summary
  expect_s3_class(summary, "data.frame")
  expect_setequal(
    names(summary),
    c("min", "max", "median", "mean", "stdDev", "pctRows")
  )
})
