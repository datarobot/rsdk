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

describe("BatchFeaturesTypeTransform", {
  featuresInfoJson <- '[{"featureType": "Numeric", "lowInformation": false, "name": "feature",
                        "uniqueCount": 200, "importance": 1, "id": 34, "naCount": 0,
                        "mean": 1, "median": 1, "stdDev": 1, "min": 1, "max": 10,
                        "timeSeriesEligible": false,
                        "timeSeriesEligibilityReason": "notADate",
                        "timeStep": null, "timeUnit": null, "targetLeakage": "SKIPPED_DETECTION"}]'

  mockPOST <- function(routeString,
                       addUrl = TRUE,
                       body = NULL,
                       returnRawResponse = FALSE, ...) {
    bodyForInspect <<- body
    "NOOP"
  }

  test_that("can feature type transform one feature", {
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
      "datarobot:::DataRobotPOST" = mockPOST,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      "datarobot:::WaitForAsyncReturn" = function(...) {
        "NOOP"
      },
      BatchFeaturesTypeTransform(
        fakeProject, "var",
        "categorical", "transform"
      )
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_equal(length(featuresInfo), 1)
    featureInfo <- featuresInfo[[1]]
    expect_equal(featureInfo$id, 34)
    expect_equal(as.character(bodyForInspect$parentNames[[1]]), "var")
    expect_equal(as.character(bodyForInspect$variableType), "categorical")
    expect_equal(as.character(bodyForInspect$prefix), "transform")
  })

  test_that("can feature type transform two features", {
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
      "datarobot:::DataRobotPOST" = mockPOST,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      "datarobot:::WaitForAsyncReturn" = function(...) {
        "NOOP"
      },
      BatchFeaturesTypeTransform(
        fakeProject, c("var1", "var2"),
        "categorical", "transform"
      )
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_equal(length(featuresInfo), 1)
    featureInfo <- featuresInfo[[1]]
    expect_equal(featureInfo$id, 34)
    expect_equal(as.character(bodyForInspect$parentNames[[1]]), "var1")
    expect_equal(as.character(bodyForInspect$parentNames[[2]]), "var2")
    expect_equal(as.character(bodyForInspect$variableType), "categorical")
    expect_equal(as.character(bodyForInspect$prefix), "transform")
  })
})
