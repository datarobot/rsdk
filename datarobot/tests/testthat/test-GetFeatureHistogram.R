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

test_that("GetFeatureHistogram works", {
  getStub <- stub(httr::GET)
  featureHistogramUrl <- UrlJoin(projectUrl, "featureHistograms", fakeFeature)
  featureHistogramJson <- fileToChar("responses/GetFeatureHistogram.json")
  featureHistogramResponse <- httr:::response(
    url = featureHistogramUrl,
    status_code = 200L,
    content = charToRaw(featureHistogramJson)
  )
  getStub$onCall(1)$returns(featureHistogramResponse)
  histogram <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetFeatureHistogram(fakeProject, fakeFeature)
  )
  expect_type(histogram, "list")
  expect_equal(length(histogram), 58)
  expect_type(histogram[[1]], "list")
  ExpectHasKeys(histogram[[1]], c("count", "target", "label"))
  expect_type(histogram[[1]]$count, "integer")
  expect_true(is.null(histogram[[1]]$target))
  expect_type(histogram[[1]]$label, "character")
})

test_that("GetFeatureHistogram works with binLimit", {
  getStub <- stub(httr::GET)
  featureHistogramUrl <- UrlJoin(projectUrl, "featureHistograms", fakeFeature)
  featureHistogramJson <- fileToChar("responses/GetFeatureHistogram.json")
  featureHistogramResponse <- httr:::response(
    url = featureHistogramUrl,
    status_code = 200L,
    content = charToRaw(featureHistogramJson)
  )
  getStub$onCall(1)$returns(featureHistogramResponse)
  histogram <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::DataRobotGET" = function(routeString,
                                          addUrl = TRUE,
                                          query = NULL,
                                          returnRawResponse = FALSE, ...) {
      queryForInspect <<- query
      datarobot:::MakeDataRobotRequest(httr::GET, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        query = query, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetFeatureHistogram(fakeProject, fakeFeature, binLimit = 2)
  )
  expect_type(histogram, "list")
  expect_equal(length(histogram), 58)
  expect_type(histogram[[1]], "list")
  ExpectHasKeys(histogram[[1]], c("count", "target", "label"))
  expect_type(histogram[[1]]$count, "integer")
  expect_true(is.null(histogram[[1]]$target))
  expect_type(histogram[[1]]$label, "character")
  expect_equal(queryForInspect$binLimit, 2)
})
