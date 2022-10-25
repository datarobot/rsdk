# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(stubthat)
library(testthat)


test_that("it can get a feature log", {
  getStub <- stub(httr::GET)
  listFeatureLogsUrl <- UrlJoin(projectUrl, "timeSeriesFeatureLog")
  listFeatureLogJson <- fileToChar("responses/ListFeatureLog.json")
  featureLogResponse <- httr:::response(
    url = listFeatureLogsUrl,
    status_code = 200L,
    content = charToRaw(listFeatureLogJson)
  )
  getStub$onCall(1)$returns(featureLogResponse)
  featureLog <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetTimeSeriesFeatureDerivationLog(fakeProjectId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(featureLog, "Feature Log goes here")
})

test_that("It can download a feature log", {
  expect_false(file.exists(fakeFilePath))
  getStub <- stub(httr::GET)
  getFeatureLogUrl <- UrlJoin(projectUrl, "timeSeriesFeatureLog", "file")
  getFeatureLogTxt <- fileToChar("responses/GetFeatureLog.txt")
  featureLogResponse <- httr:::response(
    url = getFeatureLogUrl,
    status_code = 200L,
    content = charToRaw(getFeatureLogTxt)
  )
  getStub$onCall(1)$returns(featureLogResponse)
  featureLog <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DownloadTimeSeriesFeatureDerivationLog(fakeProjectId, fakeFilePath)
  )
  expect_equal(getStub$calledTimes(), 1)
})
