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

seriesAccuracyUrl <- UrlJoin(projectUrl, "datetimeModels", fakeModelId, "multiseriesScores")

test_that("it can request series accuracy", {
  postStub <- stub(httr::POST)
  seriesAccuracyResponse <- httr:::response(
    url = seriesAccuracyUrl,
    status_code = 202L,
    content = raw(0)
  )
  postStub$onCall(1)$returns(seriesAccuracyResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "httr::GET" = function() stop("Should not be called!"),
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::JobIdFromResponse" = identity,
    RequestSeriesAccuracy(fakeModel)
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_s3_class(jobId, "response")
})

test_that("it can get series accuracy from a model", {
  getStub <- stub(httr::GET)
  seriesAccuracyJson <- fileToChar("responses/seriesAccuracy.json")
  seriesAccuracyResponse <- httr:::response(
    url = seriesAccuracyUrl,
    status_code = 200L,
    content = charToRaw(seriesAccuracyJson)
  )
  getStub$onCall(1)$returns(seriesAccuracyResponse)
  seriesAccuracy <- with_mock(
    "httr::POST" = function() stop("Should not be called!"),
    "httr::GET" = getStub$f,
    "datarobot::WaitForAsyncReturn" = function(...) {
      ParseReturnResponse(waitResponse)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::JobIdFromResponse" = identity,
    GetSeriesAccuracyForModel(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(seriesAccuracy, "data.frame")
  ExpectHasKeys(seriesAccuracy, c(
    "multiseriesId", "validationScore", "rowCount",
    "multiseriesValues", "duration"
  ))
})

test_that("it can download series accuracy", {
  getStub <- stub(httr::GET)
  seriesAccuracyJson <- fileToChar("responses/seriesAccuracy.json")
  seriesAccuracyResponse <- httr:::response(
    url = seriesAccuracyUrl,
    status_code = 200L,
    content = charToRaw(seriesAccuracyJson)
  )
  getStub$onCall(1)$returns(seriesAccuracyResponse)
  expect_false(file.exists(fakeFilePath))
  with_mock(
    "httr::GET" = getStub$f,
    "httr::POST" = function(...) stop("Should not be called!"),
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DownloadSeriesAccuracy(fakeModel, fakeFilePath)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_true(file.exists(fakeFilePath))
  csv <- read.csv(fakeFilePath)
  expect_equal(nrow(csv), 11)
  expect_equal(ncol(csv), 5)
})

test_that("it can get series accuracy -- full workflow", {
  getStub <- stub(httr::GET)
  seriesAccuracyJson <- fileToChar("responses/seriesAccuracy.json")
  seriesAccuracyResponse <- httr:::response(
    url = seriesAccuracyUrl,
    status_code = 200L,
    content = charToRaw(seriesAccuracyJson)
  )
  getStub$onCall(1)$returns(seriesAccuracyResponse)
  seriesAccuracy <- with_mock(
    "httr::POST" = function() stop("Should not be called!"),
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::JobIdFromResponse" = function(...) "NOOP",
    GetSeriesAccuracy(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(seriesAccuracy, "data.frame")
  ExpectHasKeys(seriesAccuracy, c(
    "multiseriesId", "validationScore", "rowCount",
    "multiseriesValues", "duration"
  ))
})

test_that("it can get series accuracy -- full workflow with request", {
  getStub <- stub(httr::GET)
  seriesAccuracyJson <- fileToChar("responses/seriesAccuracy.json")
  errorMsg <- jsonlite::toJSON(list(message = list("404 whatever")))
  seriesAccuracyResponse <- httr:::response(
    url = seriesAccuracyUrl,
    status_code = 404L,
    content = charToRaw(errorMsg)
  )
  getStub$onCall(1)$returns(seriesAccuracyResponse)
  seriesAccuracyResponse <- httr:::response(
    url = seriesAccuracyUrl,
    status_code = 200L,
    content = charToRaw(seriesAccuracyJson)
  )
  getStub$onCall(2)$returns(seriesAccuracyResponse)
  postStub <- stub(httr::POST)
  seriesAccuracyResponse <- httr:::response(
    url = seriesAccuracyUrl,
    status_code = 202L,
    content = raw(0)
  )
  postStub$onCall(1)$returns(seriesAccuracyResponse)
  seriesAccuracy <- with_mock(
    "httr::POST" = postStub$f,
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::WaitForJobToComplete" = function(...) "NOOP",
    "datarobot:::JobIdFromResponse" = function(...) "NOOP",
    GetSeriesAccuracy(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 2)
  expect_equal(postStub$calledTimes(), 1)
  expect_s3_class(seriesAccuracy, "data.frame")
  ExpectHasKeys(seriesAccuracy, c(
    "multiseriesId", "validationScore", "rowCount",
    "multiseriesValues", "duration"
  ))
})

test_that("it can get series accuracy -- full workflow with error", {
  getStub <- stub(httr::GET)
  expect_error(with_mock(
    "httr::POST" = function(...) stop("Should not be called."),
    "httr::GET" = function(...) stop("error lol"),
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::WaitForJobToComplete" = function(...) "NOOP",
    "datarobot:::JobIdFromResponse" = function(...) "NOOP",
    GetSeriesAccuracy(fakeModel)
  ), "error lol")
})

unlink(fakeFilePath)
