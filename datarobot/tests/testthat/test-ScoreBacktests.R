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

describe("ScoreBacktests", {
  test_that("Required parameters are present", {
    expect_error(ScoreBacktests())
  })

  test_that("Function works with project and blueprint", {
    postStub <- stub(httr::POST)
    postBacktestUrl <- UrlJoin(projectUrl, "datetimeModels", fakeModelId, "backtests")
    postBacktestJson <- fileToChar("responses/scoreBacktests.json")
    datetimeModelResponse <- httr:::response(
      url = postBacktestUrl,
      status_code = 200L,
      content = charToRaw(postBacktestJson)
    )
    postStub$onCall(1)$returns(datetimeModelResponse)
    jobId <- with_mock(
      "httr::POST" = postStub$f,
      "httr::GET" = function() stop("Should not be called!"),
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      "datarobot:::JobIdFromResponse" = identity,
      ScoreBacktests(fakeModel)
    )
    expect_equal(postStub$calledTimes(), 1)
    expect_s3_class(jobId, "response")
  })

  test_that("Function works with wait", {
    postStub <- stub(httr::POST)
    postBacktestUrl <- UrlJoin(projectUrl, "datetimeModels", fakeModelId, "backtests")
    postBacktestJson <- fileToChar("responses/scoreBacktests.json")
    datetimeModelResponse <- httr:::response(
      url = postBacktestUrl,
      status_code = 200L,
      content = charToRaw(postBacktestJson)
    )
    postStub$onCall(1)$returns(datetimeModelResponse)
    response <- with_mock(
      "httr::POST" = postStub$f,
      "httr::GET" = function() stop("Should not be called!"),
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      "datarobot:::WaitForJobToComplete" = function(...) NULL,
      "datarobot:::JobIdFromResponse" = identity,
      ScoreBacktests(fakeModel, wait = TRUE)
    )
    expect_equal(postStub$calledTimes(), 1)
    expect_true(is.null(response))
  })
})
