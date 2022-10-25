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

test_that("RequestNewFrozenDatetimeModel works", {
  postStub <- stub(httr::POST)
  postDatetimeModelUrl <- UrlJoin(projectUrl, "frozenDatetimeModels")
  datetimeModelResponse <- httr:::response(
    url = postDatetimeModelUrl,
    status_code = 202L,
    headers = list(location = jobUrl),
    content = raw(0)
  )
  postStub$onCall(1)$returns(datetimeModelResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "httr::GET" = function() stop("Should not be called!"),
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::JobIdFromResponse" = identity,
    RequestFrozenDatetimeModel(fakeModel, fakeDatetimeModelTrainingRowCount)
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_s3_class(jobId, "response")
})

test_that("RequestNewFrozenDatetimeModel works with samplePct", {
  postStub <- stub(httr::POST)
  postDatetimeModelUrl <- UrlJoin(projectUrl, "datetimeModels")
  datetimeModelResponse <- httr:::response(
    url = postDatetimeModelUrl,
    status_code = 202L,
    headers = list(location = jobUrl),
    content = raw(0)
  )
  postStub$onCall(1)$returns(datetimeModelResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "httr::GET" = function() stop("Should not be called!"),
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::JobIdFromResponse" = identity,
    RequestFrozenDatetimeModel(
      fakeModel,
      trainingDuration = ConstructDurationString(days = 20),
      timeWindowSamplePct = 90
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_s3_class(jobId, "response")
})

test_that("Required parameters are present", {
  expect_error(
    RequestFrozenDatetimeModel(trainingRowCount = 20),
    'argument "model" is missing, with no default'
  )
  expect_error(
    RequestFrozenDatetimeModel(trainingRowCount = fakeDatetimeModelTrainingRowCount),
    'argument "model" is missing, with no default'
  )
})
