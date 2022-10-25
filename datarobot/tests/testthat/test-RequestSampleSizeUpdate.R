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

test_that("Required parameters are present", {
  expect_error(RequestSampleSizeUpdate())
  expect_error(RequestSampleSizeUpdate(model))
  expect_error(RequestSampleSizeUpdate(samplePct = newPct))
})

test_that("With modelId only", {
  expect_error(RequestSampleSizeUpdate(model$modelId))
})

test_that("With non-list model", {
  expect_error(RequestSampleSizeUpdate("Not a model object"))
})


projectUrl <- UrlJoin(fakeEndpoint, "projects", fakeProjectId)
modelUrl <- UrlJoin(projectUrl, "models")
requestResponse <- httr:::response(
  url = modelUrl,
  status_code = 202L,
  headers = list(location = modelUrl),
  content = raw(0)
)

test_that("RequestSampleSizeUpdate succeeds", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  newPct <- 93
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RequestSampleSizeUpdate(fakeModel, samplePct = newPct)
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(jobId, "character")
})


test_that("RequestSampleSizeUpdate succeeds with trainingRowCount", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  newRowCount <- 200
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RequestSampleSizeUpdate(fakeModel, trainingRowCount = newRowCount)
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(jobId, "character")
})
