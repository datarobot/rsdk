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

fakePct <- 20

test_that("Required parameters are present", {
  expect_error(RequestFrozenModel())
  expect_error(RequestFrozenModel(fakeModel))
  expect_error(RequestFrozenModel(samplePct = fakePct))
})


projectUrl <- UrlJoin(fakeEndpoint, "projects", fakeProjectId)
modelUrl <- UrlJoin(projectUrl, "models")
requestResponse <- httr:::response(
  url = modelUrl,
  status_code = 202L,
  headers = list(location = modelUrl),
  content = raw(0)
)

test_that("RequestFrozenModel succeeds", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RequestFrozenModel(fakeModel, samplePct = fakePct)
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(jobId, "character")
})

test_that("RequestFrozenModel succeeds with trainingRowCount", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RequestFrozenModel(fakeModel, trainingRowCount = 100)
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(jobId, "character")
})
