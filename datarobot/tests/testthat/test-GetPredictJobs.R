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

predictJobs <- data.frame(
  status = c("queue", "queue"),
  projectId = c("5698223fc80891501c445f15", "5698223fc80891501c445f15"),
  id = c("67", "68"),
  modelId = c("569822cc3b86bf0cb7c0fde4", "569822cc3b86bf0cb7c0fde4")
)

test_that("it can get a predict job", {
  getStub <- stub(httr::GET)
  getPredictJobUrl <- UrlJoin(projectUrl, "predictJobs")
  predictJobResponse <- httr:::response(
    url = getPredictJobUrl,
    status_code = 200L,
    content = charToRaw(jsonlite::toJSON(predictJobs[1, ]))
  )
  getStub$onCall(1)$returns(predictJobResponse)
  predictJob <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictJob(fakeProject, fakeJobId)
  )
  expect_equal(getStub$calledTimes(), 1)
  ExpectHasKeys(predictJob, c("status", "projectId", "predictJobId", "modelId"))
})

test_that("it can get predict jobs", {
  getStub <- stub(httr::GET)
  getPredictJobUrl <- UrlJoin(projectUrl, "predictJobs")
  predictJobResponse <- httr:::response(
    url = getPredictJobUrl,
    status_code = 200L,
    content = charToRaw(jsonlite::toJSON(predictJobs))
  )
  getStub$onCall(1)$returns(predictJobResponse)
  predictJobs <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictJobs(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 1)
  ExpectHasKeys(predictJobs, c("status", "projectId", "predictJobId", "modelId"))
  expect_equal(nrow(predictJobs), 2)
})

test_that("it can get predict jobs with non-default status", {
  getStub <- stub(httr::GET)
  getPredictJobUrl <- UrlJoin(projectUrl, "predictJobs")
  predictJobResponse <- httr:::response(
    url = getPredictJobUrl,
    status_code = 200L,
    content = charToRaw(jsonlite::toJSON(predictJobs))
  )
  getStub$onCall(1)$returns(predictJobResponse)
  predictJobs <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictJobs(fakeProject, status = "queue")
  )
  expect_equal(getStub$calledTimes(), 1)
  ExpectHasKeys(predictJobs, c("status", "projectId", "predictJobId", "modelId"))
  expect_equal(nrow(predictJobs), 2)
})

test_that("it can get predict jobs with error status", {
  getStub <- stub(httr::GET)
  getPredictJobUrl <- UrlJoin(projectUrl, "predictJobs")
  emptyPredictJobs <- data.frame(status = NULL, projectId = NULL, modelId = NULL, id = NULL)
  predictJobResponse <- httr:::response(
    url = getPredictJobUrl,
    status_code = 200L,
    content = charToRaw(jsonlite::toJSON(emptyPredictJobs))
  )
  getStub$onCall(1)$returns(predictJobResponse)
  predictJobs <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictJobs(fakeProject, status = "error")
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_setequal(predictJobs, list())
})
