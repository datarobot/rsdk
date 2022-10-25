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

modelJobUrl <- UrlJoin(projectUrl, "modelJobs", fakeJobId)
expectedKeys <- c(
  "status", "processes", "projectId", "modelId", "samplePct",
  "trainingRowCount", "modelType", "featurelistId",
  "modelCategory", "blueprintId", "modelJobId",
  "modelJobId", "isBlocked"
)
modelJobJson <- fileToChar("responses/modelJobInfo.json")

test_that("GetModelJob can fetch a completed job", {
  completeModelJobResponse <- httr:::response(
    url = modelJobUrl,
    status_code = 303L,
    headers = list(location = UrlJoin(
      projectUrl,
      "someRedirect"
    )),
    content = charToRaw(modelJobJson)
  )
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completeModelJobResponse)
  response <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetModelJob(fakeProject, fakeJobId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(response, "list")
  ExpectHasKeys(response, expectedKeys)
  expect_type(response$status, "character")
  expect_type(response$projectId, "character")
  expect_type(response$modelId, "character")
  expect_type(response$modelJobId, "character")
  expect_type(response$processes, "character")
  expect_type(response$samplePct, "double")
  expect_type(response$trainingRowCount, "integer")
  expect_type(response$modelType, "character")
  expect_type(response$featurelistId, "character")
  expect_type(response$modelCategory, "character")
  expect_type(response$blueprintId, "character")
  expect_type(response$isBlocked, "logical")
})

test_that("GetModelJob can fetch a queued job", {
  tmp <- fromJSON(modelJobJson)
  tmp$status <- JobStatus$Queue
  modelJobJson <- toJSON(tmp)

  completeModelJobResponse <- httr:::response(
    url = modelJobUrl,
    status_code = 200L,
    content = charToRaw(modelJobJson)
  )
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completeModelJobResponse)
  response <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetModelJob(fakeProject, fakeJobId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(response, "list")
  ExpectHasKeys(response, expectedKeys)
  expect_type(response$status, "character")
  expect_type(response$projectId, "character")
  expect_type(response$modelId, "character")
  expect_type(response$modelJobId, "character")
  expect_type(response$processes, "character")
  expect_type(response$samplePct, "double")
  expect_type(response$trainingRowCount, "integer")
  expect_type(response$modelType, "character")
  expect_type(response$featurelistId, "character")
  expect_type(response$modelCategory, "character")
  expect_type(response$blueprintId, "character")
  expect_type(response$isBlocked, "logical")
})

test_that("GetModelJob throws an error if the DataRobot API request is invalid", {
  completeModelJobResponse <- httr:::response(
    url = modelJobUrl,
    status_code = 400L,
    content = charToRaw(modelJobJson)
  )
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completeModelJobResponse)
  expect_error(with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetModelJob(fakeProject, fakeJobId)
  ))
})
