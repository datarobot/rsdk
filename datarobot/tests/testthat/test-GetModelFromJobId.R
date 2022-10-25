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
  expect_error(GetModelFromJobId())
  expect_error(GetModelFromJobId(fakeProjectId))
  expect_error(GetModelFromJobId(modelJobId = fakeJobId))
})

modelJson <- fileToChar("responses/getModel.json")
completedModelResponse <- httr:::response(
  url = modelUrl,
  status_code = 200L,
  content = charToRaw(modelJson)
)
waitResponse <- httr:::response(
  url = modelUrl,
  status_code = 303L,
  content = charToRaw(modelJson)
)
test_that("Use projectId only", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedModelResponse)
  model <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::WaitForAsyncReturn" = function(...) {
      ParseReturnResponse(waitResponse)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::GetProject" = function(project) {
      p <- fromJSON(fileToChar("responses/GetProject.json"))
      datarobot:::as.dataRobotProject(p)
    },
    {
      expect_message(
        {
          model <- GetModelFromJobId(fakeProjectId, fakeJobId)
        },
        "retrieved"
      )
      model
    }
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(model, "dataRobotModel")
  expect_type(model$modelId, "character")
  expect_type(model$projectId, "character")
  expect_type(model$samplePct, "double")
  expect_type(model$trainingRowCount, "integer")
})

test_that("Use complete project list", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedModelResponse)
  model <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::WaitForAsyncReturn" = function(...) {
      ParseReturnResponse(waitResponse)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::GetProject" = function(project) {
      p <- fromJSON(fileToChar("responses/GetProject.json"))
      datarobot:::as.dataRobotProject(p)
    },
    {
      expect_message(
        {
          model <- GetModelFromJobId(fakeProject, fakeJobId)
        },
        "retrieved"
      )
      model
    }
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(model, "dataRobotModel")
  expect_type(model$modelId, "character")
  expect_type(model$projectId, "character")
  expect_type(model$samplePct, "double")
  expect_type(model$trainingRowCount, "integer")
})

test_that("GetModelFromJobId raises appropriate exception when job status indicates failure", {
  jobFailureJson <- '{"status": "error", "message": "some job failure message"}'
  testReturn <- with_mock(
    "datarobot::DataRobotGET" = function(url, ...) {
      httr:::response(
        url = url,
        status_code = 200,
        content = charToRaw(jobFailureJson)
      )
    },
    expect_error(GetModelFromJobId(fakeProjectId, fakeJobId), "PendingJobFailed")
  )
})

test_that("maxWait parameter is passed to WaitForAsyncReturn", {
  maxWaitToUse <- 2
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedModelResponse)
  model <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::WaitForAsyncReturn" = function(...) {
      expect_equal(list(...)$maxWait, maxWaitToUse)
      ParseReturnResponse(waitResponse)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::GetProject" = function(project) {
      p <- fromJSON(fileToChar("responses/GetProject.json"))
      datarobot:::as.dataRobotProject(p)
    },
    {
      GetModelFromJobId(fakeProjectId, fakeJobId, maxWait = maxWaitToUse)
    }
  )
  expect_equal(getStub$calledTimes(), 1)
})
