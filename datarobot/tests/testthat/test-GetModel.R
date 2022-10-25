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
  expect_error(GetModel())
  expect_error(GetModel(fakeProject))
  expect_error(GetModel(modelId = modelId))
})

test_that("Message if modelId is blank", {
  expect_error(GetModel(fakeProject, ""))
})

modelJson <- fileToChar("responses/getModel.json")
completedModelResponse <- httr:::response(
  url = modelUrl,
  status_code = 200L,
  content = charToRaw(modelJson)
)
mockedModel <- datarobot:::as.dataRobotModel(fromJSON(modelJson))

test_that("GetModel succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedModelResponse)
  model <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::GetProject" = function(project) {
      p <- fromJSON(fileToChar("responses/GetProject.json"))
      datarobot:::as.dataRobotProject(p)
    }, GetModel(fakeProject, fakeModelId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(model, "dataRobotModel")
  ExpectHasKeys(model, c(
    "projectId",
    "projectName",
    "modelId",
    "modelNumber",
    "blueprintId",
    "featurelistId",
    "isStarred",
    "predictionThreshold",
    "predictionThresholdReadOnly"
  ))
  expect_type(model$modelId, "character")
  expect_type(model$modelNumber, "integer")
  expect_type(model$projectId, "character")
  expect_type(model$samplePct, "double")
  expect_type(model$trainingRowCount, "integer")
  expect_type(model$isStarred, "logical")
  expect_type(model$predictionThreshold, "double")
  expect_type(model$predictionThresholdReadOnly, "logical")
  expect_true("LogLoss" %in% names(model$metrics))
  expect_type(model$metrics, "list")
  expect_s3_class(model$metrics$LogLoss, "data.frame")
  expect_true("validation" %in% names(model$metrics$LogLoss))
})

test_that("Zero-length processes element works", {
  modifiedModel <- mockedModel
  modifiedModel$processes <- character(0)
  class(modifiedModel) <- "list" # Functionally equivalent and necessary for toJSON to work
  modifiedModel <- toJSON(modifiedModel)
  modifiedModel <- gsub("{}", "null", modifiedModel, fixed = TRUE) # Correct JSON coercion
  getStub <- stub(httr::GET)
  completedModelResponse <- httr:::response(
    url = modelUrl,
    status_code = 200L,
    content = charToRaw(modifiedModel)
  )
  getStub$onCall(1)$returns(completedModelResponse)
  model <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::GetProject" = function(project) {
      p <- fromJSON(fileToChar("responses/GetProject.json"))
      datarobot:::as.dataRobotProject(p)
    }, GetModel(fakeProject, fakeModelId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(model, "dataRobotModel")
})
