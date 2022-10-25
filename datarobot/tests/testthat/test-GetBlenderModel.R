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

blenderModelUrl <- UrlJoin(projectUrl, "blenderModels", fakeModelId)
blenderModelJson <- fileToChar("responses/blendModel.json")
completedBlenderModelResponse <- httr:::response(
  url = blenderModelUrl,
  status_code = 200L,
  content = charToRaw(blenderModelJson)
)

test_that("Required parameters are present", {
  expect_error(GetBlenderModel())
  expect_error(GetBlenderModel(fakeProject))
  expect_error(GetBlenderModel(fakeProjectId))
  expect_error(GetBlenderModel(modelId = fakeModelId))
})

test_that("GetBlenderModel succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedBlenderModelResponse)
  blendModel <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::GetProject" = function(project) {
      p <- fromJSON(fileToChar("responses/GetProject.json"))
      datarobot:::as.dataRobotProject(p)
    }, GetBlenderModel(fakeProject, fakeModelId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(blendModel, "dataRobotBlenderModel")
  expect_type(blendModel$modelId, "character")
  expect_type(blendModel$modelNumber, "integer")
  expect_type(blendModel$samplePct, "double")
  expect_type(blendModel$trainingRowCount, "integer")
  expect_type(blendModel$modelIds, "character")
  expect_type(blendModel$projectId, "character")
})
