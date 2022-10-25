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

frozenModeltUrl <- UrlJoin(projectUrl, "frozenModels", fakeModelId)

test_that("GetFrozenModel succeeds", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  frozenModelJson <- fileToChar("responses/frozenModel.json")
  completedFrozenModelResponse <- httr:::response(
    url = frozenModeltUrl,
    status_code = 200L,
    content = charToRaw(frozenModelJson)
  )
  getStub$onCall(2)$returns(completedFrozenModelResponse)
  frozenModel <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetFrozenModel(fakeProject, fakeModelId)
  )
  expect_equal(getStub$calledTimes(), 2)
  expect_s3_class(frozenModel, "dataRobotFrozenModel")
  expect_type(frozenModel$modelId, "character")
  expect_type(frozenModel$modelNumber, "integer")
  expect_equal(frozenModel$isFrozen, TRUE)
  expect_type(frozenModel$parentModelId, "character")
  expect_type(frozenModel$projectId, "character")
})
