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
  expect_error(
    GetDatetimeModel(),
    'argument "modelId" is missing, with no default'
  )
  expect_error(
    GetDatetimeModel(fakeProject),
    'argument "modelId" is missing, with no default'
  )
  expect_error(
    GetDatetimeModel(modelId = fakeModelId),
    'argument "project" is missing, with no default'
  )
})

test_that("Message if modelId is blank", {
  expect_error(GetDatetimeModel(fakeProject, ""))
})

test_that("it can get a datetime model", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse) # GetDatetimeModel calls GetProject
  getDatetimeModelUrl <- UrlJoin(projectUrl, "datetimeModels", fakeModelId)
  getDatetimeModelJson <- fileToChar("responses/getDatetimeModelObject.json")
  datetimeModelResponse <- httr:::response(
    url = getDatetimeModelUrl,
    status_code = 200L,
    content = charToRaw(getDatetimeModelJson)
  )
  getStub$onCall(2)$returns(datetimeModelResponse)
  datetimeModel <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDatetimeModel(fakeProject, fakeModelId)
  )
  expect_equal(getStub$calledTimes(), 2)
  expect_s3_class(datetimeModel, "dataRobotDatetimeModel")
})
