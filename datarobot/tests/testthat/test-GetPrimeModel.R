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

test_that("GetPrimeModel succeeds", {
  primeModeltUrl <- UrlJoin(projectUrl, "primeModels", fakeModelId)
  primeModelJson <- fileToChar("responses/primeModel.json")
  completedPrimeModelResponse <- httr:::response(
    url = primeModeltUrl,
    status_code = 200L,
    content = charToRaw(primeModelJson)
  )

  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedPrimeModelResponse)
  primeModel <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPrimeModel(fakeProjectId, fakeModelId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(primeModel, "dataRobotPrimeModel")
  expect_type(primeModel$id, "character")
  expect_type(primeModel$ruleCount, "integer")
  expect_type(primeModel$parentModelId, "character")
  expect_type(primeModel$projectId, "character")
})
