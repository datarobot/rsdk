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

primeModelstUrl <- UrlJoin(projectUrl, "primeModels")

expectPrimeModelClass <- function(response) {
  expect_s3_class(response, "dataRobotPrimeModels")
  expect_true(is.data.frame(response))
}

test_that("ListPrimeModels succeeds when no models are found", {
  noDataJson <- fileToChar("responses/noServerData.json")
  noResponse <- httr:::response(
    url = primeModelstUrl,
    status_code = 200L,
    content = charToRaw(noDataJson)
  )

  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noResponse)
  noModels <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListPrimeModels(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 1)
  expectPrimeModelClass(noModels)
  expect_equal(nrow(noModels), 0)
})

test_that("ListPrimeModels succeeds", {
  primeModelsJson <- fileToChar("responses/primeModels.json")
  primeModelsData <- jsonlite::fromJSON(primeModelsJson)
  completedPrimeModelsResponse <- httr:::response(
    url = primeModelstUrl,
    status_code = 200L,
    content = charToRaw(primeModelsJson)
  )

  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedPrimeModelsResponse)
  primeModels <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListPrimeModels(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 1)
  expectPrimeModelClass(primeModels)
  expect_equal(nrow(primeModels), primeModelsData$count)
  expect_type(primeModels$id, "character")
  expect_type(primeModels$ruleCount, "integer")
  expect_type(primeModels$parentModelId, "character")
  expect_type(primeModels$projectId, "character")
})
