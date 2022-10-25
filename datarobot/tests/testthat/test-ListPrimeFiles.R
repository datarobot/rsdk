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

primeFilestUrl <- UrlJoin(projectUrl, "primeFiles")
primeFilesJson <- fileToChar("responses/primeFiles.json")
completedPrimeFilesResponse <- httr:::response(
  url = primeFilestUrl,
  status_code = 200L,
  content = charToRaw(primeFilesJson)
)

test_that("ListprimeFiles succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedPrimeFilesResponse)
  primeList <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListPrimeFiles(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(primeList, "list")
  expect_type(primeList[[1]]$language, "character")
  expect_type(primeList[[1]]$isValid, "logical")
  expect_type(primeList[[1]]$rulesetId, "integer")
  expect_type(primeList[[1]]$parentModelId, "character")
  expect_type(primeList[[1]]$projectId, "character")
  expect_type(primeList[[1]]$id, "character")
  expect_type(primeList[[1]]$modelId, "character")
})
