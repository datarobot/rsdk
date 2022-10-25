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

fakePrimeFileId <- "primefile-id"
primeFiletUrl <- UrlJoin(projectUrl, "primeFile", fakePrimeFileId)
primeFileJson <- fileToChar("responses/primeFile.json")
completedPrimeFileResponse <- httr:::response(
  url = primeFiletUrl,
  status_code = 200L,
  content = charToRaw(primeFileJson)
)

test_that("GetPrimeFile succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedPrimeFileResponse)
  primeFile <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPrimeFile(fakeProject, fakePrimeFileId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(primeFile, "list")
  expect_type(primeFile$language, "character")
  expect_type(primeFile$isValid, "logical")
  expect_type(primeFile$rulesetId, "integer")
  expect_type(primeFile$parentModelId, "character")
  expect_type(primeFile$projectId, "character")
  expect_type(primeFile$id, "character")
  expect_type(primeFile$modelId, "character")
})
