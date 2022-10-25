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

primeJobJson <- fileToChar("responses/primeDownloadValidation.json")
completedPrimeJobResponse <- httr:::response(
  url = jobUrl,
  status_code = 200L,
  content = charToRaw(primeJobJson)
)
completedPrimeJobResponse0 <- httr:::response(
  url = rulesetsUrl,
  status_code = 202L,
  headers = list(location = rulesetsUrl),
  content = raw(0)
)

test_that("CreatePrimeCode succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedPrimeJobResponse)
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(completedPrimeJobResponse0)
  primeJobId <- with_mock(
    "httr::GET" = getStub$f,
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    CreatePrimeCode(fakeProject, fakeModelId, PrimeLanguage$Java)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(primeJobId, "character")
})
