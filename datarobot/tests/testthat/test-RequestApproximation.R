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

primeJobJson <- fileToChar("responses/primeJob.json")
completedPrimeJobResponse <- httr:::response(
  url = jobUrl,
  status_code = 200L,
  content = charToRaw(primeJobJson)
)

completedPrimeJobResponse0 <- httr:::response(
  url = rulesetsUrl,
  status_code = 303L,
  headers = list(location = rulesetsUrl),
  content = raw(0)
)

test_that("RequestApproximation succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedPrimeJobResponse)
  getStub2 <- stub(httr::POST)
  getStub2$onCall(1)$returns(completedPrimeJobResponse0)
  primeJobId <- with_mock(
    "httr::GET" = getStub$f,
    "httr::POST" = getStub2$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RequestApproximation(fakeProject, fakeModelId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(primeJobId, "character")
})
