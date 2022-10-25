# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(testthat)

primeEligibilitytUrl <- UrlJoin(projectUrl, "models", fakeModelId, "primeInfo")

primeEligibilityJson <- fileToChar("responses/primeEligibility.json")
completedPrimeEligibilityResponse <- httr:::response(
  url = primeEligibilitytUrl,
  status_code = 200L,
  content = charToRaw(primeEligibilityJson)
)

test_that("GetPrimeEligibilityForModel succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedPrimeEligibilityResponse)
  isEligible <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPrimeEligibility(fakeProject, fakeModelId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(isEligible$canMakePrime, "logical")
  expect_type(isEligible$message, "character")
})
