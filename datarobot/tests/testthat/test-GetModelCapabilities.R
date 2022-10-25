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

capabilitiesUrl <- UrlJoin(projectUrl, "models", fakeModelId, "supportedCapabilities")
capabilities <- fileToChar("responses/modelCapabilities.json")
capabilitiesJson <- jsonlite::fromJSON(capabilities)
capabilitiesResponse <- httr:::response(
  url = capabilitiesUrl,
  status_code = 200L,
  content = charToRaw(capabilities)
)

test_that("GetModelCapabilities succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(capabilitiesResponse)
  response <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetModelCapabilities(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(response, capabilitiesJson)
})
