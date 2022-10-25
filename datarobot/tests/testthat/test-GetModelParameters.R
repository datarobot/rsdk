# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
parametersUrl <- UrlJoin(projectUrl, "models", fakeModelId, "parameters")

parametersJson <- fileToChar("responses/modelParameters.json")
parametersResponse <- httr:::response(
  url = parametersUrl,
  status_code = 200L,
  content = charToRaw(parametersJson)
)
expectedCols <- c(
  "coefficient", "type", "derivedFeature", "originalFeature",
  "transformations", "stageCoefficients"
)

test_that("GetModelParameters succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(parametersResponse)
  testReturn <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetModelParameters(fakeProject, fakeModelId)
  )
  expect_type(testReturn, "list")
  ExpectHasKeys(testReturn, c("parameters", "derivedFeatures"))
  expect_type(testReturn$parameters, "list")
  ExpectHasKeys(testReturn$derivedFeatures[[1]], expectedCols)
  expect_type(testReturn$derivedFeatures, "list")
  ExpectHasKeys(testReturn$parameters[[1]], c("name", "value"))
})

parametersJson <- fileToChar("responses/modelParametersTwoStage.json")
parametersResponse <- httr:::response(
  url = parametersUrl,
  status_code = 200L,
  content = charToRaw(parametersJson)
)

test_that("GetModelParameters succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(parametersResponse)
  testReturn <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetModelParameters(fakeProject, fakeModelId)
  )
  expect_type(testReturn, "list")
  ExpectHasKeys(testReturn, c("parameters", "derivedFeatures"))
  expect_type(testReturn$parameters, "list")
  ExpectHasKeys(testReturn$derivedFeatures[[1]], expectedCols)
  expect_type(testReturn$derivedFeatures, "list")
  ExpectHasKeys(testReturn$parameters[[1]], c("name", "value"))
  ExpectHasKeys(testReturn$derivedFeatures[[1]]$stageCoefficients[[1]], c("coefficient", "stage"))
})
