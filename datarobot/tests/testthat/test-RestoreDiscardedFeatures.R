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


projectId <- "5ece5927962d741e19c2febb"

restoreDiscardedFeaturesUrl <- UrlJoin(
  "projects", projectId,
  "modelingFeatures", "fromDiscardedFeatures"
)
getDiscardedFeaturesUrl <- UrlJoin("projects", projectId, "discardedFeatures")


test_that("can get discarded features information", {
  getStub <- stub(httr::GET)
  discardedFeaturesInformation <- fileToChar("responses/discardedFeaturesInformation.json")
  discardedFeaturesResponse <- httr:::response(
    url = getDiscardedFeaturesUrl,
    status_code = 200L,
    content = charToRaw(discardedFeaturesInformation)
  )
  getStub$onCall(1)$returns(discardedFeaturesResponse)
  discardedFeaturesInfo <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RetrieveDiscardedFeaturesInformation(projectId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expectedCols <- c("totalRestoreLimit", "remainingRestoreLimit", "count", "features")
  expect_s3_class(discardedFeaturesInfo, "dataRobotDiscardedFeaturesInformation")
  ExpectHasKeys(discardedFeaturesInfo, expectedCols)
})


test_that("can restore discarded features", {
  postStub <- stub(httr::POST)
  responseJson <- fileToChar("responses/restoredFeaturesInformation.json")
  createResponse <- httr:::response(
    url = restoreDiscardedFeaturesUrl,
    status_code = 202L,
    content = charToRaw(responseJson)
  )
  postStub$onCall(1)$returns(createResponse)
  restoredInfo <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot::WaitForAsyncReturn" = function(...) {},
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RestoreDiscardedFeatures(projectId,
      featuresToRestore = list("feature1", "bad_feature")
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expectedCols <- c("warnings", "restoredFeatures")
  expect_s3_class(restoredInfo, "dataRobotRestoredFeaturesInformation")
  ExpectHasKeys(restoredInfo, expectedCols, allowAdditional = FALSE)
})

test_that("Restoring discarded features 422", {
  postStub <- stub(httr::POST)
  responseJson <- fileToChar("responses/restoredFeaturesInformation.json")
  createResponse <- httr:::response(
    url = restoreDiscardedFeaturesUrl,
    status_code = 422L,
    content = charToRaw(responseJson)
  )
  postStub$onCall(1)$returns(createResponse)
  restoredInfo <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot::WaitForAsyncReturn" = function(...) {},
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RestoreDiscardedFeatures(projectId,
      featuresToRestore = list("bad_feature")
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expectedCols <- c("warnings", "restoredFeatures")
  expect_s3_class(restoredInfo, "dataRobotRestoredFeaturesInformation")
  ExpectHasKeys(restoredInfo, expectedCols, allowAdditional = FALSE)
})
