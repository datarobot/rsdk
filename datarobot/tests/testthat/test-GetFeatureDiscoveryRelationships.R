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


test_that("it can get feature discovery relationships", {
  getStub <- stub(httr::GET)
  featureDiscoveryRelationshipsUrl <- UrlJoin(projectUrl, "relationshipsConfiguration")
  featureDiscoveryRelationshipsJson <- fileToChar("responses/FeatureDiscoveryRelationships.json")
  featureDiscoveryRelationshipsResponse <- httr:::response(
    url = featureDiscoveryRelationshipsUrl,
    status_code = 200L,
    content = charToRaw(featureDiscoveryRelationshipsJson)
  )
  getStub$onCall(1)$returns(featureDiscoveryRelationshipsResponse)
  featureDiscoveryRelationships <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetFeatureDiscoveryRelationships(fakeProjectId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(featureDiscoveryRelationships$id, "620d320fddfb8cf2a45f91e3")
})
