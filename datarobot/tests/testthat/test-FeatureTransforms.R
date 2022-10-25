# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(testthat)
library(stubthat)

derivedFeaturesUrl <- datarobot:::UrlJoin(projectUrl, "typeTransformFeatures")
newFeatureName <- "new-feature"
newFeatureUrl <- datarobot:::UrlJoin(projectUrl, "features", newFeatureName)
statusUrl <- datarobot:::UrlJoin(fakeEndpoint, "status", "some-status")

test_that("derived feature functions invoke the API as expected", {
  # This is solely an interaction test
  # TODO consider rewriting this to an actual unit test
  functionsToTest <- c(
    CreateDerivedFeatureAsNumeric, CreateDerivedFeatureAsCategorical,
    CreateDerivedFeatureAsText, CreateDerivedFeatureIntAsCategorical
  )


  derivedFeatureJson <- '{"featureType": "Categorical", "lowInformation": false, "name": "feature",
                           "uniqueCount": 200, "importance": 1, "id": 34, "naCount": 0,
                           "mean": 1, "median": 1, "stdDev": 1, "min": 1, "max": 10,
                           "parentFeature": 31,
                           "timeSeriesEligible": false,
                           "timeSeriesEligibilityReason": "notADate",
                           "timeStep": null, "timeUnit": null,
                           "targetLeakage": "SKIPPED_DETECTION"}'
  for (functionToTest in functionsToTest) {
    postStub <- stub(httr::POST)
    getStub <- stub(httr::GET)

    postStub$onCall(1)$expects(url = derivedFeaturesUrl)
    postStub$onCall(1)$returns(httr:::response(
      url = derivedFeaturesUrl,
      status_code = 202L,
      headers = list(location = statusUrl),
      content = raw(0)
    ))

    getStub$onCall(1)$expects(url = statusUrl)
    getStub$onCall(1)$returns(httr:::response(
      url = statusUrl,
      status_code = 200L,
      content = charToRaw('{"status": "RUNNING"}')
    ))

    getStub$onCall(2)$expects(url = statusUrl)
    getStub$onCall(2)$returns(httr:::response(
      url = statusUrl,
      status_code = 303L,
      headers = list(location = newFeatureUrl),
      content = raw(0)
    ))

    getStub$onCall(3)$expects(url = newFeatureUrl)
    getStub$onCall(3)$returns(httr:::response(
      url = newFeatureUrl,
      status_code = 200L,
      content = charToRaw(derivedFeatureJson)
    ))

    output <- with_mock(
      "httr::POST" = postStub$f,
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      functionToTest(fakeProject, "old-feature", "new-feature")
    )

    expect_equal(postStub$calledTimes(), 1)
    expect_equal(getStub$calledTimes(), 3)
  }
})
