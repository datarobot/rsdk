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

test_that("Required parameters are present", {
  expect_error(RequestNewModel())
  expect_error(RequestNewModel(fakeProjectId))
  expect_error(RequestNewModel(blueprint = fakeBlueprint))
})


projectUrl <- UrlJoin(fakeEndpoint, "projects", fakeProjectId)
modelUrl <- UrlJoin(projectUrl, "models")
requestResponse <- httr:::response(
  url = modelUrl,
  status_code = 202L,
  headers = list(location = modelUrl),
  content = raw(0)
)

test_that("RequestNewModel succeeds", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    {
      expect_message(
        {
          jobId <- RequestNewModel(fakeProjectId, fakeBlueprint)
        },
        "model request"
      )
      jobId
    }
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(jobId, "character")
})

test_that("RequestNewModel succeeds with samplePct", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::DataRobotPOST" = function(routeString,
                                           addUrl = TRUE,
                                           body = NULL,
                                           returnRawResponse = FALSE, ...) {
      bodyForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::POST, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    {
      expect_message(
        {
          jobId <- RequestNewModel(fakeProjectId,
            fakeBlueprint,
            samplePct = 78
          )
        },
        "model request"
      )
      expect_equal(as.numeric(bodyForInspect$samplePct), 78)
      jobId
    }
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(jobId, "character")
})

test_that("RequestNewModel succeeds with trainingRowCount", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::DataRobotPOST" = function(routeString,
                                           addUrl = TRUE,
                                           body = NULL,
                                           returnRawResponse = FALSE, ...) {
      bodyForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::POST, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    {
      expect_message(
        {
          jobId <- RequestNewModel(fakeProjectId,
            fakeBlueprint,
            trainingRowCount = 200
          )
        },
        "model request"
      )
      expect_equal(as.numeric(bodyForInspect$trainingRowCount), 200)
      jobId
    }
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(jobId, "character")
})

test_that("RequestNewModel succeeds with featurelist", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::DataRobotPOST" = function(routeString,
                                           addUrl = TRUE,
                                           body = NULL,
                                           returnRawResponse = FALSE, ...) {
      bodyForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::POST, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    {
      expect_message(
        {
          jobId <- RequestNewModel(fakeProjectId,
            fakeBlueprint,
            featurelist = fakeFeaturelist
          )
        },
        "model request"
      )
      expect_equal(
        as.character(bodyForInspect$featurelistId),
        fakeFeaturelistId
      )
      jobId
    }
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(jobId, "character")
})

test_that("RequestNewModel succeeds with scoringType", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::DataRobotPOST" = function(routeString,
                                           addUrl = TRUE,
                                           body = NULL,
                                           returnRawResponse = FALSE, ...) {
      bodyForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::POST, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    {
      expect_message(
        {
          jobId <- RequestNewModel(fakeProjectId,
            fakeBlueprint,
            scoringType = "validation"
          )
        },
        "model request"
      )
      expect_equal(as.character(bodyForInspect$scoringType), "validation")
      jobId
    }
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(jobId, "character")
})

test_that("RequestNewModel succeeds with blueprint from another project", {
  otherBlueprint <- fakeBlueprint
  otherBlueprint$projectId <- "other-project"
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::DataRobotPOST" = function(routeString,
                                           addUrl = TRUE,
                                           body = NULL,
                                           returnRawResponse = FALSE, ...) {
      bodyForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::POST, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    {
      expect_message(
        {
          jobId <- RequestNewModel(
            fakeProjectId,
            otherBlueprint
          )
        },
        "model request"
      )
      expect_equal(as.character(bodyForInspect$sourceProjectId), "other-project")
      jobId
    }
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(jobId, "character")
})

test_that("RequestNewModel succeeds with monotonic constraints", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::DataRobotPOST" = function(routeString,
                                           addUrl = TRUE,
                                           body = NULL,
                                           returnRawResponse = FALSE, ...) {
      bodyForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::POST, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    {
      expect_message(
        {
          jobId <- RequestNewModel(fakeProjectId,
            fakeBlueprint,
            monotonicIncreasingFeaturelistId = "mono-up",
            monotonicDecreasingFeaturelistId = "mono-down"
          )
        },
        "model request"
      )
      expect_equal(
        as.character(bodyForInspect$monotonicIncreasingFeaturelistId),
        "mono-up"
      )
      expect_equal(
        as.character(bodyForInspect$monotonicDecreasingFeaturelistId),
        "mono-down"
      )
      jobId
    }
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(jobId, "character")
})

test_that("RequestNewModel succeeds with monotonic constraints - full feature list", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::DataRobotPOST" = function(routeString,
                                           addUrl = TRUE,
                                           body = NULL,
                                           returnRawResponse = FALSE, ...) {
      bodyForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::POST, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    {
      expect_message(
        {
          jobId <- RequestNewModel(fakeProjectId,
            fakeBlueprint,
            monotonicIncreasingFeaturelistId = fakeFeaturelist,
            monotonicDecreasingFeaturelistId = fakeFeaturelist
          )
        },
        "model request"
      )
      expect_equal(
        as.character(bodyForInspect$monotonicIncreasingFeaturelistId),
        fakeFeaturelistId
      )
      expect_equal(
        as.character(bodyForInspect$monotonicDecreasingFeaturelistId),
        fakeFeaturelistId
      )
      jobId
    }
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(jobId, "character")
})

test_that("RequestNewModel succeeds with monotonic constraints - blank override", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  jobId <- with_mock(
    "httr::POST" = function(...) {
      rawBodyForInspect <<- list(...)
      postStub$f(...)
    },
    "datarobot:::DataRobotPOST" = function(routeString,
                                           addUrl = TRUE,
                                           body = NULL,
                                           returnRawResponse = FALSE, ...) {
      bodyForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::POST, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    {
      expect_message(
        {
          jobId <- RequestNewModel(fakeProjectId,
            fakeBlueprint,
            monotonicIncreasingFeaturelistId = "",
            monotonicDecreasingFeaturelistId = ""
          )
        },
        "model request"
      )
      ExpectHasKeys(bodyForInspect, c(
        "monotonicIncreasingFeaturelistId",
        "monotonicDecreasingFeaturelistId",
        "blueprintId"
      ))
      expect_true(is.null(bodyForInspect$monotonicIncreasingFeaturelistId))
      expect_true(is.null(bodyForInspect$monotonicDecreasingFeaturelistId))
      expect_equal(as.character(bodyForInspect$blueprintId), fakeBlueprintId)
      expect_equal(rawBodyForInspect[[1]], modelUrl)
      expect_true(jsonlite::validate(rawBodyForInspect$body),
        info = sprintf("invalid json: %s", rawBodyForInspect$body)
      )
      expect_equal(rawBodyForInspect$encode, "raw")
      jobId
    }
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(jobId, "character")
})
