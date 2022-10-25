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
  expect_error(
    RequestNewDatetimeModel(),
    'argument "project" is missing, with no default'
  )
  expect_error(
    RequestNewDatetimeModel(fakeProjectId),
    'argument "blueprint" is missing, with no default'
  )
  expect_error(
    RequestNewDatetimeModel(blueprint = blueprint),
    'argument "project" is missing, with no default'
  )
})

test_that("RequestNewDatetimeModel works", {
  postStub <- stub(httr::POST)
  postDatetimeModelUrl <- UrlJoin(projectUrl, "datetimeModels")
  datetimeModelResponse <- httr:::response(
    url = postDatetimeModelUrl,
    status_code = 200L,
    content = NULL
  )
  postStub$onCall(1)$returns(datetimeModelResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "httr::GET" = function() stop("Should not be called!"),
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::JobIdFromResponse" = identity,
    RequestNewDatetimeModel(fakeProject, fakeBlueprint)
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_s3_class(jobId, "response")
})

test_that("RequestNewDatetimeModel works with trainingRowCount", {
  postStub <- stub(httr::POST)
  postDatetimeModelUrl <- UrlJoin(projectUrl, "datetimeModels")
  datetimeModelResponse <- httr:::response(
    url = postDatetimeModelUrl,
    status_code = 202L,
    headers = list(location = jobUrl),
    content = raw(0)
  )
  postStub$onCall(1)$returns(datetimeModelResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "httr::GET" = function() stop("Should not be called!"),
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::JobIdFromResponse" = identity,
    RequestNewDatetimeModel(fakeProject,
      fakeBlueprint,
      trainingRowCount = fakeDatetimeModelTrainingRowCount
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_s3_class(jobId, "response")
})

test_that("RequestNewDatetimeModel works with featurelist", {
  postStub <- stub(httr::POST)
  postDatetimeModelUrl <- UrlJoin(projectUrl, "datetimeModels")
  datetimeModelResponse <- httr:::response(
    url = postDatetimeModelUrl,
    status_code = 202L,
    headers = list(location = jobUrl),
    content = raw(0)
  )
  postStub$onCall(1)$returns(datetimeModelResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "httr::GET" = function() stop("Should not be called!"),
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::JobIdFromResponse" = identity,
    RequestNewDatetimeModel(fakeProject,
      fakeBlueprint,
      featurelist = fakeFeaturelist
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_s3_class(jobId, "response")
})

test_that("RequestNewDatetimeModel works with trainingDuration", {
  postStub <- stub(httr::POST)
  postDatetimeModelUrl <- UrlJoin(projectUrl, "datetimeModels")
  datetimeModelResponse <- httr:::response(
    url = postDatetimeModelUrl,
    status_code = 202L,
    headers = list(location = jobUrl),
    content = raw(0)
  )
  postStub$onCall(1)$returns(datetimeModelResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "httr::GET" = function() stop("Should not be called!"),
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::JobIdFromResponse" = identity,
    RequestNewDatetimeModel(fakeProject,
      fakeBlueprint,
      trainingDuration = ConstructDurationString(days = 20)
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_s3_class(jobId, "response")
})

test_that("RequestNewDatetimeModel works with trainingDuration and timeWindowSamplePct", {
  postStub <- stub(httr::POST)
  postDatetimeModelUrl <- UrlJoin(projectUrl, "datetimeModels")
  datetimeModelResponse <- httr:::response(
    url = postDatetimeModelUrl,
    status_code = 202L,
    headers = list(location = jobUrl),
    content = raw(0)
  )
  postStub$onCall(1)$returns(datetimeModelResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "httr::GET" = function() stop("Should not be called!"),
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::JobIdFromResponse" = identity,
    RequestNewDatetimeModel(fakeProject,
      fakeBlueprint,
      trainingDuration = ConstructDurationString(days = 20),
      timeWindowSamplePct = 90
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_s3_class(jobId, "response")
})

test_that("RequestNewDatetimeModel succeeds with monotonic constraints", {
  postStub <- stub(httr::POST)
  postDatetimeModelUrl <- UrlJoin(projectUrl, "datetimeModels")
  datetimeModelResponse <- httr:::response(
    url = postDatetimeModelUrl,
    status_code = 202L,
    headers = list(location = jobUrl),
    content = raw(0)
  )
  postStub$onCall(1)$returns(datetimeModelResponse)
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
          jobId <- RequestNewDatetimeModel(
            fakeProjectId,
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

test_that("RequestNewDatetimeModel succeeds with monotonic constraints - full feature list", {
  postStub <- stub(httr::POST)
  postDatetimeModelUrl <- UrlJoin(projectUrl, "datetimeModels")
  datetimeModelResponse <- httr:::response(
    url = postDatetimeModelUrl,
    status_code = 202L,
    headers = list(location = jobUrl),
    content = raw(0)
  )
  postStub$onCall(1)$returns(datetimeModelResponse)
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
          jobId <- RequestNewDatetimeModel(
            fakeProjectId,
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

test_that("RequestNewDatetimeModel succeeds with monotonic constraints - blank override", {
  postStub <- stub(httr::POST)
  postDatetimeModelUrl <- UrlJoin(projectUrl, "datetimeModels")
  datetimeModelResponse <- httr:::response(
    url = postDatetimeModelUrl,
    status_code = 202L,
    headers = list(location = jobUrl),
    content = raw(0)
  )
  postStub$onCall(1)$returns(datetimeModelResponse)
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
          jobId <- RequestNewDatetimeModel(fakeProjectId,
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
      expect_equal(rawBodyForInspect[[1]], postDatetimeModelUrl)
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
