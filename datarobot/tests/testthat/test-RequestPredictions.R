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


requestPredictionsUrl <- UrlJoin("projects", fakeProjectId, "predictions")
requestPredictionsJobUrl <- UrlJoin("projects", fakeProjectId, "predictions", fakeJobId)
predictionResponse <- httr:::response(
  url = requestPredictionsUrl,
  status_code = 303L,
  headers = list(location = requestPredictionsJobUrl),
  content = raw(0)
)

test_that("RequestPredictions", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(predictionResponse)
  predictJobId <- with_mock(
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
    RequestPredictions(
      fakeProjectId,
      fakeModel$modelId,
      fakeDatasetId
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(predictJobId, "character")
  expect_equal(predictJobId, fakeJobId)
  expect_equal(bodyForInspect$modelId, fakeModelId)
  expect_equal(bodyForInspect$datasetId, fakeDatasetId)
  expect_null(bodyForInspect$predictionIntervalsSize)
})

test_that("RequestPredictions with prediction intervals", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(predictionResponse)
  predictJobId <- with_mock(
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
    RequestPredictions(fakeProjectId,
      fakeModel$modelId,
      fakeDatasetId,
      includePredictionIntervals = TRUE
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(predictJobId, "character")
  expect_equal(predictJobId, fakeJobId)
  expect_equal(bodyForInspect$modelId, fakeModelId)
  expect_equal(bodyForInspect$datasetId, fakeDatasetId)
})

test_that("RequestPredictions with prediction intervals and custom size", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(predictionResponse)
  predictJobId <- with_mock(
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
    RequestPredictions(fakeProjectId,
      fakeModel$modelId,
      fakeDatasetId,
      includePredictionIntervals = TRUE,
      predictionIntervalsSize = 100
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(predictJobId, "character")
  expect_equal(predictJobId, fakeJobId)
  expect_equal(bodyForInspect$modelId, fakeModelId)
  expect_equal(bodyForInspect$datasetId, fakeDatasetId)
  expect_equal(bodyForInspect$predictionIntervalsSize, 100)
})

test_that("RequestPredictions with custom prediction intervals size", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(predictionResponse)
  predictJobId <- with_mock(
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
    RequestPredictions(fakeProjectId,
      fakeModel$modelId,
      fakeDatasetId,
      predictionIntervalsSize = 100
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(predictJobId, "character")
  expect_equal(predictJobId, fakeJobId)
  expect_equal(bodyForInspect$modelId, fakeModelId)
  expect_equal(bodyForInspect$datasetId, fakeDatasetId)
})
