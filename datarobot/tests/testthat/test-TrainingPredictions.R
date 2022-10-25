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

postTrainingPredictionsUrl <- UrlJoin(projectUrl, "trainingPredictions")
getTrainingPredictionsUrl <- UrlJoin(projectUrl, "jobs", fakeJobId)
listTrainingPredictionsUrl <- UrlJoin(projectUrl, "trainingPredictions")

postTrainingPredictionsJson <- fileToChar("responses/requestTrainingPredictions.json")
trainingPredictionsResponse <- httr:::response(
  url = postTrainingPredictionsUrl,
  status_code = 200L,
  content = charToRaw(postTrainingPredictionsJson)
)
getTrainingPredictionsJson <- fileToChar("responses/getTrainingPredictions.json")
trainingGetPredictionsResponse <- httr:::response(
  url = getTrainingPredictionsUrl,
  status_code = 200L,
  content = charToRaw(getTrainingPredictionsJson)
)
waitResponse <- httr:::response(
  url = getTrainingPredictionsUrl,
  status_code = 200L,
  content = charToRaw(getTrainingPredictionsJson)
)

test_that("Fail on invalid dataSubset", {
  expect_error(
    RequestTrainingPredictions(fakeModel, "MAGIC"),
    paste0(
      "Invalid ", sQuote("DataSubset"), ". Must be in ", sQuote("all"), ", ",
      sQuote("validationAndHoldout"), ", ", sQuote("holdout"), ", ",
      sQuote("allBacktests"), " but got ", sQuote("MAGIC"), " instead."
    )
  )
})

test_that("it can request training predictions", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(trainingPredictionsResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "httr::GET" = function() stop("Should not be called!"),
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::JobIdFromResponse" = identity,
    RequestTrainingPredictions(fakeModel, "all")
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_s3_class(jobId, "response")
})

test_that("it can get training predictions by prediction ID", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(trainingGetPredictionsResponse)
  trainingPredictions <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetTrainingPredictions(fakeProject, fakeModelId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(trainingPredictions, "data.frame")
  ExpectHasKeys(trainingPredictions, c(
    "partitionId", "prediction", "rowId", "class_Yes",
    "class_No"
  ))
})

test_that("it can get training predictions by job ID", {
  getTrainingPredictionsJson <- fileToChar("responses/getTrainingPredictions.json")
  trainingPredictions <- with_mock(
    "datarobot::WaitForAsyncReturn" = function(...) {
      ParseReturnResponse(waitResponse)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetTrainingPredictionsFromJobId(fakeProject, fakeJobId)
  )
  expect_s3_class(trainingPredictions, "data.frame")
  ExpectHasKeys(trainingPredictions, c(
    "partitionId", "prediction", "rowId", "class_Yes",
    "class_No"
  ))
})

test_that("it can get training predictions by model ID", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(trainingPredictionsResponse)
  getTrainingPredictionsJson <- fileToChar("responses/getTrainingPredictions.json")
  trainingPredictions <- with_mock(
    "httr::POST" = postStub$f,
    "httr::GET" = function() stop("Should not be called!"),
    "datarobot::WaitForAsyncReturn" = function(...) {
      ParseReturnResponse(waitResponse)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::JobIdFromResponse" = identity,
    GetTrainingPredictionsForModel(fakeModel, "all")
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_s3_class(trainingPredictions, "data.frame")
  ExpectHasKeys(trainingPredictions, c(
    "partitionId", "prediction", "rowId", "class_Yes",
    "class_No"
  ))
})

test_that("it can list training predictions", {
  getStub <- stub(httr::GET)
  listTrainingPredictionsJson <- fileToChar("responses/listTrainingPredictions.json")
  trainingPredictionsResponse <- httr:::response(
    url = listTrainingPredictionsUrl,
    status_code = 200L,
    content = charToRaw(listTrainingPredictionsJson)
  )
  getStub$onCall(1)$returns(trainingPredictionsResponse)
  trainingPredictions <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListTrainingPredictions(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(trainingPredictions, "list")
  ExpectHasKeys(trainingPredictions[[1]], c("id", "modelId", "dataSubset"))
})

test_that("it can download training predictions", {
  getStub <- stub(httr::GET)
  downloadTrainingPredictionsUrl <- UrlJoin(projectUrl, "trainingPredictions", fakePredictionId)
  downloadTrainingPredictionsJson <- fileToChar("responses/downloadTrainingPredictions.json")
  trainingPredictionsResponse <- httr:::response(
    url = downloadTrainingPredictionsUrl,
    status_code = 200L,
    content = charToRaw(downloadTrainingPredictionsJson)
  )
  getStub$onCall(1)$returns(trainingPredictionsResponse)
  expect_false(file.exists(fakeFilePath))
  with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DownloadTrainingPredictions(fakeProject, fakeModelId, fakeFilePath)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_true(file.exists(fakeFilePath))
  csv <- read.csv(fakeFilePath)
  expect_equal(nrow(csv), 698)
  expect_equal(ncol(csv), 5)
})


test_that("it can get training predictions by job ID -- time series", {
  getTrainingPredictionsJson <- fileToChar("responses/getTrainingPredictionsTimeSeries.json")
  waitResponse <- httr:::response(
    url = getTrainingPredictionsUrl,
    status_code = 200L,
    content = charToRaw(getTrainingPredictionsJson)
  )
  trainingPredictions <- with_mock(
    "datarobot::WaitForAsyncReturn" = function(...) {
      ParseReturnResponse(waitResponse)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetTrainingPredictionsFromJobId(fakeProject, fakeJobId)
  )
  expect_s3_class(trainingPredictions, "data.frame")
  ExpectHasKeys(trainingPredictions, c(
    "partitionId", "prediction", "rowId", "timestamp",
    "forecastDistance", "forecastPoint"
  ))
})


test_that("it can get training predictions by job ID -- multiseries", {
  getTrainingPredictionsJson <- fileToChar("responses/getTrainingPredictionsMultiseries.json")
  waitResponse <- httr:::response(
    url = getTrainingPredictionsUrl,
    status_code = 200L,
    content = charToRaw(getTrainingPredictionsJson)
  )
  trainingPredictions <- with_mock(
    "datarobot::WaitForAsyncReturn" = function(...) {
      ParseReturnResponse(waitResponse)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetTrainingPredictionsFromJobId(fakeProject, fakeJobId)
  )
  expect_s3_class(trainingPredictions, "data.frame")
  ExpectHasKeys(trainingPredictions, c(
    "partitionId", "prediction", "rowId", "timestamp",
    "forecastDistance", "forecastPoint", "seriesId"
  ))
})

unlink(fakeFilePath)
