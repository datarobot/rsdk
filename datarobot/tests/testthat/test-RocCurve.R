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

fakeSource <- "validation"
rocCurveUrl <- UrlJoin(projectUrl, "models", fakeModelId, "rocCurve", fakeSource)
rocCurveAllUrl <- UrlJoin(projectUrl, "models", fakeModelId, "rocCurve")

rocCurveJson <- fileToChar("responses/rocCurve.json")
rocCurveResponse <- httr:::response(
  url = rocCurveUrl,
  status_code = 200L,
  content = charToRaw(rocCurveJson)
)
errorMsg <- jsonlite::toJSON(list(message = list("404 whatever")))
noCurveResponse <- httr:::response(
  url = rocCurveUrl,
  status_code = 404L,
  content = charToRaw(errorMsg)
)
rocCurveAllJson <- fileToChar("responses/rocCurveAll.json")
rocCurveAllResponse <- httr:::response(
  url = rocCurveAllUrl,
  status_code = 200L,
  content = charToRaw(rocCurveAllJson)
)

rocElements <- c(
  "source",
  "negativeClassPredictions",
  "rocPoints",
  "positiveClassPredictions"
)
rocPointsElements <- c(
  "accuracy", "f1Score", "falseNegativeScore",
  "trueNegativeScore", "truePositiveScore",
  "falsePositiveScore", "trueNegativeRate",
  "falsePositiveRate", "truePositiveRate",
  "matthewsCorrelationCoefficient", "positivePredictiveValue",
  "negativePredictiveValue", "threshold",
  "fractionPredictedAsPositive", "fractionPredictedAsNegative",
  "liftPositive", "liftNegative"
)


test_that("GetRocCurve succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(rocCurveResponse)
  rocCurve <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetRocCurve(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(rocCurve, "list")
  ExpectHasKeys(rocCurve, rocElements)
  expect_type(rocCurve$source, "character")
  expect_type(rocCurve$negativeClassPredictions, "double")
  expect_s3_class(rocCurve$rocPoints, "data.frame")
  ExpectHasKeys(rocCurve$rocPoints, rocPointsElements)
  expect_type(rocCurve$positiveClassPredictions, "double")
})

test_that("GetRocCurve succeeds with fallback to parent insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noCurveResponse)
  getStub$onCall(2)$returns(rocCurveResponse)
  rocCurve <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetFrozenModel" = function(...) fakeModel,
    "datarobot::GetModel" = function(...) fakeModel,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetRocCurve(fakeModel, fallbackToParentInsights = TRUE)
  )
  expect_equal(getStub$calledTimes(), 2)
  expect_type(rocCurve, "list")
  expect_type(rocCurve$source, "character")
  expect_type(rocCurve$negativeClassPredictions, "double")
  expect_s3_class(rocCurve$rocPoints, "data.frame")
  expect_type(rocCurve$positiveClassPredictions, "double")
})

test_that("GetRocCurve fails without fallback to parent insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noCurveResponse)
  expect_error(with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetFrozenModel" = function(...) fakeModel,
    "datarobot::GetModel" = function(...) fakeModel,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetRocCurve(fakeModel, fallbackToParentInsights = FALSE)
  ), "404")
})


test_that("ListRocCurves succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(rocCurveAllResponse)
  rocCurveAll <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListRocCurves(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(rocCurveAll, "list")
  expect_type(rocCurveAll$validation, "list")
  expect_type(rocCurveAll$validation$source, "character")
  expect_type(rocCurveAll$validation$negativeClassPredictions, "double")
  expect_s3_class(rocCurveAll$validation$rocPoints, "data.frame")
  expect_type(rocCurveAll$validation$positiveClassPredictions, "double")
  expect_type(rocCurveAll$crossValidation, "list")
  expect_type(rocCurveAll$crossValidation$source, "character")
  expect_type(rocCurveAll$crossValidation$negativeClassPredictions, "double")
  expect_s3_class(rocCurveAll$crossValidation$rocPoints, "data.frame")
  expect_type(rocCurveAll$crossValidation$positiveClassPredictions, "double")
  expect_type(rocCurveAll$holdout, "list")
  expect_type(rocCurveAll$holdout$source, "character")
  expect_type(rocCurveAll$holdout$negativeClassPredictions, "double")
  expect_s3_class(rocCurveAll$holdout$rocPoints, "data.frame")
  expect_type(rocCurveAll$holdout$positiveClassPredictions, "double")
})

test_that("ListLiftCurves succeeds with fallback to parent insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noCurveResponse)
  getStub$onCall(2)$returns(rocCurveAllResponse)
  rocCurveAll <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetFrozenModel" = function(...) fakeModel,
    "datarobot::GetModel" = function(...) fakeModel,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListRocCurves(fakeModel, fallbackToParentInsights = TRUE)
  )
  expect_equal(getStub$calledTimes(), 2)
  expect_type(rocCurveAll, "list")
})

test_that("ListRocCurves fails without fallback to parent insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noCurveResponse)
  getStub$onCall(2)$returns(rocCurveAllResponse)
  expect_error(with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetFrozenModel" = function(...) fakeModel,
    "datarobot::GetModel" = function(...) fakeModel,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListRocCurves(fakeModel, fallbackToParentInsights = FALSE)
  ), "404")
})
