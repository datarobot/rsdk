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
liftChartUrl <- UrlJoin(projectUrl, "models", fakeModelId, "liftChart", fakeSource)
liftChartAllUrl <- UrlJoin(projectUrl, "models", fakeModelId, "liftChart")

liftChartJson <- fileToChar("responses/liftChart.json")
liftChartResponse <- httr:::response(
  url = liftChartUrl,
  status_code = 200L,
  content = charToRaw(liftChartJson)
)
errorMsg <- jsonlite::toJSON(list(message = list("404 whatever")))
noChartResponse <- httr:::response(
  url = liftChartUrl,
  status_code = 404L,
  content = charToRaw(errorMsg)
)
liftChartAllJson <- fileToChar("responses/liftChartAll.json")
liftChartAllResponse <- httr:::response(
  url = liftChartAllUrl,
  status_code = 200L,
  content = charToRaw(liftChartAllJson)
)


test_that("GetLiftChart succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(liftChartResponse)
  liftChart <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetLiftChart(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(liftChart, "data.frame")
  expect_type(liftChart$binWeight, "double")
  expect_type(liftChart$actual, "double")
  expect_type(liftChart$predicted, "double")
})

test_that("GetLiftChart succeeds with fallback to parent insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noChartResponse)
  getStub$onCall(2)$returns(liftChartResponse)
  liftChart <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetFrozenModel" = function(...) fakeModel,
    "datarobot::GetModel" = function(...) fakeModel,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetLiftChart(fakeModel, fallbackToParentInsights = TRUE)
  )
  expect_equal(getStub$calledTimes(), 2)
  expect_s3_class(liftChart, "data.frame")
  expect_type(liftChart$binWeight, "double")
  expect_type(liftChart$actual, "double")
  expect_type(liftChart$predicted, "double")
})

test_that("GetLiftChart fails without fallback to parent insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noChartResponse)
  getStub$onCall(2)$returns(liftChartResponse)
  expect_error(with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetFrozenModel" = function(...) fakeModel,
    "datarobot::GetModel" = function(...) fakeModel,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetLiftChart(fakeModel, fallbackToParentInsights = FALSE)
  ), "404")
})


test_that("ListLiftCharts succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(liftChartAllResponse)
  liftChartAll <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListLiftCharts(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(liftChartAll, "list")
  expect_s3_class(liftChartAll$validation, "data.frame")
  expect_type(liftChartAll$validation$binWeight, "double")
  expect_type(liftChartAll$validation$actual, "double")
  expect_type(liftChartAll$validation$predicted, "double")
  expect_s3_class(liftChartAll$holdout, "data.frame")
  expect_type(liftChartAll$holdout$binWeight, "double")
  expect_type(liftChartAll$holdout$actual, "double")
  expect_type(liftChartAll$holdout$predicted, "double")
  expect_s3_class(liftChartAll$crossValidation, "data.frame")
  expect_type(liftChartAll$crossValidation$binWeight, "double")
  expect_type(liftChartAll$crossValidation$actual, "double")
  expect_type(liftChartAll$crossValidation$predicted, "double")
})

test_that("ListLiftCharts succeeds with fallback to parent insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noChartResponse)
  getStub$onCall(2)$returns(liftChartAllResponse)
  liftChartAll <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetFrozenModel" = function(...) fakeModel,
    "datarobot::GetModel" = function(...) fakeModel,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListLiftCharts(fakeModel, fallbackToParentInsights = TRUE)
  )
  expect_equal(getStub$calledTimes(), 2)
  expect_type(liftChartAll, "list")
})

test_that("ListLiftCharts fails without fallback to parent insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noChartResponse)
  getStub$onCall(2)$returns(liftChartAllResponse)
  expect_error(with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetFrozenModel" = function(...) fakeModel,
    "datarobot::GetModel" = function(...) fakeModel,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListLiftCharts(fakeModel, fallbackToParentInsights = FALSE)
  ), "404")
})
