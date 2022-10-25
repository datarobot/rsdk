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
source("helper-ResidualsChart.R")

getResidualsChartAllUrl <- UrlJoin(projectUrl, "models", fakeModelId, "residuals")
getResidualsChartAllJson <- fileToChar("responses/residualsChartAll.json")
residualsChartAllResponse <- httr:::response(
  url = getResidualsChartAllUrl,
  status_code = 200L,
  content = charToRaw(getResidualsChartAllJson)
)

errorMsg <- jsonlite::toJSON(list(message = list("404 Not Found")))
noChartResponse <- httr:::response(
  url = getResidualsChartAllUrl,
  status_code = 404L,
  content = charToRaw(errorMsg)
)

test_that("ListResidualsChart() returns all residuals charts for a model", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(residualsChartAllResponse)
  residualsCharts <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListResidualsCharts(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 1)
  expectResidualsChartHasValidSource(residualsCharts)
  for (i in seq(residualsCharts)) {
    expectResidualsChartDataShape(residualsCharts[[i]])
  }
})

test_that("ListResidualsChart() will fallback to parent model insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noChartResponse)
  getStub$onCall(2)$returns(residualsChartAllResponse)
  residualsCharts <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetFrozenModel" = function(...) fakeModel,
    "datarobot::GetModel" = function(...) fakeModel,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListResidualsCharts(fakeModel, fallbackToParentInsights = TRUE)
  )
  expect_equal(getStub$calledTimes(), 2)
  for (i in seq(residualsCharts)) {
    expectResidualsChartDataShape(residualsCharts[[i]])
  }
})

test_that("ListResidualsChart() will fail if no residuals are available", {
  getStub <- stub(httr::GET)
  getStub$returns(noChartResponse)
  expect_error(
    with_mock(
      "httr::GET" = getStub$f,
      "datarobot::GetFrozenModel" = function(...) fakeModel,
      "datarobot::GetModel" = function(...) fakeModel,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      ListResidualsCharts(fakeModel, fallbackToParentInsights = FALSE)
    ),
    "404"
  )
  expect_equal(getStub$calledTimes(), 1)
})

test_that("ListResidualsChart() will fail if no residuals are available even on the parent model", {
  getStub <- stub(httr::GET)
  getStub$returns(noChartResponse)
  expect_error(
    with_mock(
      "httr::GET" = getStub$f,
      "datarobot::GetFrozenModel" = function(...) fakeModel,
      "datarobot::GetModel" = function(...) fakeModel,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      ListResidualsCharts(fakeModel, fallbackToParentInsights = TRUE)
    ),
    "404"
  )
  expect_equal(getStub$calledTimes(), 2)
})
