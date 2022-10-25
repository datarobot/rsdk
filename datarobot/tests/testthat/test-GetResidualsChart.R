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

fakeSource <- "validation"
getResidualsChartUrl <- UrlJoin(projectUrl, "models", fakeModelId, "residuals", fakeSource)
getResidualsChartJson <- fileToChar("responses/residualsChart.json")
residualsChartResponse <- httr:::response(
  url = getResidualsChartUrl,
  status_code = 200L,
  content = charToRaw(getResidualsChartJson)
)

errorMsg <- jsonlite::toJSON(list(message = list("404 Not Found")))
noChartResponse <- httr:::response(
  url = getResidualsChartUrl,
  status_code = 404L,
  content = charToRaw(errorMsg)
)

test_that("GetResidualsChart() gets the correct residuals chart and
           returns data in the expected shape", {
  getStub <- stub(httr::GET)
  getStub$returns(residualsChartResponse)
  residualsChart <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetModel" = function(...) fakeModel,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetResidualsChart(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 1)
  expectResidualsChartForCorrectSource(residualsChart, fakeSource)
  expectResidualsChartHasValidSource(residualsChart)
  expectResidualsChartDataShape(residualsChart[[1]])
})

test_that("GetResidualsChart() can fallback to parent model insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noChartResponse)
  getStub$onCall(2)$returns(residualsChartResponse)
  residualsChart <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetFrozenModel" = function(...) fakeModel,
    "datarobot::GetModel" = function(...) fakeModel,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetResidualsChart(fakeModel, fallbackToParentInsights = TRUE)
  )
  expect_equal(getStub$calledTimes(), 2)
  expectResidualsChartForCorrectSource(residualsChart, fakeSource)
  expectResidualsChartHasValidSource(residualsChart)
  expectResidualsChartDataShape(residualsChart[[1]])
})

test_that("GetResidualsChart() will fail if no residuals are available", {
  getStub <- stub(httr::GET)
  getStub$returns(noChartResponse)
  expect_error(
    with_mock(
      "httr::GET" = getStub$f,
      "datarobot::GetFrozenModel" = function(...) fakeModel,
      "datarobot::GetModel" = function(...) fakeModel,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetResidualsChart(fakeModel, fallbackToParentInsights = FALSE)
    ),
    "404"
  )
  expect_equal(getStub$calledTimes(), 1)
})

test_that("GetResidualsChart() will fail if no residuals are available even on the parent model", {
  getStub <- stub(httr::GET)
  getStub$returns(noChartResponse)
  expect_error(
    with_mock(
      "httr::GET" = getStub$f,
      "datarobot::GetFrozenModel" = function(...) fakeModel,
      "datarobot::GetModel" = function(...) fakeModel,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetResidualsChart(fakeModel, fallbackToParentInsights = TRUE)
    ),
    "404"
  )
  expect_equal(getStub$calledTimes(), 2)
})

test_that("GetResidualsChart() will return rowNumber as NA if it is not in the API response", {
  getResidualsChartJson <- fileToChar("responses/residualsChart5.2.json")
  residualsChartResponse <- httr:::response(
    url = getResidualsChartUrl,
    status_code = 200L,
    content = charToRaw(getResidualsChartJson)
  )

  getStub <- stub(httr::GET)
  getStub$returns(residualsChartResponse)
  residualsChart <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetModel" = function(...) fakeModel,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetResidualsChart(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 1)
  expectResidualsChartForCorrectSource(residualsChart, fakeSource)
  expectResidualsChartHasValidSource(residualsChart)
  chart <- residualsChart[[1]]
  expectResidualsChartDataShape(chart)
  expect_true(all(is.na(chart$data$rowNumber)))
})
