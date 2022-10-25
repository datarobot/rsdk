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

datetimeTrendPlotsComputeUrl <- UrlJoin(
  projectUrl, "datetimeModels", fakeModelId, "datetimeTrendPlots"
)

accuracyOverTimeMetadataGetUrl <- UrlJoin(
  projectUrl, "datetimeModels", fakeModelId, "accuracyOverTimePlots", "metadata"
)

accuracyOverTimePlotGetUrl <- UrlJoin(
  projectUrl, "datetimeModels", fakeModelId, "accuracyOverTimePlots"
)

accuracyOverTimePlotPreviewGetUrl <- UrlJoin(
  projectUrl, "datetimeModels", fakeModelId, "accuracyOverTimePlots", "preview"
)

forecastVsActualMetadataGetUrl <- UrlJoin(
  projectUrl, "datetimeModels", fakeModelId, "forecastVsActualPlots", "metadata"
)

forecastVsActualPlotGetUrl <- UrlJoin(
  projectUrl, "datetimeModels", fakeModelId, "forecastVsActualPlots"
)

forecastVsActualPlotPreviewGetUrl <- UrlJoin(
  projectUrl, "datetimeModels", fakeModelId, "forecastVsActualPlots", "preview"
)

anomalyOverTimeMetadataGetUrl <- UrlJoin(
  projectUrl, "datetimeModels", fakeModelId, "anomalyOverTimePlots", "metadata"
)

anomalyOverTimePlotGetUrl <- UrlJoin(
  projectUrl, "datetimeModels", fakeModelId, "anomalyOverTimePlots"
)

anomalyOverTimePlotPreviewGetUrl <- UrlJoin(
  projectUrl, "datetimeModels", fakeModelId, "anomalyOverTimePlots", "preview"
)

test_that("it can request calculation of datetime trend plots", {
  postStub <- stub(httr::POST)
  datetimeTrendPlotsResponse <- httr:::response(
    url = datetimeTrendPlotsComputeUrl,
    status_code = 202L,
    content = raw(0)
  )
  postStub$onCall(1)$returns(datetimeTrendPlotsResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "httr::GET" = function() stop("Should not be called!"),
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::JobIdFromResponse" = identity,
    ComputeDatetimeTrendPlots(fakeModel)
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_s3_class(jobId, "response")
})

test_that("it can get accuracy over time plots metadata", {
  getStub <- stub(httr::GET)
  metadataJson <- fileToChar("responses/accuracyOverTimeMetadata.json")
  metadataResponse <- httr:::response(
    url = accuracyOverTimeMetadataGetUrl,
    status_code = 200L,
    content = charToRaw(metadataJson)
  )
  getStub$onCall(1)$returns(metadataResponse)
  metadata <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetAccuracyOverTimePlotsMetadata(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 1)

  expectedCols <- c(
    "forecastDistance", "resolutions", "holdoutStatuses",
    "holdoutMetadata", "backtestMetadata", "backtestStatuses"
  )
  ExpectHasKeys(metadata, expectedCols)
  expect_equal(metadata$forecastDistance, 3)
  expect_equal(metadata$resolutions, c("days", "weeks", "months", "quarters", "years"))
  expect_equal(
    metadata$backtestMetadata$training$startDate[1],
    datarobot.apicore::ParseRFC3339Timestamp("2012-11-02T00:00:00.000000Z")
  )
  expect_equal(
    metadata$backtestMetadata$training$endDate[1],
    datarobot.apicore::ParseRFC3339Timestamp("2014-01-22T00:00:00.000000Z")
  )
  expect_equal(
    metadata$backtestMetadata$validation$startDate[2],
    datarobot.apicore::ParseRFC3339Timestamp("2013-11-29T00:00:00.000000Z")
  )
  expect_equal(
    metadata$backtestMetadata$validation$endDate[2],
    datarobot.apicore::ParseRFC3339Timestamp("2014-02-03T00:00:00.000000Z")
  )
  expect_true(is.na(metadata$backtestMetadata$training$startDate[2]))
  expect_true(is.na(metadata$backtestMetadata$training$endDate[2]))
  expect_equal(
    metadata$backtestStatuses$training[1], DatetimeTrendPlotsStatuses$Completed
  )
  expect_equal(
    metadata$backtestStatuses$validation[2], DatetimeTrendPlotsStatuses$Completed
  )
  expect_equal(
    metadata$backtestStatuses$training[2], DatetimeTrendPlotsStatuses$NotCompleted
  )
  expect_equal(
    metadata$holdoutStatuses$training, DatetimeTrendPlotsStatuses$NotCompleted
  )
  expect_equal(
    metadata$holdoutStatuses$validation, DatetimeTrendPlotsStatuses$Completed
  )
  expect_equal(
    metadata$holdoutMetadata$validation$startDate,
    datarobot.apicore::ParseRFC3339Timestamp("2014-04-10T00:00:00.000000Z")
  )
  expect_equal(
    metadata$holdoutMetadata$validation$endDate,
    datarobot.apicore::ParseRFC3339Timestamp("2014-06-15T00:00:00.000000Z")
  )
  expect_true(is.na(metadata$holdoutMetadata$training$startDate))
  expect_true(is.na(metadata$holdoutMetadata$training$endDate))
})

test_that("it can get accuracy over time plot", {
  getStub <- stub(httr::GET)
  plotJson <- fileToChar("responses/accuracyOverTimePlot.json")
  plotResponse <- httr:::response(
    url = accuracyOverTimePlotGetUrl,
    status_code = 200L,
    content = charToRaw(plotJson)
  )
  getStub$onCall(1)$returns(plotResponse)
  plot <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetAccuracyOverTimePlot(fakeModel, maxWait = 0)
  )
  expect_equal(getStub$calledTimes(), 1)

  expectedCols <- c(
    "startDate", "endDate", "bins", "resolution",
    "statistics", "calendarEvents"
  )
  ExpectHasKeys(plot, expectedCols)
  expect_equal(
    plot$startDate, datarobot.apicore::ParseRFC3339Timestamp("2014-02-01T00:00:00.000000Z")
  )
  expect_equal(
    plot$endDate, datarobot.apicore::ParseRFC3339Timestamp("2014-02-05T00:00:00.000000Z")
  )
  expect_equal(plot$resolution, DatetimeTrendPlotsResolutions$Days)
  expect_equal(plot$calendarEvents$name[1], "Washington's Birthday")
  expect_equal(plot$calendarEvents$date[1], datarobot.apicore::ParseRFC3339Timestamp("2014-02-17T00:00:00.000000Z"))
  expect_true(is.na(plot$calendarEvents$seriesId[1]))
  expect_equal(plot$calendarEvents$seriesId[2], "Baltimore")
  expect_equal(nrow(plot$bins), 4)
  expect_equal(
    plot$bins$startDate[1], datarobot.apicore::ParseRFC3339Timestamp("2014-02-01T00:00:00.000000Z")
  )
  expect_equal(
    plot$bins$endDate[1], datarobot.apicore::ParseRFC3339Timestamp("2014-02-02T00:00:00.000000Z")
  )
  expect_equal(plot$bins$actual[1], 56363.2)
  expect_equal(plot$bins$predicted[1], 73027.96022570778)
  expect_equal(plot$bins$frequency[1], 10)
  expect_equal(plot$statistics$durbinWatson, 0.2479576303709634)
})

test_that("it can get accuracy over time and forecast vs actual preview plot", {
  checkPreviewPlot <- function(plot) {
    expectedCols <- c("startDate", "endDate", "bins")
    ExpectHasKeys(plot, expectedCols)
    expect_equal(
      plot$startDate, datarobot.apicore::ParseRFC3339Timestamp("2014-02-01T00:00:00.000000Z")
    )
    expect_equal(
      plot$endDate, datarobot.apicore::ParseRFC3339Timestamp("2014-02-04T00:00:00.000000Z")
    )
    expect_equal(nrow(plot$bins), 3)
    expect_equal(
      plot$bins$startDate[1], datarobot.apicore::ParseRFC3339Timestamp("2014-02-01T00:00:00.000000Z")
    )
    expect_equal(
      plot$bins$endDate[1], datarobot.apicore::ParseRFC3339Timestamp("2014-02-02T00:00:00.000000Z")
    )
    expect_equal(plot$bins$actual[1], 56363.2)
    expect_equal(plot$bins$predicted[1], 73027.96022570778)
  }
  getStub <- stub(httr::GET)
  plotJson <- fileToChar("responses/accuracyOverTimeAndForecastVsActualPreviewPlot.json")
  plotResponse <- httr:::response(
    url = accuracyOverTimePlotPreviewGetUrl,
    status_code = 200L,
    content = charToRaw(plotJson)
  )
  getStub$onCall(1)$returns(plotResponse)
  plot <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetAccuracyOverTimePlotPreview(fakeModel, maxWait = 0)
  )
  expect_equal(getStub$calledTimes(), 1)
  checkPreviewPlot(plot)
  getStub <- stub(httr::GET)
  plotResponse <- httr:::response(
    url = forecastVsActualPlotPreviewGetUrl,
    status_code = 200L,
    content = charToRaw(plotJson)
  )
  getStub$onCall(1)$returns(plotResponse)
  plot <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetForecastVsActualPlotPreview(fakeModel, maxWait = 0)
  )
  expect_equal(getStub$calledTimes(), 1)
  checkPreviewPlot(plot)
})

test_that("it can calculate accuracy over time plot automatically", {
  metadataJson <- fileToChar("responses/accuracyOverTimeMetadata.json")
  metadataResponse <- httr:::response(
    url = accuracyOverTimeMetadataGetUrl,
    status_code = 200L,
    content = charToRaw(metadataJson)
  )
  datetimeTrendPlotsResponse <- httr:::response(
    url = datetimeTrendPlotsComputeUrl,
    status_code = 202L,
    content = raw(0)
  )
  resetMockAndCallFunction <- function(backtest, source) {
    postStub <- stub(httr::POST)
    getStub <- stub(httr::GET)
    postStub$onCall(1)$returns(datetimeTrendPlotsResponse)
    getStub$onCall(1)$returns(metadataResponse)
    with_mock(
      "httr::POST" = postStub$f,
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      "datarobot:::JobIdFromResponse" = identity,
      "datarobot:::WaitForJobToComplete" = function(...) NULL,
      computeAccuracyOverTimePlotIfNotComputed(fakeModel, backtest, source, NULL, NULL)
    )
    expect_equal(getStub$calledTimes(), 1)
    return(postStub$calledTimes())
  }
  expect_equal(resetMockAndCallFunction(1, "training"), 1)
  expect_equal(resetMockAndCallFunction(DataSubset$Holdout, "training"), 1)
  expect_equal(resetMockAndCallFunction(0, "validation"), 0)
  expect_equal(resetMockAndCallFunction(DataSubset$Holdout, "validation"), 0)
  expect_equal(resetMockAndCallFunction(10, "validation"), 0)
  expect_equal(resetMockAndCallFunction(0, "somethingElse"), 0)
})

test_that("it can get forecast vs actual plots metadata", {
  getStub <- stub(httr::GET)
  metadataJson <- fileToChar("responses/forecastVsActualMetadata.json")
  metadataResponse <- httr:::response(
    url = forecastVsActualMetadataGetUrl,
    status_code = 200L,
    content = charToRaw(metadataJson)
  )
  getStub$onCall(1)$returns(metadataResponse)
  metadata <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetForecastVsActualPlotsMetadata(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 1)

  expectedCols <- c(
    "resolutions", "holdoutStatuses",
    "holdoutMetadata", "backtestMetadata", "backtestStatuses"
  )
  ExpectHasKeys(metadata, expectedCols)
  expect_equal(metadata$resolutions, c("days", "weeks", "months", "quarters", "years"))
  expect_equal(
    metadata$backtestMetadata$training$startDate[1],
    datarobot.apicore::ParseRFC3339Timestamp("2012-10-25T00:00:00.000000Z")
  )
  expect_equal(
    metadata$backtestMetadata$training$endDate[1],
    datarobot.apicore::ParseRFC3339Timestamp("2014-01-27T00:00:00.000000Z")
  )
  expect_true(is.na(metadata$backtestMetadata$validation$startDate[2]))
  expect_true(is.na(metadata$backtestMetadata$validation$endDate[2]))
  expect_true(is.na(metadata$backtestMetadata$training$startDate[2]))
  expect_true(is.na(metadata$backtestMetadata$training$endDate[2]))
  expect_equal(unlist(metadata$backtestStatuses[1, "training"][["completed"]]), 1:7)
  expect_null(unlist(metadata$backtestStatuses[1, "training"][["notCompleted"]]))
  expect_equal(unlist(metadata$backtestStatuses[1, "validation"][["completed"]]), 1:7)
  expect_equal(unlist(metadata$backtestStatuses[2, "training"][["notCompleted"]]), 1:4)
  expect_equal(unlist(metadata$backtestStatuses[2, "training"][["inProgress"]]), 5:7)
  expect_equal(unlist(metadata$holdoutStatuses$training$errored), 2:3)
  expect_equal(unlist(metadata$holdoutStatuses$training$completed), 4:6)
  expect_equal(unlist(metadata$holdoutStatuses$validation$completed), c(6))
  expect_equal(
    metadata$holdoutMetadata$validation$startDate,
    datarobot.apicore::ParseRFC3339Timestamp("2014-04-09T00:00:00.000000Z")
  )
  expect_equal(
    metadata$holdoutMetadata$validation$endDate,
    datarobot.apicore::ParseRFC3339Timestamp("2014-06-15T00:00:00.000000Z")
  )
})

test_that("it can get forecast vs actual plot", {
  getStub <- stub(httr::GET)
  plotJson <- fileToChar("responses/forecastVsActualPlot.json")
  plotResponse <- httr:::response(
    url = forecastVsActualPlotGetUrl,
    status_code = 200L,
    content = charToRaw(plotJson)
  )
  getStub$onCall(1)$returns(plotResponse)
  plot <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetForecastVsActualPlot(fakeModel, maxWait = 0)
  )
  expect_equal(getStub$calledTimes(), 1)

  expectedCols <- c(
    "startDate", "endDate", "bins", "resolution",
    "forecastDistances", "calendarEvents"
  )
  ExpectHasKeys(plot, expectedCols)
  expect_equal(
    plot$startDate, datarobot.apicore::ParseRFC3339Timestamp("2014-02-01T00:00:00.000000Z")
  )
  expect_equal(
    plot$endDate, datarobot.apicore::ParseRFC3339Timestamp("2014-04-09T00:00:00.000000Z")
  )
  expect_equal(plot$resolution, DatetimeTrendPlotsResolutions$Days)
  expect_equal(plot$calendarEvents$name[1], "Washington's Birthday")
  expect_equal(plot$calendarEvents$date[1], datarobot.apicore::ParseRFC3339Timestamp("2014-02-17T00:00:00.000000Z"))
  expect_equal(plot$forecastDistances, 1:7)
  expect_equal(nrow(plot$bins), 4)
  expect_equal(
    plot$bins$startDate[1], datarobot.apicore::ParseRFC3339Timestamp("2014-02-01T00:00:00.000000Z")
  )
  expect_equal(
    plot$bins$endDate[1], datarobot.apicore::ParseRFC3339Timestamp("2014-02-02T00:00:00.000000Z")
  )
  expect_equal(plot$bins$actual[1], 56363.2)
  expect_equal(
    unlist(plot$bins$forecasts[1]),
    c(59347.18, 47507.73, 50233.24, 45612.95, 51144.96, 56315.85, 63315.86)
  )
  expect_equal(plot$bins$frequency[1], 10)
  expect_equal(plot$bins$error[1], 6938.456283187342)
  expect_equal(plot$bins$normalizedError[1], 0.5147163592494918)
})

test_that("it can calculate forecast vs actual plots automatically", {
  metadataJson <- fileToChar("responses/forecastVsActualMetadata.json")
  metadataResponse <- httr:::response(
    url = forecastVsActualMetadataGetUrl,
    status_code = 200L,
    content = charToRaw(metadataJson)
  )
  datetimeTrendPlotsResponse <- httr:::response(
    url = datetimeTrendPlotsComputeUrl,
    status_code = 202L,
    content = raw(0)
  )
  resetMockAndCallFunction <- function(backtest, source, forecastDistanceStart, forecastDistanceEnd) {
    postStub <- stub(httr::POST)
    getStub <- stub(httr::GET)
    postStub$onCall(1)$returns(datetimeTrendPlotsResponse)
    getStub$onCall(1)$returns(metadataResponse)
    with_mock(
      "httr::POST" = postStub$f,
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      "datarobot:::JobIdFromResponse" = identity,
      "datarobot:::WaitForJobToComplete" = function(...) NULL,
      computeForecastVsActualPlotIfNotComputed(
        fakeModel, backtest, source, forecastDistanceStart, forecastDistanceEnd, NULL
      )
    )
    expect_equal(getStub$calledTimes(), 1)
    return(postStub$calledTimes())
  }
  expect_equal(resetMockAndCallFunction(1, "training", NULL, NULL), 1)
  expect_equal(resetMockAndCallFunction(1, "training", 5, 6), 0)
  expect_equal(resetMockAndCallFunction(1, "training", 3, NULL), 1)
  expect_equal(resetMockAndCallFunction(DataSubset$Holdout, "training", 1, 1), 1)
  expect_equal(resetMockAndCallFunction(DataSubset$Holdout, "training", NULL, NULL), 1)
  expect_equal(resetMockAndCallFunction(DataSubset$Holdout, "training", 5, 6), 0)
  expect_equal(resetMockAndCallFunction(0, "validation", NULL, NULL), 0)
  expect_equal(resetMockAndCallFunction(DataSubset$Holdout, "validation", NULL, 5), 1)
  expect_equal(resetMockAndCallFunction(10, "validation", NULL, NULL), 0)
  expect_equal(resetMockAndCallFunction(0, "somethingElse", NULL, NULL), 0)
})

test_that("it can get anomaly over time plots metadata", {
  getStub <- stub(httr::GET)
  metadataJson <- fileToChar("responses/anomalyOverTimeMetadata.json")
  metadataResponse <- httr:::response(
    url = anomalyOverTimeMetadataGetUrl,
    status_code = 200L,
    content = charToRaw(metadataJson)
  )
  getStub$onCall(1)$returns(metadataResponse)
  metadata <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetAnomalyOverTimePlotsMetadata(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 1)

  expectedCols <- c(
    "resolutions", "holdoutStatuses", "holdoutMetadata", "backtestMetadata", "backtestStatuses"
  )
  ExpectHasKeys(metadata, expectedCols)
  expect_equal(metadata$resolutions, c("months", "quarters", "years"))
  expect_equal(
    metadata$backtestMetadata$training$startDate[1],
    datarobot.apicore::ParseRFC3339Timestamp("1952-08-02T00:00:00.000000Z")
  )
  expect_equal(
    metadata$backtestMetadata$training$endDate[1],
    datarobot.apicore::ParseRFC3339Timestamp("1960-02-02T00:00:00.000000Z")
  )
  expect_true(is.na(metadata$backtestMetadata$training$startDate[2]))
  expect_true(is.na(metadata$backtestMetadata$training$endDate[2]))
  expect_equal(
    metadata$backtestStatuses$training[1], DatetimeTrendPlotsStatuses$Completed
  )
  expect_equal(
    metadata$backtestStatuses$validation[2], DatetimeTrendPlotsStatuses$NotCompleted
  )
  expect_equal(
    metadata$backtestStatuses$training[2], DatetimeTrendPlotsStatuses$NotCompleted
  )
  expect_equal(
    metadata$holdoutStatuses$training, DatetimeTrendPlotsStatuses$NotCompleted
  )
  expect_equal(
    metadata$holdoutStatuses$validation, DatetimeTrendPlotsStatuses$Completed
  )
  expect_equal(
    metadata$holdoutMetadata$validation$startDate,
    datarobot.apicore::ParseRFC3339Timestamp("1960-07-02T00:00:00.000000Z")
  )
  expect_equal(
    metadata$holdoutMetadata$validation$endDate,
    datarobot.apicore::ParseRFC3339Timestamp("1960-12-02T00:00:00.000000Z")
  )
  expect_true(is.na(metadata$holdoutMetadata$training$startDate))
  expect_true(is.na(metadata$holdoutMetadata$training$endDate))
})

test_that("it can get anomaly over time plot", {
  getStub <- stub(httr::GET)
  plotJson <- fileToChar("responses/anomalyOverTimePlot.json")
  plotResponse <- httr:::response(
    url = anomalyOverTimePlotGetUrl,
    status_code = 200L,
    content = charToRaw(plotJson)
  )
  getStub$onCall(1)$returns(plotResponse)
  plot <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetAnomalyOverTimePlot(fakeModel, maxWait = 0)
  )
  expect_equal(getStub$calledTimes(), 1)

  expectedCols <- c(
    "startDate", "endDate", "bins", "resolution", "calendarEvents"
  )
  ExpectHasKeys(plot, expectedCols)
  expect_equal(
    plot$startDate, datarobot.apicore::ParseRFC3339Timestamp("1960-02-02T00:00:00.000000Z")
  )
  expect_equal(
    plot$endDate, datarobot.apicore::ParseRFC3339Timestamp("1960-07-02T00:00:00.000000Z")
  )
  expect_equal(plot$resolution, DatetimeTrendPlotsResolutions$Months)
  expect_equal(nrow(plot$bins), 5)
  expect_equal(
    plot$bins$startDate[1], datarobot.apicore::ParseRFC3339Timestamp("1960-02-02T00:00:00.000000Z")
  )
  expect_equal(
    plot$bins$endDate[1], datarobot.apicore::ParseRFC3339Timestamp("1960-03-02T00:00:00.000000Z")
  )
  expect_equal(plot$bins$predicted[1], 0.33)
  expect_equal(plot$bins$frequency[1], 1)
})

test_that("it can get anomaly over time plot", {
  getStub <- stub(httr::GET)
  plotJson <- fileToChar("responses/anomalyOverTimePlot.json")
  plotResponse <- httr:::response(
    url = anomalyOverTimePlotGetUrl,
    status_code = 200L,
    content = charToRaw(plotJson)
  )
  getStub$onCall(1)$returns(plotResponse)
  plot <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetAnomalyOverTimePlot(fakeModel, maxWait = 0)
  )
  expect_equal(getStub$calledTimes(), 1)

  expectedCols <- c(
    "startDate", "endDate", "bins", "resolution", "calendarEvents"
  )
  ExpectHasKeys(plot, expectedCols)
  expect_equal(
    plot$startDate, datarobot.apicore::ParseRFC3339Timestamp("1960-02-02T00:00:00.000000Z")
  )
  expect_equal(
    plot$endDate, datarobot.apicore::ParseRFC3339Timestamp("1960-07-02T00:00:00.000000Z")
  )
  expect_equal(plot$resolution, DatetimeTrendPlotsResolutions$Months)
  expect_equal(nrow(plot$bins), 5)
  expect_equal(
    plot$bins$startDate[1], datarobot.apicore::ParseRFC3339Timestamp("1960-02-02T00:00:00.000000Z")
  )
  expect_equal(
    plot$bins$endDate[1], datarobot.apicore::ParseRFC3339Timestamp("1960-03-02T00:00:00.000000Z")
  )
  expect_equal(plot$bins$predicted[1], 0.33)
  expect_equal(plot$bins$frequency[1], 1)
})

test_that("it can get anomaly over time plot preview", {
  getStub <- stub(httr::GET)
  plotJson <- fileToChar("responses/anomalyOverTimePreview.json")
  plotResponse <- httr:::response(
    url = anomalyOverTimePlotGetUrl,
    status_code = 200L,
    content = charToRaw(plotJson)
  )
  getStub$onCall(1)$returns(plotResponse)
  plot <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetAnomalyOverTimePlotPreview(fakeModel, maxWait = 0)
  )
  expect_equal(getStub$calledTimes(), 1)

  expectedCols <- c("startDate", "endDate", "bins", "predictionThreshold")
  ExpectHasKeys(plot, expectedCols)
  expect_equal(
    plot$startDate, datarobot.apicore::ParseRFC3339Timestamp("1960-02-02T00:00:00.000000Z")
  )
  expect_equal(
    plot$endDate, datarobot.apicore::ParseRFC3339Timestamp("1960-07-02T00:00:00.000000Z")
  )
  expect_equal(nrow(plot$bins), 1)
  expect_equal(
    plot$bins$startDate[1], datarobot.apicore::ParseRFC3339Timestamp("1960-06-02T00:00:00.000000Z")
  )
  expect_equal(
    plot$bins$endDate[1], datarobot.apicore::ParseRFC3339Timestamp("1960-07-02T00:00:00.000000Z")
  )
})

test_that("it can calculate anomaly over time plot automatically", {
  metadataJson <- fileToChar("responses/anomalyOverTimeMetadata.json")
  metadataResponse <- httr:::response(
    url = anomalyOverTimeMetadataGetUrl,
    status_code = 200L,
    content = charToRaw(metadataJson)
  )
  datetimeTrendPlotsResponse <- httr:::response(
    url = datetimeTrendPlotsComputeUrl,
    status_code = 202L,
    content = raw(0)
  )
  resetMockAndCallFunction <- function(backtest, source) {
    postStub <- stub(httr::POST)
    getStub <- stub(httr::GET)
    postStub$onCall(1)$returns(datetimeTrendPlotsResponse)
    getStub$onCall(1)$returns(metadataResponse)
    with_mock(
      "httr::POST" = postStub$f,
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      "datarobot:::JobIdFromResponse" = identity,
      "datarobot:::WaitForJobToComplete" = function(...) NULL,
      computeAnomalyOverTimePlotIfNotComputed(fakeModel, backtest, source, NULL)
    )
    expect_equal(getStub$calledTimes(), 1)
    return(postStub$calledTimes())
  }
  expect_equal(resetMockAndCallFunction(1, "training"), 1)
  expect_equal(resetMockAndCallFunction(DataSubset$Holdout, "training"), 1)
  expect_equal(resetMockAndCallFunction(1, "validation"), 1)
  expect_equal(resetMockAndCallFunction(0, "validation"), 0)
  expect_equal(resetMockAndCallFunction(DataSubset$Holdout, "validation"), 0)
  expect_equal(resetMockAndCallFunction(DataSubset$Holdout, "training"), 1)
  expect_equal(resetMockAndCallFunction(10, "validation"), 0)
  expect_equal(resetMockAndCallFunction(0, "somethingElse"), 0)
})
