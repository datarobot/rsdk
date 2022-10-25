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

errorNotFoundMsg <- jsonlite::toJSON(list(message = list("Not Found")))

expectServiceStatsResponseShape <- function(response) {
  expect_s3_class(response, "deploymentServiceStats")
  expect_true(is.character(response$modelId))
  # period
  expect_true(is.list(response$period))
  expect_s3_class(response$period$start, "POSIXct")
  expect_false(is.na(response$period$start))
  expect_s3_class(response$period$end, "POSIXct")
  expect_false(is.na(response$period$end))
  # metrics
  expect_true(is.list(response$metrics))
  expect_true(is.integer(response$metrics$totalPredictions))
  expect_true(is.integer(response$metrics$totalRequests))
  expect_true(is.integer(response$metrics$slowRequests))
  expect_true(is.numeric(response$metrics$responseTime))
  expect_true(is.numeric(response$metrics$executionTime))
  expect_true(is.integer(response$metrics$medianLoad))
  expect_true(is.integer(response$metrics$peakLoad))
  expect_true(is.numeric(response$metrics$userErrorRate))
  expect_true(is.numeric(response$metrics$serverErrorRate))
  expect_true(is.integer(response$metrics$numConsumers))
  expect_true(is.numeric(response$metrics$cacheHitRatio))
  # segment analysis
  if (hasName(response, "segmentAttribute")) {
    expect_true(is.character(response$segmentAttribute))
  }
  if (hasName(response, "segmentValue")) {
    expect_true(is.character(response$segmentValue))
  }
}

getStatsUrl <- UrlJoin("deployments", fakeDeployment$id, "serviceStats")

test_that("GetDeploymentServiceStats() will return
          service health statistics for the correct deployment", {
  statsJson <- fileToChar("responses/deploymentServiceStats.json")
  statsResponse <- httr:::response(
    url = getStatsUrl,
    status_code = 200L,
    content = charToRaw(statsJson)
  )

  getStub <- stub(httr::GET)
  getStub$returns(statsResponse)
  serviceStats <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetDeployment" = function(...) fakeDeployment,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDeploymentServiceStats(fakeDeploymentId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expectServiceStatsResponseShape(serviceStats)
  expect_equal(serviceStats$modelId, fakeModelId)
})
test_that("GetDeploymentServiceStats() will return
          service health statistics for a deployment with no predictions
          in a specified reporting period", {
  noStatsJsonText <- fileToChar("responses/deploymentServiceStatsNone.json")
  noStatsJson <- jsonlite::fromJSON(noStatsJsonText)
  noStatsResponse <- httr:::response(
    url = getStatsUrl,
    status_code = 200L,
    content = charToRaw(noStatsJsonText)
  )
  startTime <- ISOdate(2021, 01, 05, 1, 0, 0, tz = "UTC")
  endTime <- ISOdate(2021, 01, 12, 1, 0, 0, tz = "UTC")

  getStub <- stub(httr::GET)
  getStub$returns(noStatsResponse)
  serviceStats <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetDeployment" = function(...) fakeDeployment,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDeploymentServiceStats(fakeDeploymentId,
      start = startTime,
      end = endTime
    )
  )
  expect_equal(getStub$calledTimes(), 1)
  expectServiceStatsResponseShape(serviceStats)
  expect_equal(serviceStats$modelId, fakeModelId)
  expect_equal(serviceStats$period$start, startTime)
  expect_equal(serviceStats$period$end, endTime)
})
test_that("GetDeploymentServiceStats() will return
          service health statistics with segment analysis", {
  statsSegmentJsonText <- fileToChar("responses/deploymentServiceStatsSegmentAnalysis.json")
  statsSegmentJson <- jsonlite::fromJSON(statsSegmentJsonText)
  statsResponse <- httr:::response(
    url = getStatsUrl,
    status_code = 200L,
    content = charToRaw(statsSegmentJsonText)
  )

  # "DataRobot-Remote-IP"
  requestSegmentAttribute <- statsSegmentJson$segmentAttribute
  # "192.168.0.1"
  requestSegmentValue <- statsSegmentJson$segmentValue

  getStub <- stub(httr::GET)
  getStub$returns(statsResponse)
  serviceStats <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetDeployment" = function(...) fakeDeployment,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDeploymentServiceStats(fakeDeploymentId,
      segmentAttribute = requestSegmentAttribute,
      segmentValue = requestSegmentValue
    )
  )
  expect_equal(getStub$calledTimes(), 1)
  expectServiceStatsResponseShape(serviceStats)
  expect_equal(serviceStats$modelId, fakeModelId)
  expect_equal(serviceStats$segmentAttribute, requestSegmentAttribute)
  expect_equal(serviceStats$segmentValue, requestSegmentValue)
})
test_that("GetDeploymentServiceStats() will fail when startTime has sub-hour resolution", {
  expect_error(GetDeploymentServiceStats(fakeDeployment$id,
    start = as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  ))
  expect_error(GetDeploymentServiceStats(fakeDeployment$id,
    start = as.POSIXct("2021-01-01 00:59:00", tz = "UTC")
  ))
})
test_that("GetDeploymentServiceStats() will fail when endTime has sub-hour resolution", {
  expect_error(GetDeploymentServiceStats(fakeDeployment$id,
    end = as.POSIXct("2021-01-01 00:00:01", tz = "UTC")
  ))
  expect_error(GetDeploymentServiceStats(fakeDeployment$id,
    end = as.POSIXct("2021-01-01 00:00:59", tz = "UTC")
  ))
})
test_that("GetDeploymentServiceStats() will fail when startTime is not defined with UTC timezone", {
  expect_error(GetDeploymentServiceStats(fakeDeployment$id,
    start = as.POSIXct("2021-01-01 00:00:01", tz = "EET")
  ))
})
test_that("GetDeploymentServiceStats() will fail when endTime is not defined with UTC timezone", {
  expect_error(GetDeploymentServiceStats(fakeDeployment$id,
    end = as.POSIXct("2021-01-01 00:00:01", tz = "EET")
  ))
})
test_that("GetDeploymentServiceStats() will fail when the deployment does not exist", {
  notFoundResponse <- httr:::response(
    url = getStatsUrl,
    status_code = 404L,
    content = charToRaw(errorNotFoundMsg)
  )

  getStub <- stub(httr::GET)
  getStub$returns(notFoundResponse)
  expect_error(
    with_mock(
      "httr::GET" = getStub$f,
      "datarobot::GetDeployment" = function(...) fakeDeployment,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetDeploymentServiceStats(fakeDeploymentId)
    ),
    "404"
  )
  expect_equal(getStub$calledTimes(), 1)
})
