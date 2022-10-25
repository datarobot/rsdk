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

expectServiceStatsOverTimeResponseShape <- function(response) {
  expect_s3_class(response, "deploymentServiceStatsOverTime")
  expect_type(response$modelId, "character")
  expect_s3_class(response$summary, "data.frame")
  expect_gte(
    length(colnames(response$summary)), 3,
    "'start', 'end', and at least one metric"
  )
  expect_true(
    Contains(colnames(response$summary), c("start", "end")),
    "The summary column names should include 'start' and 'end'"
  )
  expect_gte(
    length(intersect(DeploymentServiceHealthMetric, colnames(response$summary))), 1,
    "The # of summary column names that match valid metric"
  )
  expect_s3_class(response$buckets, "data.frame")
  expect_setequal(colnames(response$summary), colnames(response$buckets))
  # segment analysis
  if (hasName(response, "segmentAttribute")) {
    expect_true(is.character(response$segmentAttribute))
  }
  if (hasName(response, "segmentValue")) {
    expect_true(is.character(response$segmentValue))
  }
}

getStotsUrl <- UrlJoin("deployments", fakeDeployment$id, "serviceStatsOverTime")
statsTotalPredictionsJson <- fileToChar("responses/serviceStatsOverTimeTotalPredictions.json")
statsTotalPredictionsResponse <- httr:::response(
  url = getStotsUrl,
  status_code = 200L,
  content = charToRaw(statsTotalPredictionsJson)
)

statsTotalRequestsJson <- fileToChar("responses/serviceStatsOverTimeTotalRequests.json")
statsTotalRequestsResponse <- httr:::response(
  url = getStotsUrl,
  status_code = 200L,
  content = charToRaw(statsTotalRequestsJson)
)

testMetrics <- c(
  DeploymentServiceHealthMetric$TotalPredictions,
  DeploymentServiceHealthMetric$TotalRequests
)

test_that("GetDeploymentServiceStatsOverTime() will return history for multiple metrics", {
  startTime <- ISOdate(2021, 1, 8, 6, tz = "UTC")
  endTime <- ISOdate(2021, 1, 15, 6, tz = "UTC")
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(statsTotalPredictionsResponse)
  getStub$onCall(2)$returns(statsTotalRequestsResponse)
  stot <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetDeployment" = function(...) fakeDeployment,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDeploymentServiceStatsOverTime(
      fakeDeploymentId,
      metrics = testMetrics,
      start = startTime,
      end = endTime
    )
  )
  expect_equal(getStub$calledTimes(), 2)
  expectServiceStatsOverTimeResponseShape(stot)
  expect_equal(stot$modelId, fakeModelId)
  expect_true(Contains(colnames(stot$summary), testMetrics))
  expect_equal(stot$summary$start, startTime)
  expect_equal(stot$summary$end, endTime)
})
test_that("GetDeploymentServiceStatsOverTime() will return
           history for a single metric with segment analysis", {
  stotsSegmentJsonText <- fileToChar("responses/serviceStatsOverTimeSegmentAnalysis.json")
  stotsSegmentJson <- jsonlite::fromJSON(stotsSegmentJsonText)
  stotsSegmentResponse <- httr:::response(
    url = getStotsUrl,
    status_code = 200L,
    content = charToRaw(stotsSegmentJsonText)
  )

  # "totalRequests"
  requestMetric <- stotsSegmentJson$metric
  # "DataRobot-Remote-IP"
  requestSegmentAttribute <- stotsSegmentJson$segmentAttribute
  # "10.11.12.13"
  requestSegmentValue <- stotsSegmentJson$segmentValue

  getStub <- stub(httr::GET)
  getStub$returns(stotsSegmentResponse)
  stots <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetDeployment" = function(...) fakeDeployment,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDeploymentServiceStatsOverTime(fakeDeploymentId,
      metrics = requestMetric,
      segmentAttribute = requestSegmentAttribute,
      segmentValue = requestSegmentValue
    )
  )
  expect_equal(getStub$calledTimes(), 1)
  expectServiceStatsOverTimeResponseShape(stots)
  expect_equal(stots$modelId, fakeModelId)
  expect_equal(stots$segmentAttribute, requestSegmentAttribute)
  expect_equal(stots$segmentValue, requestSegmentValue)
})
test_that("GetDeploymentServiceStatsOverTime() will warn when metrics is an empty vector", {
  expect_warning(GetDeploymentServiceStatsOverTime(fakeDeployment$id, metrics = NULL))
  expect_warning(GetDeploymentServiceStatsOverTime(fakeDeployment$id, metrics = c()))
})
test_that("GetDeploymentServiceStatsOverTime() will ignore invalid metrics", {
  invalidMetrics <- c("Not a metric", "Not this one either")

  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(statsTotalPredictionsResponse)
  getStub$onCall(2)$returns(statsTotalRequestsResponse)
  stot <- expect_warning(
    with_mock(
      "httr::GET" = getStub$f,
      "datarobot::GetDeployment" = function(...) fakeDeployment,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetDeploymentServiceStatsOverTime(
        fakeDeploymentId,
        metrics = c(testMetrics, invalidMetrics)
      )
    )
  )

  expect_equal(getStub$calledTimes(), 2, info = "The API should only have been called twice")
  expectServiceStatsOverTimeResponseShape(stot)
  expect_equal(stot$modelId, fakeModelId)
  expect_false(any(invalidMetrics %in% colnames(stot$summary)),
    info = "None of the invalid metrics should be in $summary"
  )
  expect_true(all(testMetrics %in% colnames(stot$summary)),
    info = "All valid test metrics should be in $summary"
  )
})
test_that("GetDeploymentServiceStatsOverTime() will fail when startTime has sub-hour resolution", {
  expect_error(GetDeploymentServiceStatsOverTime(fakeDeployment$id,
    start = as.POSIXct("2021-01-01 00:01:00",
      tz = "UTC"
    )
  ))
  expect_error(GetDeploymentServiceStatsOverTime(fakeDeployment$id,
    start = as.POSIXct("2021-01-01 00:59:00",
      tz = "UTC"
    )
  ))
})
test_that("GetDeploymentServiceStatsOverTime() will fail when endTime has sub-hour resolution", {
  expect_error(GetDeploymentServiceStatsOverTime(fakeDeployment$id,
    end = as.POSIXct("2021-01-01 00:00:01",
      tz = "UTC"
    )
  ))
  expect_error(GetDeploymentServiceStatsOverTime(fakeDeployment$id,
    end = as.POSIXct("2021-01-01 00:00:59",
      tz = "UTC"
    )
  ))
})
test_that("GetDeploymentServiceStatsOverTime() will fail
           when startTime is not defined with UTC timezone", {
  expect_error(GetDeploymentServiceStatsOverTime(fakeDeployment$id,
    start = as.POSIXct("2021-01-01 00:00:01",
      tz = "EET"
    )
  ))
})
test_that("GetDeploymentServiceStatsOverTime() will fail
           when endTime is not defined with UTC timezone", {
  expect_error(GetDeploymentServiceStatsOverTime(fakeDeployment$id,
    end = as.POSIXct("2021-01-01 00:00:01",
      tz = "EET"
    )
  ))
})
test_that("GetDeploymentServiceStatsOverTime() will fail when the deployment does not exist", {
  notFoundResponse <- httr:::response(
    url = getStotsUrl,
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
      GetDeploymentServiceStatsOverTime(fakeDeploymentId)
    ),
    "404"
  )
})
