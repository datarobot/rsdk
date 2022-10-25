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
testAccuracyMetrics <- DeploymentAccuracyMetric$Gamma.Deviance

expectAccuracyOverTimeResponseShape <- function(response) {
  expect_s3_class(response, "deploymentAccuracyOverTime")
  expect_type(response$modelId, "character")
  # summary
  expect_s3_class(response$summary, "data.frame")
  expect_true(Contains(
    colnames(response$summary),
    c("start", "end", "sampleSize")
  ))
  expect_type(response$summary$sampleSize, "integer")
  expect_s3_class(response$summary$start, "POSIXct")
  expect_s3_class(response$summary$end, "POSIXct")
  # buckets
  expect_s3_class(response$buckets, "data.frame")
  expect_true(Contains(
    colnames(response$buckets),
    c("start", "end", "sampleSize")
  ))
  expect_type(response$buckets$sampleSize, "integer")
  expect_s3_class(response$buckets$start, "POSIXct")
  expect_s3_class(response$buckets$end, "POSIXct")
  # baseline
  expect_s3_class(response$baseline, "data.frame")
  expect_true(Contains(
    colnames(response$baseline),
    c("start", "end", "sampleSize")
  ))
  expect_type(response$baseline$sampleSize, "integer")
  expect_s3_class(response$baseline$start, "POSIXct")
  expect_s3_class(response$baseline$end, "POSIXct")
  # segment analysis
  if (hasName(response, "segmentAttribute")) {
    expect_type(response$segmentAttribute, "character")
  }
  if (hasName(response, "segmentValue")) {
    expect_type(response$segmentValue, "character")
  }
}

getAccuracyOverTimeUrl <- UrlJoin("deployments", fakeDeployment$id, "accuracyOverTime")

test_that("GetDeploymentAccuracyOverTime() succeeds for a valid deployment", {
  accuracyOverTimeText <- fileToChar("responses/deploymentAccuracyOverTime-1.json")
  aotResponse <- httr:::response(
    url = getAccuracyOverTimeUrl,
    status_code = 200L,
    content = charToRaw(accuracyOverTimeText)
  )

  getStub <- stub(httr::GET)
  getStub$returns(aotResponse)
  aot <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetDeployment" = function(...) fakeDeployment,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDeploymentAccuracyOverTime(
      fakeDeployment$id,
      testAccuracyMetrics
    )
  )
  expect_equal(getStub$calledTimes(), 1)
  expectAccuracyOverTimeResponseShape(aot)
  expect_known_output(aot,
    "snapshots/GetDeploymentAccuracyOverTimeSucceeds",
    print = TRUE,
    width = 120
  )
})

test_that("GetDeploymentAccuracyOverTime() succeeds for multiple metrics", {
  rmseText <- fileToChar("responses/deploymentAccuracyOverTime-2.json")
  rmseResponse <- httr:::response(
    url = getAccuracyOverTimeUrl,
    status_code = 200L,
    content = charToRaw(rmseText)
  )
  gammaDevianceText <- fileToChar("responses/deploymentAccuracyOverTime-1.json")
  gammaDevianceResponse <- httr:::response(
    url = getAccuracyOverTimeUrl,
    status_code = 200L,
    content = charToRaw(gammaDevianceText)
  )

  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(rmseResponse)
  getStub$onCall(2)$returns(gammaDevianceResponse)
  aot <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetDeployment" = function(...) fakeDeployment,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDeploymentAccuracyOverTime(
      fakeDeployment$id,
      c(
        DeploymentAccuracyMetric$RMSE,
        DeploymentAccuracyMetric$Gamma.Deviance
      )
    )
  )
  expect_equal(getStub$calledTimes(), 2)
  expectAccuracyOverTimeResponseShape(aot)
  expect_known_output(aot,
    "snapshots/GetDeploymentAccuracyOverTimeSucceedsMultiple",
    print = TRUE,
    width = 120
  )
})

test_that("GetDeploymentAccuracyOverTime() succeeds when accuracy is not available", {
  aotText <- fileToChar("responses/deploymentAccuracyOverTimeNone.json")
  aotJson <- jsonlite::fromJSON(aotText)
  aotResponse <- httr:::response(
    url = getAccuracyOverTimeUrl,
    status_code = 200L,
    content = charToRaw(aotText)
  )

  # ISOdate(2021, 01, 09, 5, 0, 0, tz = "UTC")
  startTime <- lubridate::ymd_hms(aotJson$summary$period$start)
  # ISOdate(2021, 01, 16, 5, 0, 0, tz = "UTC")
  endTime <- lubridate::ymd_hms(aotJson$summary$period$end)

  getStub <- stub(httr::GET)
  getStub$returns(aotResponse)
  aot <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetDeployment" = function(...) fakeDeployment,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDeploymentAccuracyOverTime(fakeDeploymentId,
      testAccuracyMetrics,
      start = startTime,
      end = endTime
    )
  )
  expect_equal(getStub$calledTimes(), 1)
  expectAccuracyOverTimeResponseShape(aot)
  expect_equal(aot$modelId, aotJson$modelId)
  expect_equal(aot$summary$start, startTime)
  expect_equal(aot$summary$end, endTime)
  expect_known_output(aot,
    "snapshots/GetDeploymentAccuracyOverTimeSucceedsWithNA",
    print = TRUE,
    width = 120
  )
})

test_that("GetDeploymentAccuracyOverTime() succeeds with segment analysis", {
  aotText <- fileToChar("responses/deploymentAccuracyOverTimeSegmentAnalysis.json")
  aotJson <- jsonlite::fromJSON(aotText)
  aotResponse <- httr:::response(
    url = getAccuracyOverTimeUrl,
    status_code = 200L,
    content = charToRaw(aotText)
  )

  # "DataRobot-Remote-IP"
  requestSegmentAttribute <- aotJson$segmentAttribute
  # "10.11.12.13"
  requestSegmentValue <- aotJson$segmentValue

  getStub <- stub(httr::GET)
  getStub$returns(aotResponse)
  aot <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetDeployment" = function(...) fakeDeployment,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDeploymentAccuracyOverTime(fakeDeploymentId,
      testAccuracyMetrics,
      segmentAttribute = requestSegmentAttribute,
      segmentValue = requestSegmentValue
    )
  )
  expect_equal(getStub$calledTimes(), 1)
  expectAccuracyOverTimeResponseShape(aot)
  expect_equal(aot$segmentAttribute, requestSegmentAttribute)
  expect_equal(aot$segmentValue, requestSegmentValue)
  expect_known_output(aot,
    "snapshots/GetDeploymentAccuracyOverTimeSucceedsWithSegmentAnalysis",
    print = TRUE,
    width = 120
  )
})

test_that("GetDeploymentServiceStatsOverTime() will warn when metrics is an empty vector", {
  expect_warning(GetDeploymentAccuracyOverTime(
    fakeDeployment$id,
    NULL
  ), "Metrics should not be an empty vector")
  expect_warning(GetDeploymentAccuracyOverTime(
    fakeDeployment$id,
    c()
  ), "Metrics should not be an empty vector")
})

test_that("GetDeploymentServiceStatsOverTime() will warn when a metric is invalid", {
  expect_warning(GetDeploymentAccuracyOverTime(
    fakeDeployment$id,
    NA
  ), "These metrics are not valid")
  expect_warning(GetDeploymentAccuracyOverTime(
    fakeDeployment$id,
    ""
  ), "These metrics are not valid")
  expect_warning(GetDeploymentAccuracyOverTime(
    fakeDeployment$id,
    "not-a-valid-metric"
  ), "These metrics are not valid")
  expect_warning(GetDeploymentAccuracyOverTime(
    fakeDeployment$id,
    "GammaDeviance"
  ), "These metrics are not valid",
  info = "API will expect 'Gamma Deviance', not 'GammaDeviance'"
  )
})

test_that("GetDeploymentAccuracyOverTime() will fail when startTime has sub-hour resolution", {
  expect_error(GetDeploymentAccuracyOverTime(
    fakeDeployment$id,
    testAccuracyMetrics,
    start = as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  ))
  expect_error(GetDeploymentAccuracyOverTime(
    fakeDeployment$id,
    testAccuracyMetrics,
    start = as.POSIXct("2021-01-01 00:59:00", tz = "GMT")
  ))
})

test_that("GetDeploymentAccuracyOverTime() will fail when endTime has sub-hour resolution", {
  expect_error(GetDeploymentAccuracyOverTime(
    fakeDeployment$id,
    testAccuracyMetrics,
    end = as.POSIXct("2021-01-01 00:00:01", tz = "UTC")
  ))
  expect_error(GetDeploymentAccuracyOverTime(
    fakeDeployment$id,
    testAccuracyMetrics,
    end = as.POSIXct("2021-01-01 00:00:59", tz = "UTC")
  ))
})

test_that("GetDeploymentAccuracyOverTime() will fail when
          startTime is not defined with UTC timezone", {
  expect_error(GetDeploymentAccuracyOverTime(
    fakeDeployment$id,
    testAccuracyMetrics,
    start = as.POSIXct("2021-01-01 00:00:01", tz = "EET")
  ))
})

test_that("will fail when endTime is not defined with UTC timezone", {
  expect_error(GetDeploymentAccuracyOverTime(
    fakeDeployment$id,
    testAccuracyMetrics,
    end = as.POSIXct("2021-01-01 00:00:01", tz = "EET")
  ))
})

test_that("GetDeploymentAccuracyOverTime() will fail when the deployment does not exist", {
  notFoundResponse <- httr:::response(
    url = getAccuracyOverTimeUrl,
    status_code = 404L,
    content = charToRaw(errorNotFoundMsg)
  )

  getStub <- stub(httr::GET)
  getStub$returns(notFoundResponse)
  expect_error(
    with_mock(
      "httr::GET" = getStub$f,
      "datarobot::GetDeployment" = function(...) NA,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetDeploymentAccuracyOverTime(
        fakeDeploymentId,
        testAccuracyMetrics
      )
    ),
    "404"
  )
  expect_equal(getStub$calledTimes(), 1)
})
