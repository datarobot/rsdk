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

expectAccuracyResponseShape <- function(response) {
  expect_s3_class(response, "deploymentAccuracy")
  expect_type(response$modelId, "character")
  # period
  expect_type(response$period, "list")
  expect_s3_class(response$period$start, "POSIXct")
  expect_false(is.na(response$period$start))
  expect_s3_class(response$period$end, "POSIXct")
  expect_false(is.na(response$period$end))
  # metrics
  expect_s3_class(response$metrics, "data.frame")
  expect_equal(
    colnames(response$metrics),
    c("metric", "baselineValue", "value", "percentChange")
  )
  expect_true(
    Contains(DeploymentAccuracyMetric, response$metrics$metric),
    info = "All values in response$metrics$metric
                should be listed in DeploymentAccuracyMetric.",
    label = "All returned metric names are valid"
  )
  # segment analysis
  if (hasName(response, "segmentAttribute")) {
    expect_type(response$segmentAttribute, "character")
  }
  if (hasName(response, "segmentValue")) {
    expect_type(response$segmentValue, "character")
  }
}

expectAccuracyResponseMatchesAPIResponse <- function(response, apiJsonResponse) {
  # model id
  expect_equal(response$modelId, apiJsonResponse$modelId)
  # period
  expect_equal(response$period$start, lubridate::ymd_hms(apiJsonResponse$period$start))
  expect_equal(response$period$end, lubridate::ymd_hms(apiJsonResponse$period$end))
  # metrics
  expect_equal(nrow(response$metrics),
    length(apiJsonResponse$metrics),
    label = "# metrics in the returned df"
  )
  for (m in response$metrics$metric) {
    # expected: find the corresponding list in the JSON response, and convert nulls to NAs
    pos <- match(m, names(apiJsonResponse$metrics))
    expectedMetric <- nullToNA(apiJsonResponse$metrics[[pos]])
    # actual: subset of the DF
    actualMetric <- response$metrics[response$metrics$metric == m, ]
    # then compare actual metric values to expected metric values
    expect_equal(actualMetric$baselineValue[[1]], expectedMetric$baselineValue)
    expect_equal(actualMetric$value[[1]], expectedMetric$value)
    expect_equal(actualMetric$baselineValue[[1]], expectedMetric$baselineValue)
  }
  # segment analysis
  if (hasName(response, "segmentAttribute")) {
    expect_equal(response$segmentAttribute, apiJsonResponse$segmentAttribute)
  }
  if (hasName(response, "segmentValue")) {
    expect_equal(response$segmentValue, apiJsonResponse$segmentValue)
  }
}

getAccuracyUrl <- UrlJoin("deployments", fakeDeployment$id, "accuracy")

test_that("GetDeploymentAccuracy() will return
          service accuracy for the correct deployment", {
  accuracyText <- fileToChar("responses/deploymentAccuracy.json")
  accuracyResponse <- httr:::response(
    url = getAccuracyUrl,
    status_code = 200L,
    content = charToRaw(accuracyText)
  )

  getStub <- stub(httr::GET)
  getStub$returns(accuracyResponse)
  accuracy <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetDeployment" = function(...) fakeDeployment,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDeploymentAccuracy(fakeDeploymentId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expectAccuracyResponseShape(accuracy)
  expectAccuracyResponseMatchesAPIResponse(accuracy, jsonlite::fromJSON(accuracyText))
})
test_that("GetDeploymentAccuracy() will return
          with missing values when accuracy is not available", {
  naAccuracyJsonText <- fileToChar("responses/deploymentAccuracyNone.json")
  naAccuracyJson <- jsonlite::fromJSON(naAccuracyJsonText)
  naAccuracyResponse <- httr:::response(
    url = getAccuracyUrl,
    status_code = 200L,
    content = charToRaw(naAccuracyJsonText)
  )
  # ISOdate(2021, 01, 09, 5, 0, 0, tz = "UTC")
  startTime <- lubridate::ymd_hms(naAccuracyJson$period$start)
  # ISOdate(2021, 01, 16, 5, 0, 0, tz = "UTC")
  endTime <- lubridate::ymd_hms(naAccuracyJson$period$end)

  getStub <- stub(httr::GET)
  getStub$returns(naAccuracyResponse)
  accuracy <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetDeployment" = function(...) fakeDeployment,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDeploymentAccuracy(fakeDeploymentId,
      start = startTime,
      end = endTime
    )
  )
  expect_equal(getStub$calledTimes(), 1)
  expectAccuracyResponseShape(accuracy)
  expect_equal(accuracy$modelId, naAccuracyJson$modelId)
  expect_equal(accuracy$period$start, startTime)
  expect_equal(accuracy$period$end, endTime)
  expect_true(all(is.na(accuracy$metrics$baselineValue)))
  expect_true(all(is.na(accuracy$metrics$value)))
  expect_true(all(is.na(accuracy$metrics$percentChange)))
})
test_that("GetDeploymentAccuracy() will return service health statistics with segment analysis", {
  statsSegmentJsonText <- fileToChar("responses/deploymentAccuracySegmentAnalysis.json")
  statsSegmentJson <- jsonlite::fromJSON(statsSegmentJsonText)
  statsResponse <- httr:::response(
    url = getAccuracyUrl,
    status_code = 200L,
    content = charToRaw(statsSegmentJsonText)
  )

  # "DataRobot-Remote-IP"
  requestSegmentAttribute <- statsSegmentJson$segmentAttribute
  # "192.168.0.1"
  requestSegmentValue <- statsSegmentJson$segmentValue

  getStub <- stub(httr::GET)
  getStub$returns(statsResponse)
  accuracy <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetDeployment" = function(...) fakeDeployment,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDeploymentAccuracy(fakeDeploymentId,
      segmentAttribute = requestSegmentAttribute,
      segmentValue = requestSegmentValue
    )
  )
  expect_equal(getStub$calledTimes(), 1)
  expectAccuracyResponseShape(accuracy)
  expectAccuracyResponseMatchesAPIResponse(accuracy, statsSegmentJson)
})
test_that("GetDeploymentAccuracy() will fail when startTime has sub-hour resolution", {
  expect_error(GetDeploymentAccuracy(fakeDeployment$id,
    start = as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  ))
  expect_error(GetDeploymentAccuracy(fakeDeployment$id,
    start = as.POSIXct("2021-01-01 00:59:00", tz = "UTC")
  ))
})
test_that("GetDeploymentAccuracy() will fail when endTime has sub-hour resolution", {
  expect_error(GetDeploymentAccuracy(fakeDeployment$id,
    end = as.POSIXct("2021-01-01 00:00:01", tz = "UTC")
  ))
  expect_error(GetDeploymentAccuracy(fakeDeployment$id,
    end = as.POSIXct("2021-01-01 00:00:59", tz = "UTC")
  ))
})
test_that("GetDeploymentAccuracy() will fail when startTime is not defined with UTC timezone", {
  expect_error(GetDeploymentAccuracy(fakeDeployment$id,
    start = as.POSIXct("2021-01-01 00:00:01", tz = "EET")
  ))
})
test_that("will fail when endTime is not defined with UTC timezone", {
  expect_error(GetDeploymentAccuracy(fakeDeployment$id,
    end = as.POSIXct("2021-01-01 00:00:01", tz = "EET")
  ))
})
test_that("GetDeploymentAccuracy() will fail when the deployment does not exist", {
  notFoundResponse <- httr:::response(
    url = getAccuracyUrl,
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
      GetDeploymentAccuracy(fakeDeploymentId)
    ),
    "404"
  )
  expect_equal(getStub$calledTimes(), 1)
})
