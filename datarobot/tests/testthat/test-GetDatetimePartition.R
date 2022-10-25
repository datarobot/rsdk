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

# TODO Move to test fixture
datetimePartitionUrl <- UrlJoin(projectUrl, "datetimePartitioning")
datetimePartitionJson <- fileToChar("responses/datetimePartition.json")
completedDatetimePartitionResponse <- httr:::response(
  url = datetimePartitionUrl,
  status_code = 200L,
  content = charToRaw(datetimePartitionJson)
)

datetimePartNames <- c(
  "primaryTrainingEndDate", "primaryTrainingDuration", "backtests",
  "datetimePartitionColumn", "primaryTrainingStartDate",
  "availableTrainingStartDate", "availableTrainingDuration",
  "dateFormat", "autopilotDataSelectionMethod",
  "validationDuration", "holdoutEndDate", "numberOfBacktests", "gapStartDate",
  "availableTrainingEndDate", "holdoutStartDate", "gapEndDate",
  "holdoutDuration", "gapDuration", "cvMethod", "backtests",
  "featureSettings", "isTimeSeries", "isMultiSeries", "isCrossSeries"
)


test_that("Required parameters are present", {
  expect_error(GetDatetimePartition(), 'argument "project" is missing, with no default')
})

test_that("Function works with fakeProjectId", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedDatetimePartitionResponse)
  testReturn <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDatetimePartition(fakeProjectId)
  )
  expect_type(testReturn, "list")
  expect_s3_class(testReturn$backtests, "data.frame")
  ExpectHasKeys(testReturn, datetimePartNames)
})

dtPartitionNoHoldoutJson <- fileToChar("responses/datetimePartitionWithoutHoldout.json")
dtPartitionNoHoldoutResponse <- httr:::response(
  url = datetimePartitionUrl,
  status_code = 200L,
  content = charToRaw(dtPartitionNoHoldoutJson)
)

test_that("Function works with datetime partitioning without holdout", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(dtPartitionNoHoldoutResponse)

  testReturn <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDatetimePartition(fakeProjectId)
  )

  expect_type(testReturn, "list")
  expect_s3_class(testReturn$backtests, "data.frame")
  ExpectHasKeys(testReturn, datetimePartNames)
  # Also has availableTrainingRowCount, gapRowCount, holdoutRowCount,
  # primaryTrainingRowCount, projectId, totalRowCount
})


test_that("Function works with time series", {
  datetimePartitionJson <- fileToChar("responses/datetimePartition2.json")
  completedDatetimePartitionResponse <- httr:::response(
    url = datetimePartitionUrl,
    status_code = 200L,
    content = charToRaw(datetimePartitionJson)
  )
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedDatetimePartitionResponse)
  testReturn <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDatetimePartition(fakeProjectId)
  )
  expect_type(testReturn, "list")
  expect_true(testReturn$isTimeSeries)
  expect_false(testReturn$isMultiSeries)
  expect_false(testReturn$isCrossSeries)
})


test_that("Function works with multiseries", {
  datetimePartitionJson <- fileToChar("responses/multiseriesPartition.json")
  completedDatetimePartitionResponse <- httr:::response(
    url = datetimePartitionUrl,
    status_code = 200L,
    content = charToRaw(datetimePartitionJson)
  )
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedDatetimePartitionResponse)
  testReturn <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDatetimePartition(fakeProjectId)
  )
  expect_type(testReturn, "list")
  expect_true(testReturn$isTimeSeries)
  expect_true(testReturn$isMultiSeries)
  expect_false(testReturn$isCrossSeries)
})


test_that("Function works with cross series", {
  datetimePartitionJson <- fileToChar("responses/crossSeriesPartition.json")
  completedDatetimePartitionResponse <- httr:::response(
    url = datetimePartitionUrl,
    status_code = 200L,
    content = charToRaw(datetimePartitionJson)
  )
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedDatetimePartitionResponse)
  testReturn <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDatetimePartition(fakeProjectId)
  )
  expect_type(testReturn, "list")
  expect_true(testReturn$isTimeSeries)
  expect_true(testReturn$isMultiSeries)
  expect_true(testReturn$isCrossSeries)
})
