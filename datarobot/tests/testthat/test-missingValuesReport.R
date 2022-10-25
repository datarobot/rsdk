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

test_that("it can get a missing values report", {
  getStub <- stub(httr::GET)
  getMissingValuesReportUrl <- UrlJoin(projectUrl, "models", fakeModelId, "missingValuesReport")
  getMissingValuesReportJson <- fileToChar("responses/missingValuesReport.json")
  missingValuesReportResponse <- httr:::response(
    url = getMissingValuesReportUrl,
    status_code = 200L,
    content = charToRaw(getMissingValuesReportJson)
  )
  getStub$onCall(1)$returns(missingValuesReportResponse)
  missingValuesReport <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetMissingValuesReport(fakeProject, fakeModelId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_true("ID" %in% names(missingValuesReport))
  expect_true("flight_type" %in% names(missingValuesReport))
  ExpectHasKeys(missingValuesReport[[1]], c("missingCount", "missingPercentage", "tasks", "type"))
  expect_equal(missingValuesReport[[1]]$missingCount, 1)
  expect_equal(missingValuesReport[[1]]$missingPercentage, 0.1)
  expect_equal(missingValuesReport[[1]]$type, "Categorical")
  ExpectHasKeys(missingValuesReport[[1]]$tasks[[1]], c("descriptions", "name", "id"))
  expect_equal(missingValuesReport[[1]]$tasks[[1]]$id, "1")
  expect_equal(
    missingValuesReport[[1]]$tasks[[1]]$name,
    "Ordinal encoding of categorical variables"
  )
  expect_equal(missingValuesReport[[1]]$tasks[[1]]$descriptions, "Imputed value: -2")
})
