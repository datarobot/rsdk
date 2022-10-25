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

downloadDatasetAsCsvResponse <- httr:::response(
  url = aiCatalogUrl,
  status_code = 200L,
  headers = list(httr::content_type("text/csv")),
  content = read.csv("FakeData.csv")
)

test_that("DownloadDatasetAsCsv succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(downloadDatasetAsCsvResponse)
  downloadResponse <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DownloadDatasetAsCsv(fakeCatalogId)
  )

  expect_equal(getStub$calledTimes(), 1)
  expect_type(downloadResponse, "list")

  columnNames <- colnames(downloadResponse)
  # Check headers in CSV match what is in the FakeData CSV used
  expect_equal(
    columnNames,
    as.character(
      list(
        "crim",
        "zn",
        "indus",
        "chas",
        "nox",
        "rm",
        "age",
        "dis",
        "rad",
        "tax",
        "ptratio",
        "black",
        "lstat",
        "medv"
      )
    )
  )
})
