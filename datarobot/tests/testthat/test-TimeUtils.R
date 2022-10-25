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
library(lubridate)

describe("formatRFC3339Timestamp", {
  test_that("formatRFC3339Timestamp handles vectors", {
    singleDate <- formatRFC3339Timestamp(Sys.Date())
    expect_equal(length(singleDate), 1)

    dates <- Sys.Date() + sort(sample(1:100, 10))
    manyDates <- formatRFC3339Timestamp(dates)
    expect_equal(length(manyDates), length(dates))
  })
})
