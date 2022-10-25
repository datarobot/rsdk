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

test_that("validateReportingPeriodTime() checks for POSIXlt compatible object", {
  expect_error(validateReportingPeriodTime("not a date"))
  expect_error(validateReportingPeriodTime(NA))
  expect_error(validateReportingPeriodTime(1))
})
test_that("validateReportingPeriodTime() checks if minutes are non-zero", {
  expect_error(validateReportingPeriodTime(as.POSIXct("2021-01-01 00:01:00")))
  expect_error(validateReportingPeriodTime(as.POSIXct("2021-01-01 00:59:00")))
})
test_that("validateReportingPeriodTime() checks if seconds are non-zero", {
  expect_error(validateReportingPeriodTime(as.POSIXct("2021-01-01 00:00:01")))
  expect_error(validateReportingPeriodTime(as.POSIXct("2021-01-01 00:00:59")))
})
test_that("validateReportingPeriodTime() errors when timezone is not UTC or equivalent", {
  expect_error(validateReportingPeriodTime(as.POSIXct("2021-01-01 00:00:00", tz = "EET")))
})
test_that("validateReportingPeriodTime() succeeds on the top of the hour and the timezone is UTC or
          equivalent", {
  expect_true(validateReportingPeriodTime(as.POSIXct("2021-01-01 00:00:00", tz = "UTC")))
  expect_true(validateReportingPeriodTime(as.POSIXct("2021-01-01 01:00:00", tz = "UTC")))
  expect_true(validateReportingPeriodTime(as.POSIXct("2021-01-01 23:00:00", tz = "UTC")))
  expect_true(validateReportingPeriodTime(as.POSIXct("2021-01-01 00:00:00", tz = "GMT")))
})
