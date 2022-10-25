# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
#
# helper-ResidualsCharts.R
# Helper functions for testing Residuals Charts
#
expectResidualsChartDataShape <- function(chart) {
  expect_true(is.numeric(chart$residualMean))
  expect_true(is.numeric(chart$coefficientOfDetermination))
  # data
  chartData <- chart$data
  expect_s3_class(chartData, "data.frame")
  expect_equal(ncol(chartData), 4)
  expect_equal(names(chartData), c("actual", "predicted", "residual", "rowNumber"))
  expect_true(is.numeric(chartData$actual))
  expect_true(is.numeric(chartData$predicted))
  expect_true(is.numeric(chartData$residual))
  expect_type(chartData$rowNumber, "integer")
  # histogram
  expect_s3_class(chart$histogram, "data.frame")
  expect_true(is.numeric(chart$histogram$intervalStart))
  expect_true(is.numeric(chart$histogram$intervalEnd))
  expect_type(chart$histogram$occurrences, "integer")
}

expectResidualsChartForCorrectSource <- function(chart, source) {
  expect_type(chart, "list")
  expect_equal(length(chart), length(source))
  expect_equal(names(chart), source)
}

expectResidualsChartHasValidSource <- function(chart) {
  for (nm in names(chart)) {
    expect_true(nm %in% DataPartition)
  }
}
