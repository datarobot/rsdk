# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(testthat)
library(stubthat)

test_that("Datetime partition with empty backtests", {
  partition <- CreateDatetimePartitionSpecification("dateColumn",
    autopilotDataSelectionMethod = NULL,
    validationDuration = NULL,
    holdoutStartDate = NULL,
    holdoutDuration = NULL,
    gapDuration = NULL,
    numberOfBacktests = NULL,
    backtests = NULL
  )
  expect_s3_class(partition, "partition")
  expect_equal(partition$cvMethod, "datetime")
  expect_equal(partition$datetimePartitionColumn, "dateColumn")
  expect_false(partition$useTimeSeries)
  expect_false(partition$defaultToKnownInAdvance)
})


test_that("Datetime partition with feature settings", {
  partition <- CreateDatetimePartitionSpecification("dateColumn",
    featureSettings = list(
      featureName = "Product_offers",
      knownInAdvance = TRUE
    )
  )
  expect_s3_class(partition, "partition")
  expect_equal(partition$cvMethod, "datetime")
  expect_equal(partition$datetimePartitionColumn, "dateColumn")
  expect_false(partition$useTimeSeries)
  expect_false(partition$defaultToKnownInAdvance)
  expect_true(partition$featureSettings$knownInAdvance)
})

test_that("Datetime partition with do not derive", {
  partition <- CreateDatetimePartitionSpecification("dateColumn",
    featureSettings = list(
      featureName = "Product_offers",
      doNotDerive = TRUE
    )
  )
  expect_s3_class(partition, "partition")
  expect_equal(partition$cvMethod, "datetime")
  expect_equal(partition$datetimePartitionColumn, "dateColumn")
  expect_false(partition$useTimeSeries)
  expect_false(partition$defaultToKnownInAdvance)
  expect_true(partition$featureSettings$doNotDerive)
})


test_that("Datetime partition defaulting to known in advance", {
  partition <- CreateDatetimePartitionSpecification("dateColumn",
    defaultToKnownInAdvance = TRUE
  )
  expect_s3_class(partition, "partition")
  expect_equal(partition$cvMethod, "datetime")
  expect_equal(partition$datetimePartitionColumn, "dateColumn")
  expect_false(partition$useTimeSeries)
  expect_true(partition$defaultToKnownInAdvance)
})


test_that("Datetime partition defaulting to known in advance", {
  partition <- CreateDatetimePartitionSpecification("dateColumn",
    useTimeSeries = TRUE
  )
  expect_s3_class(partition, "partition")
  expect_equal(partition$cvMethod, "datetime")
  expect_equal(partition$datetimePartitionColumn, "dateColumn")
  expect_true(partition$useTimeSeries)
  expect_false(partition$defaultToKnownInAdvance)
})


partition <- CreateDatetimePartitionSpecification("dateColumn",
  treatAsExponential = TreatAsExponential$Always,
  differencingMethod = DifferencingMethod$Seasonal,
  periodicities = list(
    list(
      "timeSteps" = 10,
      "timeUnit" = "HOUR"
    ),
    list(
      "timeSteps" = 600,
      "timeUnit" = "MINUTE"
    ),
    list(
      "timeSteps" = 7,
      "timeUnit" = "DAY"
    )
  )
)
test_that("Datetime partition with exponential, differencing, and periodicities", {
  expect_s3_class(partition, "partition")
  expect_equal(partition$cvMethod, "datetime")
  expect_equal(partition$datetimePartitionColumn, "dateColumn")
  expect_false(partition$useTimeSeries)
  expect_false(partition$defaultToKnownInAdvance)
  expect_equal(partition$treatAsExponential, "always")
  expect_equal(partition$differencingMethod, "seasonal")
  expect_type(partition$periodicities, "list")
  expect_equal(length(partition$periodicities), 3)
  expect_equal(partition$periodicities[[1]]$timeSteps, 10)
  expect_equal(partition$periodicities[[1]]$timeUnit, "HOUR")
  expect_equal(partition$periodicities[[2]]$timeSteps, 600)
  expect_equal(partition$periodicities[[2]]$timeUnit, "MINUTE")
  expect_equal(partition$periodicities[[3]]$timeSteps, 7)
  expect_equal(partition$periodicities[[3]]$timeUnit, "DAY")
})


partition <- CreateDatetimePartitionSpecification("dateColumn",
  windowsBasisUnit = "ROW",
  periodicities = list(list(
    "timeSteps" = 10,
    "timeUnit" = "ROW"
  ))
)
test_that("Datetime partition with windowsBasisUnit", {
  expect_s3_class(partition, "partition")
  expect_equal(partition$cvMethod, "datetime")
  expect_equal(partition$datetimePartitionColumn, "dateColumn")
  expect_false(partition$useTimeSeries)
  expect_false(partition$defaultToKnownInAdvance)
  expect_equal(partition$windowsBasisUnit, "ROW")
  expect_type(partition$periodicities, "list")
  expect_equal(length(partition$periodicities), 1)
  expect_equal(partition$periodicities[[1]]$timeSteps, 10)
  expect_equal(partition$periodicities[[1]]$timeUnit, "ROW")
})


partition <- CreateDatetimePartitionSpecification("dateColumn", useTimeSeries = TRUE)
test_that("True time series partition", {
  expect_s3_class(partition, "partition")
  expect_equal(partition$cvMethod, "datetime")
  expect_equal(partition$datetimePartitionColumn, "dateColumn")
  expect_true(partition$useTimeSeries)
})


partition <- CreateDatetimePartitionSpecification("dateColumn",
  useTimeSeries = TRUE,
  multiseriesIdColumns = list("series_id")
)
test_that("Multiseries partition", {
  expect_s3_class(partition, "partition")
  expect_equal(partition$cvMethod, "datetime")
  expect_equal(partition$datetimePartitionColumn, "dateColumn")
  expect_true(partition$useTimeSeries)
  expect_false(partition$defaultToKnownInAdvance)
  expect_equal(partition$multiseriesIdColumns[[1]], "series_id")
})

partition <- CreateDatetimePartitionSpecification("dateColumn",
  useTimeSeries = TRUE,
  multiseriesIdColumns = "series_id"
)
test_that("Multiseries partition - single element", {
  expect_s3_class(partition, "partition")
  expect_equal(partition$cvMethod, "datetime")
  expect_equal(partition$datetimePartitionColumn, "dateColumn")
  expect_true(partition$useTimeSeries)
  expect_false(partition$defaultToKnownInAdvance)
  expect_equal(partition$multiseriesIdColumns[[1]], "series_id")
})


partition <- CreateDatetimePartitionSpecification("dateColumn",
  useTimeSeries = TRUE,
  multiseriesIdColumns = "series_id",
  useCrossSeries = TRUE
)
test_that("Cross series partition - default", {
  expect_s3_class(partition, "partition")
  expect_equal(partition$cvMethod, "datetime")
  expect_equal(partition$datetimePartitionColumn, "dateColumn")
  expect_true(partition$useTimeSeries)
  expect_false(partition$defaultToKnownInAdvance)
  expect_equal(partition$multiseriesIdColumns[[1]], "series_id")
  expect_true(partition$useCrossSeriesFeatures)
  expect_true(is.null(partition$aggregationType))
})

partition <- CreateDatetimePartitionSpecification("dateColumn",
  useTimeSeries = TRUE,
  multiseriesIdColumns = "series_id",
  useCrossSeries = TRUE,
  aggregationType = SeriesAggregationType$Average
)
test_that("Cross series partition - average", {
  expect_s3_class(partition, "partition")
  expect_equal(partition$cvMethod, "datetime")
  expect_equal(partition$datetimePartitionColumn, "dateColumn")
  expect_true(partition$useTimeSeries)
  expect_false(partition$defaultToKnownInAdvance)
  expect_equal(partition$multiseriesIdColumns[[1]], "series_id")
  expect_true(partition$useCrossSeriesFeatures)
  expect_equal(partition$aggregationType, SeriesAggregationType$Average)
})

partition <- CreateDatetimePartitionSpecification("dateColumn",
  useTimeSeries = TRUE,
  multiseriesIdColumns = "series_id",
  useCrossSeries = TRUE,
  aggregationType = SeriesAggregationType$Total
)
test_that("Cross series partition - total", {
  expect_s3_class(partition, "partition")
  expect_equal(partition$cvMethod, "datetime")
  expect_equal(partition$datetimePartitionColumn, "dateColumn")
  expect_true(partition$useTimeSeries)
  expect_false(partition$defaultToKnownInAdvance)
  expect_equal(partition$multiseriesIdColumns[[1]], "series_id")
  expect_true(partition$useCrossSeriesFeatures)
  expect_equal(partition$aggregationType, SeriesAggregationType$Total)
})


partition <- CreateDatetimePartitionSpecification("dateColumn",
  useTimeSeries = TRUE,
  calendar = fakeCalendar
)
test_that("Time series partition with calendar", {
  expect_s3_class(partition, "partition")
  expect_equal(partition$cvMethod, "datetime")
  expect_equal(partition$datetimePartitionColumn, "dateColumn")
  expect_true(partition$useTimeSeries)
  expect_equal(partition$calendarId, fakeCalendarId)
})


partition <- CreateDatetimePartitionSpecification("dateColumn",
  useTimeSeries = TRUE,
  multiseriesIdColumns = "series_id",
  useCrossSeries = TRUE,
  crossSeriesGroupByColumns = "group"
)
test_that("Cross series partition with crossSeriesGroupByColumns", {
  expect_false(partition$defaultToKnownInAdvance)
  expect_equal(partition$multiseriesIdColumns[[1]], "series_id")
  expect_true(partition$useCrossSeriesFeatures)
  expect_equal(partition$crossSeriesGroupByColumns, "group")
})
