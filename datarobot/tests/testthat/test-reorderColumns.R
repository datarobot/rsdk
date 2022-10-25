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

describe("reorderColumns", {
  input <- data.frame(Time = c(1, 2), In = c(2, 3), Out = c(3, 4))

  test_that("fails for a non data.frame input", {
    vars <- c("First" = 1)

    expect_error(reorderColumns(NULL, vars))
    expect_error(reorderColumns(NA, vars))
    expect_error(reorderColumns(list(), vars))

    hiddenDF <- as.data.frame(list())
    class(hiddenDF) <- "not.a.data.frame"
    expect_error(reorderColumns(hiddenDF, vars),
      label = "objects should be explicitly classed as data.frame"
    )
  })

  test_that("fails when you don't pass in any reorderings", {
    expect_error(reorderColumns(input, NULL),
      info = "You have to pass something in"
    )
    expect_error(reorderColumns(input, NA),
      info = "You have to pass something in"
    )
    expect_error(reorderColumns(input, list()),
      info = "You have to pass something in"
    )
  })

  test_that("fails when you don't define column positions by the column names", {
    expect_error(reorderColumns(input, c(1, 2)),
      info = "You need to define which columns go in which positions"
    )
    expect_error(reorderColumns(input, c(1, "Time" = 2)),
      info = "You need to define ALL columns to be moved"
    )
  })

  test_that("fails if any input names are duplicated", {
    expect_error(reorderColumns(input, c("In" = 1, "In" = 2)),
      info = "Which position do you actually want to move 'In' to?"
    )
  })

  test_that("fails if any positions are duplicated", {
    expect_error(reorderColumns(input, c("In" = 1, "Out" = 1)),
      info = "Which column do you actually want in position 1?"
    )
  })

  test_that("fails if you don't set numeric positions", {
    expect_error(reorderColumns(input, c("In" = NA)),
      info = "Column positions should be numeric"
    )
    expect_error(reorderColumns(input, c("In" = "One")),
      info = "Column positions should be numeric"
    )
    expect_error(reorderColumns(input, c("In" = "Out")),
      info = "Column positions should be numeric; this is not dplyr"
    )
  })

  test_that("fails if you try to reorder a column not in the DF", {
    expect_error(reorderColumns(input, c("NotAValidColumn" = 1)),
      info = "The input DF has no column named NotAValidColumn"
    )
  })

  test_that("fails if you try to set an invalid column position", {
    expect_error(reorderColumns(input, c("Time" = 0)),
      info = "R is 1-indexed, remember?"
    )
    expect_error(reorderColumns(input, c("Time" = -1)),
      info = "There are no negative column indices in R"
    )
    expect_error(reorderColumns(input, c("Time" = 4)),
      info = "The input DF has fewer than 4 columns"
    )
  })


  test_that("does nothing if the reordering is a no-op", {
    expect_equal(reorderColumns(input, c("Time" = 1, "In" = 2, "Out" = 3)),
      input,
      info = "no-op if you define all positions to be the same"
    )

    expect_equal(reorderColumns(input, c("Time" = 1)),
      input,
      info = "no-op if you define one position to be the same and
                 keep the rest in place"
    )

    expect_equal(reorderColumns(input, c("In" = 2)),
      input,
      info = "no-op if you define one position to be the same and
                 keep the rest in place"
    )
  })

  test_that("can move a single column", {
    expect_equal(
      reorderColumns(input, c("In" = 1)),
      data.frame(In = c(2, 3), Time = c(1, 2), Out = c(3, 4))
    )

    expect_equal(
      reorderColumns(input, c("In" = 3)),
      data.frame(Time = c(1, 2), Out = c(3, 4), In = c(2, 3))
    )
  })

  test_that("can move multiple columns but leave some in place", {
    expect_equal(reorderColumns(input, c("In" = 1, "Out" = 2)),
      data.frame(In = c(2, 3), Out = c(3, 4), Time = c(1, 2)),
      info = "Moved 'Time' to the end"
    )

    expect_equal(reorderColumns(input, c("Time" = 2, "In" = 1)),
      data.frame(In = c(2, 3), Time = c(1, 2), Out = c(3, 4)),
      info = "Swapped 'Time' and 'In' columns"
    )
  })

  test_that("can move all columns", {
    expect_equal(reorderColumns(input, c("Time" = 2, "In" = 3, "Out" = 1)),
      data.frame(Out = c(3, 4), Time = c(1, 2), In = c(2, 3)),
      info = "Swapped all columns"
    )
  })
})
