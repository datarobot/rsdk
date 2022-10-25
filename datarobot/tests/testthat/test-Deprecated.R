# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
test_that("Deprecated can deprecate a function", {
  ThisIsCorrect <- function(x, y) x + y

  ThisIsDeprecated <- function(x, y) {
    Deprecated("ThisIsDeprecated (use ThisIsCorrect instead)", "2.8", "2.10")
    ThisIsCorrect(x, y)
  }

  expect_warning(
    {
      r <- ThisIsDeprecated(2, 3)
    },
    paste(
      "ThisIsDeprecated (use ThisIsCorrect instead) has been deprecated",
      "in 2.8, will be removed in 2.10."
    ),
    fixed = TRUE
  )
  expect_equal(r, 5)
})
