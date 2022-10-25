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

allowedPizzaToppings <- c("cheese", "mushroom", "pineapple", "spinach")

describe("ValidateProject", {
  test_that("it returns a projectId from a project", {
    expect_equal(ValidateProject(fakeProject), fakeProjectId)
  })

  test_that("it returns a projectId from a projectId", {
    expect_equal(ValidateProject(fakeProjectId), fakeProjectId)
  })

  test_that("it errors if nothing is found", {
    expect_error(ValidateProject(list(nothing = "to see here")), "does not contain a valid project")
  })
})


describe("IsParameterIn", {
  test_that("It is TRUE if the parameter is in the set", {
    expect_true(IsParameterIn("cheese", allowedPizzaToppings))
    expect_true(IsParameterIn("mushroom", allowedPizzaToppings))
  })

  test_that("It is an error if the parameter is not in the set", {
    expect_equal(
      IsParameterIn("bicycle", allowedPizzaToppings),
      paste0(
        "Invalid ", sQuote("allowedPizzaToppings"), ". Must be in ",
        sQuote("cheese"), ", ", sQuote("mushroom"), ", ", sQuote("pineapple"),
        ", ", sQuote("spinach"), " but got ", sQuote("bicycle"), " instead."
      )
    )
    expect_equal(
      IsParameterIn("France", allowedPizzaToppings),
      paste0(
        "Invalid ", sQuote("allowedPizzaToppings"), ". Must be in ",
        sQuote("cheese"), ", ", sQuote("mushroom"), ", ", sQuote("pineapple"),
        ", ", sQuote("spinach"), " but got ", sQuote("France"), " instead."
      )
    )
  })

  test_that("A length > 1 vector is an error", {
    expect_equal(
      IsParameterIn(c("cheese", "mushroom"), allowedPizzaToppings),
      paste0(
        "Invalid ", sQuote("allowedPizzaToppings"), ". Must be in ",
        sQuote("cheese"), ", ", sQuote("mushroom"), ", ", sQuote("pineapple"),
        ", ", sQuote("spinach"), " but got ",
        sQuote("c(cheese, mushroom)"), " instead."
      )
    )
  })

  test_that("NULL is allowed if allowNULL = TRUE", {
    expect_true(IsParameterIn(NULL, allowedPizzaToppings))
    expect_true(IsParameterIn(NULL, allowedPizzaToppings, allowNULL = TRUE))
  })

  test_that("NULL is not allowed if allowNULL = FALSE", {
    expect_equal(
      IsParameterIn(NULL, allowedPizzaToppings, allowNULL = FALSE),
      paste0(
        "Invalid ", sQuote("allowedPizzaToppings"), ". Must be in ",
        sQuote("cheese"), ", ", sQuote("mushroom"), ", ", sQuote("pineapple"),
        ", ", sQuote("spinach"), " but got ", sQuote("NULL"), " instead."
      )
    )
  })

  test_that("It accepts `paramName`", {
    expect_equal(
      IsParameterIn("bicycle", allowedPizzaToppings, paramName = "foo"),
      paste0(
        "Invalid ", sQuote("foo"), ". Must be in ",
        sQuote("cheese"), ", ", sQuote("mushroom"), ", ", sQuote("pineapple"),
        ", ", sQuote("spinach"), " but got ", sQuote("bicycle"), " instead."
      )
    )
  })

  test_that("It has a sensical error message for unnamed `paramPossibilities`", {
    expect_equal(
      IsParameterIn("bicycle", c("cheese", "milk")),
      paste0(
        "Invalid ", sQuote("value"), ". Must be in ",
        sQuote("cheese"), ", ", sQuote("milk"),
        " but got ", sQuote("bicycle"), " instead."
      )
    )
  })
})



describe("ValidateParameterIn", {
  test_that("It is TRUE if the parameter is in the set", {
    expect_true(ValidateParameterIn("cheese", allowedPizzaToppings))
    expect_true(ValidateParameterIn("mushroom", allowedPizzaToppings))
  })

  test_that("It raises an error if the parameter is not in the set", {
    expect_error(ValidateParameterIn("bicycle", allowedPizzaToppings),
      paste0(
        "Invalid ", sQuote("allowedPizzaToppings"), ". Must be in ",
        sQuote("cheese"), ", ", sQuote("mushroom"), ", ", sQuote("pineapple"),
        ", ", sQuote("spinach"), " but got ", sQuote("bicycle"), " instead."
      ),
      fixed = TRUE
    )
  })

  test_that("NULL is allowed if allowNULL = TRUE", {
    expect_true(ValidateParameterIn(NULL, allowedPizzaToppings))
    expect_true(ValidateParameterIn(NULL, allowedPizzaToppings, allowNULL = TRUE))
  })

  test_that("NULL is not allowed if allowNULL = FALSE", {
    expect_error(
      ValidateParameterIn(NULL, allowedPizzaToppings, allowNULL = FALSE),
      paste0(
        "Invalid ", sQuote("allowedPizzaToppings"), ". Must be in ",
        sQuote("cheese"), ", ", sQuote("mushroom"), ", ", sQuote("pineapple"),
        ", ", sQuote("spinach"), " but got ", sQuote("NULL"), " instead."
      )
    )
  })

  test_that("It has a sensical error message for unnamed `paramPossibilities`", {
    expect_error(ValidateParameterIn("bicycle", c("cheese", "milk")),
      paste0(
        "Invalid ", sQuote("value"), ". Must be in ",
        sQuote("cheese"), ", ", sQuote("milk"),
        " but got ", sQuote("bicycle"), " instead."
      ),
      fixed = TRUE
    )
  })
})

describe("ValidateActuals", {
  test_that("it will not error if actuals is valid", {
    actuals <- list(
      data.frame(
        associationId = c("123", "abc"),
        actualValue = c("True", "False"),
        stringsAsFactors = FALSE
      ),
      data.frame(
        associationId = c("123", "abc"),
        actualValue = c("True", "False"),
        wasActedOn = c(TRUE, FALSE),
        stringsAsFactors = FALSE
      ),
      data.frame(
        associationId = c("123", "abc"),
        actualValue = c("True", "False"),
        wasActedOn = c(TRUE, FALSE),
        timestamp = c(
          as.POSIXct("2021-01-28 19:11:04"),
          as.POSIXct("2020-01-01 15:11:04")
        ),
        stringsAsFactors = FALSE
      )
    )

    lapply(actuals, function(a) {
      valid <- ValidateActuals(a)
      expect_true(valid)
    })
  })

  test_that("it will error if actuals is not a dataframe", {
    actuals <- list(associationId = c("123", "abc"), actualValue = c("True", "False"))
    expect_error(ValidateActuals(actuals), "Actuals is not a dataframe.")
  })

  test_that("it will error if required columns are missing", {
    actuals <- list(
      data.frame(
        associationId = c("123", "abc"),
        wasActedOn = c(TRUE, FALSE),
        stringsAsFactors = FALSE
      ),
      data.frame(
        actualValue = c("True", "False"),
        wasActedOn = c(TRUE, FALSE),
        stringsAsFactors = FALSE
      )
    )
    lapply(actuals, function(a) {
      expect_error(ValidateActuals(a), "Actuals does not contain.*")
    })
  })

  test_that("it will error if associatedId is too long", {
    longAssociatedId <- paste(replicate(129, "a"), collapse = "")
    actuals <- data.frame(
      associationId = c(longAssociatedId, "abc"),
      wasActedOn = c(TRUE, FALSE),
      stringsAsFactors = FALSE
    )
    expect_error(
      ValidateActuals(actuals),
      "Cannot have associationIds with length over max of 128 characters."
    )
  })

  test_that("it will error if optional key wasActedOn is not boolean", {
    actuals <- data.frame(
      associationId = c("123", "abc"),
      actualValue = c("True", "False"),
      wasActedOn = c("TRUE", "FALSE"),
      stringsAsFactors = FALSE
    )
    expect_error(ValidateActuals(actuals), "Optional key wasActedOn must be logical.")
  })

  test_that("it will error if optional key timestamp does not inherit from POSIXt", {
    actuals <- data.frame(
      associationId = c("123", "abc"),
      actualValue = c("True", "False"),
      timestamp = c("2021-01-28 19:11:04", "2020-01-01 15:11:04"),
      stringsAsFactors = FALSE
    )
    expect_error(ValidateActuals(actuals), "Optional key timestamp must inherit from POSIXt.")
  })
})
