# Copyright 2021-2022 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(testthat)
library(lubridate)

describe(".toS3", {
  it("should fail if input R6 object is locked", {
    LockedR6Object <- R6::R6Class(
      "LockedR6Object",
      lock_objects = TRUE # which is default anyway
    )
    lockedObject <- LockedR6Object$new()

    expect_error(.toS3(lockedObject))
  })
})

describe(".setMixedProperty", {
  # R6 class Generators
  TestObjectOne <- R6::R6Class("TestObjectOne")
  TestObjectTwo <- R6::R6Class("TestObjectTwo")
  # R6 objects
  tOne <- TestObjectOne$new()
  tTwo <- TestObjectTwo$new()

  it("should fail if typelist is empty", {
    expect_error(.setMixedProperty(NULL, NULL))
    expect_error(.setMixedProperty(NA, NULL))
    expect_error(.setMixedProperty(c(), NULL))
    expect_error(.setMixedProperty(list(), NULL))
  })

  it("should fail if typelist is not a character vector or list", {
    expect_error(.setMixedProperty(c(1L), tOne))
    expect_error(.setMixedProperty(c(1L, "2L"), tOne))
    expect_error(.setMixedProperty(TestObjectOne, tOne))
    expect_error(.setMixedProperty(c(TestObjectOne), tOne))
    expect_error(.setMixedProperty(c(logical, "TestObjectOne"), tOne))
    expect_error(.setMixedProperty(c("logical", TestObjectOne), tOne))
  })

  it("should passthrough NULL", {
    expect_null(.setMixedProperty("boolean", NULL))
    expect_null(.setMixedProperty("TestObjectOne", NULL))
    expect_null(.setMixedProperty(c("boolean"), NULL))
    expect_null(.setMixedProperty(c("TestObjectOne"), NULL))
  })

  it("should work if typelist is a list and not a vector", {
    .setMixedProperty(list("logical", "character"), TRUE)
  })
})

describe(".setComplexProperty", {
  # R6 class Generators
  TestObjectOne <- R6::R6Class("TestObjectOne")
  TestObjectTwo <- R6::R6Class("TestObjectTwo")
  # R6 objects
  tOne <- TestObjectOne$new()
  tTwo <- TestObjectTwo$new()

  it("should fail if typelist contains any non R6 generators", {
    expect_error(.setComplexProperty(c(), ""))
    expect_error(.setComplexProperty(c(1L), "")) # integer
    expect_error(.setComplexProperty(c("TestObjectOne"), "")) # name of a class, not the generator
    expect_error(.setComplexProperty(c(TestObjectOne, 1L), ""))
    expect_error(.setComplexProperty(c(1L, TestObjectOne), ""))
  })

  it("should passthrough NULL", {
    expect_null(.setComplexProperty(c(TestObjectOne), NULL))
    expect_null(.setComplexProperty(c(TestObjectOne, TestObjectTwo), NULL))
  })

  it("should passthrough R6 object if it matches an expected type", {
    expect_identical(
      .setComplexProperty(c(TestObjectOne), tOne),
      expected = tOne
    )
    expect_identical(
      .setComplexProperty(c(TestObjectTwo, TestObjectOne), tOne),
      expected = tOne
    )
    expect_identical(
      .setComplexProperty(c(TestObjectOne, TestObjectTwo), tOne),
      expected = tOne
    )
  })

  it("should fail if R6 object doesn't match an expected type", {
    TestObjectThree <- R6::R6Class("TestObjectThree")
    tThree <- TestObjectThree$new()

    expect_error(.setComplexProperty(c(TestObjectOne), tThree))
  })

  it("should succeed for an R6 object with class hierarchy", {
    TestObjectFour <- R6::R6Class("TestObjectFour", inherit = TestObjectOne)
    tFour <- TestObjectFour$new()

    expect_identical(
      .setComplexProperty(c(TestObjectFour), tFour),
      expected = tFour
    )
    expect_identical(
      .setComplexProperty(c(TestObjectOne), tFour),
      expected = tFour
    )
    expect_identical(
      .setComplexProperty(c(TestObjectOne, TestObjectFour), tFour),
      expected = tFour
    )
    expect_identical(
      .setComplexProperty(c(TestObjectFour, TestObjectOne), tFour),
      expected = tFour
    )
  })

  it("should succeed if typelist is a list and not a vector", {
    expect_identical(
      .setComplexProperty(list(TestObjectOne), tOne),
      expected = tOne
    )
  })

  it("should fail if a non-R6 object cannot be newed into an R6 object", {
    TestObjectWithField <- R6::R6Class(
      "TestObjectWithField",
      public = list(
        initialize = function(x) {
          stopifnot(x == 1L)
        }
      )
    )

    expect_error(.setComplexProperty(c(TestObjectWithField), tOne))
    x <- 0L # not 1L!
    expect_error(.setComplexProperty(c(TestObjectWithField), x))
  })
})

describe(".setPrimitiveProperty", {
  it("should return NULL", {
    expect_null(.setPrimitiveProperty(c("logical"), NULL))
    expect_null(.setPrimitiveProperty(c("character"), NULL))
    expect_null(.setPrimitiveProperty(c("numeric"), NULL))
    expect_null(.setPrimitiveProperty(c("array"), NULL))
  })

  it("should return boolean value", {
    expect_equal(.setPrimitiveProperty(c("logical"), TRUE), TRUE)
    expect_equal(.setPrimitiveProperty(c("logical"), FALSE), FALSE)
  })

  it("should return character", {
    expect_equal(.setPrimitiveProperty(c("character"), "some text"), "some text")
  })

  it("should return numeric", {
    expect_equal(.setPrimitiveProperty(c("numeric"), 42), 42)
    expect_equal(.setPrimitiveProperty(c("numeric"), 42L), 42L)
    expect_equal(.setPrimitiveProperty(c("numeric"), 42.42), 42.42)
  })

  it("should return array", {
    expect_equal(.setPrimitiveProperty(c("array"), array(c(1, 0))), array(c(1, 0)))
  })

  it("should return first valid type", {
    expect_equal(.setPrimitiveProperty(c("character", "logical"), TRUE), TRUE)
    expect_equal(.setPrimitiveProperty(c("logical", "numeric"), 42), 42)
    expect_equal(.setPrimitiveProperty(c("logical", "numeric", "array"), TRUE), TRUE)
  })

  it("should fail", {
    expect_error(.setPrimitiveProperty(c("character"), FALSE))
    expect_error(.setPrimitiveProperty(c("logical"), 42))
  })

  it("should no longer work with 'boolean' per DSX-2437", {
    expect_error(.setPrimitiveProperty(c("boolean"), TRUE))
    expect_error(.setPrimitiveProperty(c("boolean"), FALSE))
    expect_error(.setPrimitiveProperty(c("boolean", "numeric"), FALSE))
    expect_error(.setPrimitiveProperty(c("character", "boolean"), FALSE))
    expect_error(.setPrimitiveProperty(c("boolean", "logical"), FALSE))
    # this next test is fine *because* we iterate on the typeList sequentially
    expect_equal(.setPrimitiveProperty(c("logical", "boolean"), FALSE), FALSE)
  })
})

test_that("ParseRFC3339Timestamp handles missing values", {
  expect_true(is.na(ParseRFC3339Timestamp(NA)))
})

test_that("ParseRFC3339Timestamp handles NULL values", {
  result <- ParseRFC3339Timestamp(NULL)
  expected <- list()
  class(expected) <- c("POSIXct", "POSIXt")
  expect_identical(result, expected)
})

test_that("ParseRFC3339Timestamp returns a valid POSIXt", {
  input <- "2021-02-19T00:05:56.326754Z"
  output <- ParseRFC3339Timestamp(input)
  expect_s3_class(output, "POSIXt")

  # ct
  ct <- as.POSIXct(output)
  expect_equal(as.numeric(output), 1613693156)
  expect_equal(lubridate::tz(output), "UTC")
})
