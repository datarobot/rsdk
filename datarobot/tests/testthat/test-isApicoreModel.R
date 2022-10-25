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

test_that("isApicoreModel works for S3 objects", {
  lapply(
    datarobot:::apicoreModelClasses,
    function(generator) {
      x <- list()
      class(x) <- generator$classname
      expect_true(datarobot:::isApicoreModel(x))
    }
  )
})

test_that("isApicoreModel works for R6 objects", {
  lapply(
    datarobot:::apicoreModelClasses,
    function(generator) {
      TesterModelDetailsResponse <- R6::R6Class(
        inherit = generator,
        public = list(
          # remove all validation for testing
          initialize = NULL
        )
      )

      response <- TesterModelDetailsResponse$new()
      expect_true(datarobot:::isApicoreModel(response))
    }
  )
})

test_that("isApicoreModel returns false", {
  expect_false(datarobot:::isApicoreModel(NULL))
  expect_false(datarobot:::isApicoreModel(NA))
  expect_false(datarobot:::isApicoreModel(""))
  expect_false(datarobot:::isApicoreModel(1L))
  expect_false(datarobot:::isApicoreModel(c("I am not a model")))

  notApicoreModel <- c(id = "dummyId")
  expect_false(datarobot:::isApicoreModel(notApicoreModel))

  # Calling the legacy fn as.dataRobotModel doesn't make something an apicore model
  pretendApicoreModel <- as.dataRobotModel(notApicoreModel)
  expect_false(datarobot:::isApicoreModel(pretendApicoreModel))
})
