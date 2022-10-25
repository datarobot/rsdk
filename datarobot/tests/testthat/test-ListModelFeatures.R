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

test_that("Required parameters are present", {
  expect_error(ListModelFeatures())
})

test_that("With modelId only", {
  expect_error(ListModelFeatures(fakeModel$modelId))
})

test_that("With projectId only", {
  expect_error(ListModelFeatures(fakeModel$projectId))
})

test_that("With non-list Model", {
  expect_error(ListModelFeatures("Not a model"))
})

test_that("Use fakeModel", {
  response <- list(featureNames = c(
    "age", "black", "chas", "crim", "dis",
    "indus", "lstat", "medv", "nox", "ptratio",
    "rad", "rm", "tax", "zn"
  ))

  testReturn <- with_mock(
    "datarobot::DataRobotGET" = function(RouteString, AddURL, ...) response,
    ListModelFeatures(fakeModel)
  )
  expect_equal(testReturn, response$featureNames)
})
