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


test_that("ApiResponse has error property with NULL value on 200 status code", {
  response <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = c()
  )
  apir <- datarobot.apicore::ApiResponse$new(
    content = fakeProject,  # Deserialized Object is first param
    response = response
  )
  # With a successful API call we should expect a top-level error property to exist and to be NULL
  expect_true("error" %in% names(apir))
  expect_null(apir$error)
})


test_that("ApiResponse has error property with NULL value on 202 status code", {
  response <- httr:::response(
    url = projectUrl,
    status_code = 202L
  )
  apir <- datarobot.apicore::ApiResponse$new(
    content = fakeProject,  # Deserialized Object is first param
    response = response
  )
  expect_true("error" %in% names(apir))
  expect_null(apir$error)
})


test_that("ApiResponse has error property with NULL value on 204 status code", {
  response <- httr:::response(
    url = projectUrl,
    status_code = 204L
  )
  apir <- datarobot.apicore::ApiResponse$new(
    content = fakeProject,  # Deserialized Object is first param
    response = response
  )
  expect_true("error" %in% names(apir))
  expect_null(apir$error)
})


test_that("ApiResponse has error property with correct value on 400 status code", {
  errMessage <- "API client error with 400 response status code. See $response for more detail."
  response <- httr:::response(
    url = projectUrl,
    status_code = 400L,
    content =  list(message = errMessage)
  )
  apir <- datarobot.apicore::ApiResponse$new(
    content = errMessage,
    response = response
  )
  expect_true("error" %in% names(apir))
  expect_false(is.null(apir$error))
  expect_equal(apir$error, paste("400", errMessage))
})


test_that("ApiResponse has error property with correct value on 404 status code", {
  errMessage <- "API client error with 404 response status code. See $response for more detail."
  response <- httr:::response(
    url = projectUrl,
    status_code = 404L,
    content =  list(message = errMessage)
  )
  apir <- datarobot.apicore::ApiResponse$new(
    content = errMessage,
    response = response
  )
  expect_true("error" %in% names(apir))
  expect_false(is.null(apir$error))
  expect_equal(apir$error, paste("404", errMessage))
})


test_that("ApiResponse has error property with correct value on 422 status code", {
  errMessage <- "API client error with 422 response status code. See $response for more detail."
  response <- httr:::response(
    url = projectUrl,
    status_code = 422L,
    content =  list(message = errMessage)
  )
  apir <- datarobot.apicore::ApiResponse$new(
    content = errMessage,
    response = response
  )
  expect_true("error" %in% names(apir))
  expect_false(is.null(apir$error))
  expect_equal(apir$error, paste("422", errMessage))
})


test_that("ApiResponse has error property with correct value on 500 status code", {
  errMessage <- "API server error with 500 response status code. See $response for more detail."
  response <- httr:::response(
    url = projectUrl,
    status_code = 500L,
    content =  list(message = errMessage)
  )
  apir <- datarobot.apicore::ApiResponse$new(
    content = errMessage,
    response = response
  )
  expect_true("error" %in% names(apir))
  expect_false(is.null(apir$error))
  expect_equal(apir$error, paste("500", errMessage))
})
