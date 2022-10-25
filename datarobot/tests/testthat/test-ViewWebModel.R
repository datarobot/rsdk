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

test_that("With valid fakeModel", {
  testReturn <- with_mock("datarobot:::DataRobotBrowse" = function(...) "NOOP", {
    suppressMessages(
      list(
        returnValue = ViewWebModel(fakeModel),
        messageCheck = expect_message(
          ViewWebModel(fakeModel),
          "Opened URL"
        )
      )
    )
  })
  expect_null(testReturn$returnValue)
})


test_that("Without fakeModel", {
  expect_error(ViewWebModel())
})


test_that("With modelId only", {
  expect_error(ViewWebModel(fakeModel$modelId))
})

test_that("With projectId only", {
  expect_error(ViewWebModel(fakeModel$projectId))
})

test_that("With non-list object", {
  expect_error(ViewWebModel("Not a list object"))
})

damagedModel <- fakeModel
damagedModel$modelId <- NULL
test_that("With invalid fakeModel", {
  expect_error(ViewWebModel(damagedModel))
})

modifiedModel <- fakeModel
modifiedModel$modelType <- NULL
test_that("With fakeModel lacking modelType element", {
  testReturn <- with_mock("datarobot:::DataRobotBrowse" = function(...) "NOOP", {
    suppressMessages(
      list(
        returnValue = ViewWebModel(modifiedModel),
        messageCheck = expect_message(
          ViewWebModel(modifiedModel),
          "selected model"
        )
      )
    )
  })
  expect_equal(testReturn$returnValue, NULL)
})

test_that("It gets the correct URL", {
  dataRobotUrl <- Sys.getenv("DATAROBOT_API_ENDPOINT")
  parsedUrl <- httr::parse_url(dataRobotUrl)
  urlString <- MakeUrl(parsedUrl, model = fakeModel)
  expect_equal(
    urlString,
    UrlJoin(
      paste0(parsedUrl$scheme, "://", parsedUrl$hostname),
      "projects", fakeProjectId, "models", fakeModelId, "blueprint"
    )
  )
})
