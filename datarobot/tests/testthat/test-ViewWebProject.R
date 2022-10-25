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

test_that("Required parameters are present", {
  expect_error(ViewWebProject())
})

test_that("It gets the correct URL", {
  dataRobotUrl <- Sys.getenv("DATAROBOT_API_ENDPOINT")
  parsedUrl <- httr::parse_url(dataRobotUrl)
  urlString <- MakeUrl(parsedUrl, projectId = fakeProjectId)
  expect_equal(
    urlString,
    UrlJoin(
      paste0(parsedUrl$scheme, "://", parsedUrl$hostname),
      "projects", fakeProjectId, "eda"
    )
  )
})

test_that("It can view a web project when passing a project as parameter", {
  testReturn <- with_mock("datarobot:::DataRobotBrowse" = function(...) "NOOP", {
    suppressMessages(expect_message(
      ViewWebProject(fakeProject),
      "Opened URL"
    ))
  })
  expect_null(testReturn$returnValue)
})


test_that("It can view a web project when passing project ID as parameter", {
  testReturn <- with_mock("datarobot:::DataRobotBrowse" = function(...) "NOOP", {
    suppressMessages(expect_message(
      ViewWebProject(fakeProjectId),
      "Opened URL"
    ))
  })
  expect_null(testReturn$returnValue)
})
