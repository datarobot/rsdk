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
  expect_error(DeleteProject())
})

test_that("It can delete a project", {
  expect_message(with_mock(
    "datarobot:::DataRobotDELETE" = function(routeString,
                                             addUrl = TRUE,
                                             body = NULL,
                                             returnRawResponse = FALSE,
                                             ...) {
      routeForInspect <<- routeString
      ""
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    {
      DeleteProject(fakeProject$projectId)
    }
  ), "deleted")
  expect_equal(routeForInspect, paste0("projects/", fakeProjectId, "/"))
})
