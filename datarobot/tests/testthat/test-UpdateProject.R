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
  expect_error(UpdateProject())
})

test_that("No update without information", {
  expect_error(UpdateProject(fakeProject), "No update data is provided")
})

test_that("It can update a project", {
  newName <- "TestOfProjectUpdate"
  patchStub <- stub(httr::PATCH)
  patchStub$onCall(1)$returns(httr:::response(
    url = projectUrl,
    status_code = 202L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))
  expect_message(with_mock(
    "httr::PATCH" = patchStub$f,
    "datarobot:::DataRobotPATCH" = function(routeString,
                                            addUrl = TRUE,
                                            body = NULL,
                                            returnRawResponse = FALSE, ...) {
      bodyForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::PATCH, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    {
      UpdateProject(fakeProject$projectId,
        newProjectName = newName,
        holdoutUnlocked = TRUE,
        workerCount = 8
      )
    }
  ), "updated")
  expect_equal(bodyForInspect$workerCount, 8)
  expect_true(bodyForInspect$holdoutUnlocked)
  expect_equal(as.character(bodyForInspect$projectName), newName)
})


test_that("It can update a project to set workers to max", {
  newName <- "TestOfProjectUpdate"
  patchStub <- stub(httr::PATCH)
  patchStub$onCall(1)$returns(httr:::response(
    url = projectUrl,
    status_code = 202L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))
  expect_message(with_mock(
    "httr::PATCH" = patchStub$f,
    "datarobot:::DataRobotPATCH" = function(routeString,
                                            addUrl = TRUE,
                                            body = NULL,
                                            returnRawResponse = FALSE, ...) {
      bodyForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::PATCH, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    {
      UpdateProject(fakeProject$projectId, workerCount = "max")
    }
  ), "updated")
  expect_equal(bodyForInspect$workerCount, -1)
})
