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

projectStatusUrl <- UrlJoin(projectUrl, "status")

test_that("Required parameters are present", {
  expect_error(GetProjectStatus())
})

test_that("it can get project status", {
  getStub <- stub(httr::GET)
  getProjectStatusJson <- fileToChar("responses/GetProjectStatus.json")
  projectStatusResponse <- httr:::response(
    url = projectStatusUrl,
    status_code = 200L,
    content = charToRaw(getProjectStatusJson)
  )
  getStub$onCall(1)$returns(projectStatusResponse)
  projectStatus <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetProjectStatus(fakeProjectId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(projectStatus, "list")
  ExpectHasKeys(projectStatus, c("autopilotDone", "stageDescription", "stage"))
})

test_that("Use project list", {
  getStub <- stub(httr::GET)
  getProjectStatusJson <- fileToChar("responses/GetProjectStatus.json")
  projectStatusResponse <- httr:::response(
    url = projectStatusUrl,
    status_code = 200L,
    content = charToRaw(getProjectStatusJson)
  )
  getStub$onCall(1)$returns(projectStatusResponse)
  projectStatus <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetProjectStatus(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(projectStatus, "list")
  ExpectHasKeys(projectStatus, c("autopilotDone", "stageDescription", "stage"))
})
