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
  expect_error(GetProject())
})

expectedProjectNames <- c(
  "projectId", "projectName", "fileName", "stage",
  "autopilotMode", "created", "target", "metric",
  "partition", "advancedOptions",
  "positiveClass", "maxTrainPct", "maxTrainRows",
  "scaleoutMaxTrainPct", "scaleoutMaxTrainRows",
  "holdoutUnlocked", "targetType"
)


test_that("it can get a project from a projectId", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  project <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetProject(fakeProjectId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(project, "dataRobotProject")
  ExpectHasKeys(project, expectedProjectNames)
})

test_that("it can get a project from a project", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  project <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetProject(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(project, "dataRobotProject")
  ExpectHasKeys(project, expectedProjectNames)
})

test_that("GetProject returns message if projectId missing from project", {
  expect_error(GetProject(list(id = "lol")), "does not contain a valid project")
})

test_that("it can summarize a project", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  project <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetProject(fakeProjectId)
  )
  ExpectHasKeys(project, expectedProjectNames)
  expect_s3_class(project, "dataRobotProject")
  projectSummary <- summary(project)
  ExpectHasKeys(projectSummary, c(
    "projectName", "projectId", "created", "fileName", "target",
    "targetType", "metric"
  ))
  expect_type(projectSummary, "character")
})
