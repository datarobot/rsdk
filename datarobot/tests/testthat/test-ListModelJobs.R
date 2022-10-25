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

modelJobUrl <- UrlJoin(projectUrl, "modelJobs", fakeJobId)
expectedKeys <- c(
  "status", "processes", "projectId", "modelId", "samplePct",
  "trainingRowCount", "modelType", "featurelistId",
  "modelCategory", "blueprintId", "modelJobId",
  "modelJobId", "isBlocked"
)
response <- list()

test_that("Query with no arguments is correct", {
  expectedRoute <- sprintf("projects/%s/modelJobs/", fakeProjectId)
  modelJobs <- with_mock(
    "datarobot::DataRobotGET" = function(RouteString, AddURL, ...) {
      expect_equal(RouteString, expectedRoute)
      query <- list(...)$query
      for (param in query) {
        expect_null(param)
      }
      response
    }, ListModelJobs(fakeProjectId)
  )
  expect_s3_class(modelJobs, "data.frame")
  ExpectHasKeys(modelJobs, expectedKeys)
})


test_that("Query with jobStatus argument is correct", {
  expectedRoute <- sprintf("projects/%s/modelJobs/", fakeProjectId)
  modelJobs <- with_mock(
    "datarobot::DataRobotGET" = function(RouteString, AddURL, ...) {
      expect_equal(RouteString, expectedRoute)
      expect_equal(list(...)$query$status, "queue")
      response
    }, ListModelJobs(fakeProjectId, status = "queue")
  )
  ExpectHasKeys(modelJobs, expectedKeys)
})
