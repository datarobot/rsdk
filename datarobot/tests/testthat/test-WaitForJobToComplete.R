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
  expect_error(WaitForJobToComplete())
  expect_error(WaitForJobToComplete(fakeProjectId))
  expect_error(WaitForJobToComplete(fakeJobId = fakeJobId))
})


test_that("WaitForJobToComplete raises appropriate exception when job status indicates failure", {
  jobFailureJson <- '{"status": "error", "message": "some job failure message"}'
  testReturn <- with_mock(
    "datarobot::DataRobotGET" = function(url, ...) {
      failureResponse <- httr:::response(
        url = url,
        status_code = 200,
        content = charToRaw(jobFailureJson)
      )
      failureResponse
    },
    expect_error(
      WaitForJobToComplete(fakeProjectId, fakeJobId),
      "PendingJobFailed"
    )
  )
})
