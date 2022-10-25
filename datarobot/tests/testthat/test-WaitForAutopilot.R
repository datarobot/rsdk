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


test_that("WaitForAutopilot makes the right number of calls to GetProjectStatus", {
  callCount <- 0
  notDoneStatus <- list(autopilotDone = FALSE)
  isDoneStatus <- list(autopilotDone = TRUE)
  returnValues <- list(notDoneStatus, notDoneStatus, isDoneStatus)
  MockGetProjectStatus <- function(project) {
    callCount <<- callCount + 1
    returnValues[[callCount]]
  }
  suppressMessages(with_mock(
    "datarobot::GetProjectStatus" = MockGetProjectStatus,
    WaitForAutopilot(project, verbosity = 0)
  ))
  expect_equal(callCount, 3)
})

test_that("WaitForAutopilot errors if we don't finish in time", {
  callCount <- 0
  notDoneStatus <- list(autopilotDone = FALSE)
  MockGetProjectStatus <- function(project) notDoneStatus
  suppressMessages(with_mock(
    "datarobot::GetProjectStatus" = MockGetProjectStatus,
    expect_error(WaitForAutopilot(project, timeout = .2, verbosity = 0))
  ))
})
