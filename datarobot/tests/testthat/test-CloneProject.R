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

describe("CloneProject", {
  withCloneProjectMocks <- function(f,
                                    expectedOldProjectId = NULL,
                                    expectedNewProjectName = NULL) {
    cloneProjectUrl <- UrlJoin(fakeEndpoint, "projectClones")
    postResponse <- httr:::response(
      url = cloneProjectUrl,
      status_code = 202L,
      headers = list(location = statusUrl),
      content = raw(0)
    )

    # mock(DataRobotPOST) so that we can inspect the body parameters
    # sent to the DR API, but return the postResponse anyway since we
    # need to retrieve the location of the
    # mock(ProjectFromJobResponse) so that we get an actual project back
    # in the end
    result <- with_mock(
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      "datarobot:::DataRobotPOST" = function(body = NULL, ...) {
        postBodyForInspect <<- body
        postResponse
      },
      "datarobot:::ProjectFromJobResponse" = function(...) fakeProject,
      f
    )

    expect_equal(postBodyForInspect$projectId, expectedOldProjectId)
    expect_equal(postBodyForInspect$projectName, expectedNewProjectName)
    result
  }

  test_that("Project name is POSTed to API when arg given", {
    cloneProjectName <- "customClonedProject"

    result <- withCloneProjectMocks(
      CloneProject(fakeProject, cloneProjectName),
      expectedOldProjectId = fakeProject$projectId,
      expectedNewProjectName = cloneProjectName
    )
  })

  test_that("No project name passed to API to when arg is omitted", {
    return <- withCloneProjectMocks(
      CloneProject(fakeProject),
      expectedOldProjectId = fakeProject$projectId,
      expectedNewProjectName = NULL
    )
  })

  test_that("No project name is not passed to API when arg is NULL", {
    return <- withCloneProjectMocks(
      CloneProject(fakeProject, NULL),
      expectedOldProjectId = fakeProject$projectId,
      expectedNewProjectName = NULL
    )
  })

  test_that("Return value is a `dataRobotProject` class", {
    return <- withCloneProjectMocks(
      CloneProject(fakeProject),
      expectedOldProjectId = fakeProject$projectId,
      expectedNewProjectName = NULL
    )

    expect_s3_class(return, "dataRobotProject")
  })
})
