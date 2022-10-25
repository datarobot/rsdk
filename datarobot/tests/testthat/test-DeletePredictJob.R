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


test_that("Required parameters are present", {
  expect_error(DeletePredictJob())
  expect_error(DeletePredictJob(fakeProject))
  expect_error(DeletePredictJob(jobId = fakeJobId))
})

projectUrl <- UrlJoin(fakeEndpoint, "projects", fakeProjectId)
deletePredictJobUrl <- UrlJoin(projectUrl, "predictJobs", fakeJobId)
completedDeleteResponse <- httr:::response(
  url = deletePredictJobUrl,
  status_code = 200L,
  content = raw(0)
)
test_that("DeletePredictJob succeeds", {
  deleteStub <- stub(httr::DELETE)
  deleteStub$onCall(1)$returns(completedDeleteResponse)
  expect_message(with_mock(
    "httr::DELETE" = deleteStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DeletePredictJob(fakeProjectId, fakeJobId)
  ), "deleted")
})
