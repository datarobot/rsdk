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


blendModelUrl <- UrlJoin(projectUrl, "blenderModels", fakeModelId)
blendModelJson <- fileToChar("responses/blendModel.json")
completedBlendModelResponse <- httr:::response(
  url = blendModelUrl,
  status_code = 200L,
  content = charToRaw(blendModelJson)
)

test_that("GetBlenderModelFromJobId succeeds", {
  jobDataInprogress <- list(
    status = JobStatus$InProgress,
    url = "https://host_name.com/projects/p-id/jobs/1/",
    id = fakeJobId,
    jobType = JobType$Model,
    projectId = fakeProjectId
  )

  jobDataComplete <- jobDataInprogress
  jobDataComplete$status <- JobStatus$Completed

  jobInprogressResponse <-
    httr:::response(
      url = blendModelUrl,
      status_code = 200L,
      content = charToRaw(jsonlite::toJSON(jobDataInprogress))
    )

  jobCompleteResponse <-
    httr:::response(
      url = blendModelUrl,
      status_code = 303L,
      headers = list(location = blendModelUrl),
      content = charToRaw(jsonlite::toJSON(jobDataComplete))
    )

  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(jobInprogressResponse)
  getStub$onCall(2)$returns(jobCompleteResponse)
  getStub$onCall(3)$returns(jobCompleteResponse)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(4)$returns(projectResponse)
  getStub$onCall(5)$returns(completedBlendModelResponse)
  blendModel <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetBlenderModelFromJobId(fakeProject, fakeJobId, maxWait = 1)
  )
  expect_equal(getStub$calledTimes(), 5)
  expect_s3_class(blendModel, "dataRobotBlenderModel")
  expected <- names(as.dataRobotBlenderModel(fromJSON(blendModelJson)))
  actual <- names(blendModel)
  expect_true(all(expected %in% actual))
})
