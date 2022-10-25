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

frozenModelUrl <- UrlJoin(projectUrl, "frozenModels", fakeModelId)
frozenModelJson <- fileToChar("responses/frozenModel.json")
completedFrozenModelResponse <- httr:::response(
  url = frozenModelUrl,
  status_code = 200L,
  content = charToRaw(frozenModelJson)
)

test_that("GetFrozenModelFromJobId succeeds", {
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
      url = frozenModelUrl,
      status_code = 200L,
      content = charToRaw(jsonlite::toJSON(jobDataInprogress))
    )

  jobCompleteResponse <-
    httr:::response(
      url = frozenModelUrl,
      status_code = 303L,
      headers = list(location = frozenModelUrl),
      content = charToRaw(jsonlite::toJSON(jobDataComplete))
    )

  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(jobInprogressResponse)
  getStub$onCall(2)$returns(jobCompleteResponse)
  getStub$onCall(3)$returns(jobCompleteResponse)
  getStub$onCall(4)$returns(completedFrozenModelResponse)
  frozenModel <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::GetProject" = function(project) {
      p <- fromJSON(fileToChar("responses/GetProject.json"))
      datarobot:::as.dataRobotProject(p)
    }, GetFrozenModelFromJobId(fakeProject, fakeJobId, maxWait = 1)
  )
  expect_equal(getStub$calledTimes(), 4)
  expect_s3_class(frozenModel, "dataRobotFrozenModel")
  expect_true(all(c("samplePct", "parentModelId", "modelId", "trainingRowCount")
  %in% names(frozenModel)))
})
