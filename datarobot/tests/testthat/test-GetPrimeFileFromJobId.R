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


fakeFileId <- "file-id"
primeFileUrl <- UrlJoin(projectUrl, "primeFiles", fakeFileId)
primeFileJson <- fileToChar("responses/primeFile.json")
completedPrimeFileResponse <- httr:::response(
  url = primeFileUrl,
  status_code = 200L,
  content = charToRaw(primeFileJson)
)

test_that("GetPrimeFileFromJobId succeeds", {
  jobDataInprogress <- list(
    status = JobStatus$InProgress,
    url = "https://host_name.com/projects/p-id/jobs/1/",
    id = fakeJobId,
    jobType = JobType$PrimeDownloadValidation,
    projectId = fakeProjectId
  )
  jobDataComplete <- jobDataInprogress
  jobDataComplete$status <- JobStatus$Completed
  jobInprogressResponse <-
    httr:::response(
      url = primeFileUrl,
      status_code = 200L,
      content = charToRaw(jsonlite::toJSON(jobDataInprogress))
    )
  jobCompleteResponse <-
    httr:::response(
      url = primeFileUrl,
      status_code = 303L,
      headers = list(location = primeFileUrl),
      content = charToRaw(jsonlite::toJSON(jobDataComplete))
    )
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(jobInprogressResponse)
  getStub$onCall(2)$returns(jobCompleteResponse)
  getStub$onCall(3)$returns(jobCompleteResponse)
  getStub$onCall(4)$returns(completedPrimeFileResponse)


  primeFile <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPrimeFileFromJobId(fakeProject, fakeJobId, maxWait = 1)
  )
  expect_equal(getStub$calledTimes(), 4)
  expect_type(primeFile, "list")
  ExpectHasKeys(primeFile, c(
    "language", "parentModelId", "id", "projectId", "isValid",
    "rulesetId", "modelId"
  ))
})
