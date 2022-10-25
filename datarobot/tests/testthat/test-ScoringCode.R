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

scoringCodetUrl <- UrlJoin(projectUrl, "models", fakeModelId, "scoringCode")
numLength <- 100000
cont <- as.raw(sample(1:3, numLength, replace = TRUE))
rawLength <- length(cont)
completedFilesResponse <- httr:::response(
  url = scoringCodetUrl,
  status_code = 200L,
  content = cont
)
test_that("DownloadScoringCode succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedFilesResponse)
  primeCode <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DownloadScoringCode(fakeProject, fakeModelId, fakeFilePath)
  )
  expect_equal(getStub$calledTimes(), 1)
})
