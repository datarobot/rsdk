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

fakePrimeFileId <- "primeFile-id"

projectUrl <- UrlJoin(fakeEndpoint, "projects", fakeProjectId)
primeFilestUrl <- UrlJoin(projectUrl, "primeFiles", fakePrimeFileId, "download")


primeFilesJson <- fileToChar("responses/primeCode.json")
completedPrimeFilesResponse <- httr:::response(
  url = primeFilestUrl,
  status_code = 200L,
  content = charToRaw(primeFilesJson)
)

test_that("DownloadPrimeCode succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedPrimeFilesResponse)
  primeCode <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DownloadPrimeCode(fakeProject, fakePrimeFileId, fakeFilePath)
  )
  expect_equal(getStub$calledTimes(), 1)
})
