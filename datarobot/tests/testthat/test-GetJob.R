# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(jsonlite)
library(stubthat)
library(testthat)

jobJson <- fileToChar("responses/jobInfo.json")
completeJobResponse <- httr:::response(
  url = jobUrl,
  status_code = 303L,
  content = charToRaw(jobJson)
)

test_that("GetJob succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completeJobResponse)
  response <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetJob(fakeProject, fakeJobId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(response, "list")
  ExpectHasKeys(response, c("status", "url", "id", "projectId", "jobType", "isBlocked"))
  expect_type(response$status, "character")
  expect_type(response$projectId, "character")
  expect_type(response$id, "character")
  expect_type(response$url, "character")
  expect_type(response$jobType, "character")
  expect_type(response$isBlocked, "logical")

  tmp <- fromJSON(jobJson)
  tmp$status <- JobStatus$Queue
  jobJson <- toJSON(tmp)

  completeJobResponse <- httr:::response(
    url = jobUrl,
    status_code = 200L,
    content = charToRaw(jobJson)
  )
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completeJobResponse)
  response <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetJob(fakeProject, fakeJobId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(response, "list")
  ExpectHasKeys(response, c("status", "url", "id", "projectId", "jobType", "isBlocked"))
  expect_type(response$status, "character")
  expect_type(response$projectId, "character")
  expect_type(response$id, "character")
  expect_type(response$url, "character")
  expect_type(response$jobType, "character")
  expect_type(response$isBlocked, "logical")

  completeJobResponse <- httr:::response(
    url = jobUrl,
    status_code = 400L,
    content = charToRaw(jobJson)
  )
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completeJobResponse)
  expect_error(with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetJob(fakeProject, fakeJobId)
  ))
})
