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

listJobsJson <- fileToChar("responses/jobs.json")
listErroredJobsJson <- fileToChar("responses/jobs_errored.json")
datarobot:::SaveConnectionEnvironmentVars("fake_endpoint", "fake_token")

test_that("We can list and delete jobs", {
  jobs <- with_mock(
    "httr::GET" = function(url, ...) {
      expect_equal(url, datarobot:::BuildPath(datarobot:::UrlJoin(
        "projects",
        fakeProjectId,
        "jobs"
      ))$fullPath)
      httr:::response(
        url = url, status_code = 200L,
        content = charToRaw(listJobsJson)
      )
    },
    ListJobs(fakeProjectId)
  )
  expect_equal(jobs, jsonlite::fromJSON(listJobsJson, simplifyDataFrame = FALSE)$jobs)
  ExpectHasKeys(jobs[[1]], c("status", "url", "id", "projectId", "jobType", "isBlocked"))
  jobToDelete <- jobs[[1]]
  httrDeleteCallCount <- 0
  with_mock(
    "httr::DELETE" = function(url, ...) {
      httrDeleteCallCount <<- httrDeleteCallCount + 1
      expect_equal(url, jobToDelete$url)
      httr:::response(url = url, status_code = 200L, content = raw(0))
    },
    DeleteJob(jobToDelete)
  )
  expect_equal(httrDeleteCallCount, 1)
})

test_that("Get errored jobs uses the right query parameter", {
  jobs <- with_mock(
    "httr::GET" = function(url, query, ...) {
      expect_equal(url, datarobot:::BuildPath(datarobot:::UrlJoin(
        "projects",
        fakeProjectId,
        "jobs"
      ))$fullPath)
      expect_equal(query, list(status = JobStatus$Error))
      httr:::response(
        url = url, status_code = 200L,
        content = charToRaw(listErroredJobsJson)
      )
    },
    ListJobs(fakeProjectId, status = "error")
  )
})

test_that("Deleting a non-job is an error", {
  jobToDelete <- list(not = 1, a = 2, job = 3)
  httrDeleteCallCount <- 0
  with_mock(
    "httr::DELETE" = function(url, ...) {
      httrDeleteCallCount <<- httrDeleteCallCount + 1
      expect_equal(url, jobToDelete$url)
      httr:::response(url = url, status_code = 200L, content = raw(0))
    },
    expect_error(DeleteJob(jobToDelete))
  )
  expect_equal(httrDeleteCallCount, 0)
})
