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

expectedProjectNames <- c(
  "projectId", "projectName", "fileName", "stage",
  "autopilotMode", "created", "target", "metric",
  "partition", "advancedOptions",
  "positiveClass", "maxTrainPct", "maxTrainRows",
  "scaleoutMaxTrainPct", "scaleoutMaxTrainRows",
  "holdoutUnlocked", "targetType"
)

getProjectJson <- fileToChar("responses/ListProjects.json")

test_that("it can list projects", {
  getStub <- stub(httr::GET)
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  projects <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListProjects()
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(projects, "projectSummaryList")
  ExpectHasKeys(projects, expectedProjectNames)
})

test_that("it returns an empty list when there are no projects", {
  project <- with_mock(
    "datarobot:::DataRobotGET" = function(...) list(),
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListProjects()
  )
  expect_s3_class(project, "projectSummaryList")
  expect_equal(length(project$projectId), 0)
  ExpectHasKeys(project, expectedProjectNames)
})

test_that("filter parameter must be a list", {
  expect_error(ListProjects(filter = "not-a-list"), "must be a list")
})

test_that("it can filter projects by name", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  projects <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListProjects(filter = list("projectName" = "TimeSeries"))
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(projects, "projectSummaryList")
})

test_that("extraneous filters are silently ignored", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  projects <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListProjects(filter = list("tag" = "my-project"))
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(projects, "projectSummaryList")
})

test_that("summary for 'projectSummaryList' object", {
  getStub <- stub(httr::GET)
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  projects <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListProjects()
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(projects, "projectSummaryList")
  ExpectHasKeys(projects, expectedProjectNames)
  summaryList <- summary(projects)
  expect_type(summaryList, "list")
  ExpectHasKeys(summaryList, c("generalSummary", "detailedSummary"))
  expect_type(summaryList$generalSummary, "character")
  expect_equal(length(summaryList$generalSummary), 1)
  expect_s3_class(summaryList$detailedSummary, "data.frame")
  ExpectHasKeys(
    summaryList$detailedSummary,
    setdiff(expectedProjectNames, c(
      "partition", "advancedOptions",
      "maxTrainRows", "scaleoutMaxTrainPct",
      "scaleoutMaxTrainRows"
    ))
  )
})

test_that("summary for 'projectSummaryList' object - nList", {
  getStub <- stub(httr::GET)
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  projects <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListProjects()
  )
  summaryList <- summary(projects)
  # Default case is 6
  expect_equal(nrow(summary(projects)$detailedSummary), 6)
  # Can restrict
  expect_equal(nrow(summary(projects, nList = 1)$detailedSummary), 1)
  # More than number of projects -> number of projects
  expect_equal(nrow(summary(projects, nList = 20)$detailedSummary), length(projects$projectId))
})

test_that("as.data.frame for project list", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/ListProjects.json")
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  projects <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListProjects()
  )
  projectsDf <- as.data.frame(projects)
  expect_s3_class(projectsDf, "data.frame")
  ExpectHasKeys(projectsDf, c(
    "projectName", "projectId", "created", "fileName", "target",
    "targetType", "positiveClass", "metric", "autopilotMode", "stage",
    "maxTrainPct", "holdoutUnlocked"
  ))
  expect_equal(length(projects$projectId), nrow(projectsDf))
})

test_that("as.data.frame for project list with full summary", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/ListProjects.json")
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  projects <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListProjects()
  )
  projectsDf <- as.data.frame(projects, simple = FALSE)
  expect_s3_class(projectsDf, "data.frame")
  ExpectHasKeys(projectsDf, c(
    "projectName", "projectId", "created", "fileName", "target",
    "targetType", "positiveClass", "metric", "autopilotMode", "stage",
    "maxTrainPct", "holdoutUnlocked", "datetimeCol", "cvMethod",
    "validationPct", "reps", "cvHoldoutLevel", "holdoutLevel",
    "userPartitionCol", "validationType", "trainingLevel",
    "partitionKeyCols", "holdoutPct", "validationLevel",
    "datetimePartitionColumn", "scaleoutModelingMode",
    "responseCap", "downsampledMinorityRows", "downsampledMajorityRows",
    "blueprintThreshold", "seed", "weights", "smartDownsampled",
    "majorityDownsamplingRate"
  ))
  expect_equal(length(projects$projectId), nrow(projectsDf))
})

test_that("as.data.frame for project list with custom row names", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/ListProjects.json")
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  projects <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListProjects()
  )
  projectsDf <- as.data.frame(projects)
  expect_s3_class(projectsDf, "data.frame")
  ExpectHasKeys(projectsDf, c(
    "projectName", "projectId", "created", "fileName", "target",
    "targetType", "positiveClass", "metric", "autopilotMode", "stage",
    "maxTrainPct", "holdoutUnlocked"
  ))
  expect_equal(rownames(projectsDf), as.character(seq(7)))
  expect_equal(length(projects$projectId), nrow(projectsDf))
  newRowNames <- paste0("row_", as.character(seq(7)))
  projectsDf <- as.data.frame(projects, row.names = newRowNames)
  expect_s3_class(projectsDf, "data.frame")
  ExpectHasKeys(projectsDf, c(
    "projectName", "projectId", "created", "fileName", "target",
    "targetType", "positiveClass", "metric", "autopilotMode", "stage",
    "maxTrainPct", "holdoutUnlocked"
  ))
  expect_equal(rownames(projectsDf), newRowNames)
  expect_equal(length(projects$projectId), nrow(projectsDf))
})
