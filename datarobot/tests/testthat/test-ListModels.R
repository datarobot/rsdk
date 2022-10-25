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


getProjectJson <- fileToChar("responses/GetProject.json")
projectResponse <- httr:::response(
  url = projectUrl,
  status_code = 200L,
  content = charToRaw(getProjectJson)
)
listModelsUrl <- UrlJoin(projectUrl, "models")
listModelsJson <- fileToChar("responses/ListModels.json")
modelsResponse <- httr:::response(
  url = listModelsUrl,
  status_code = 200L,
  content = charToRaw(listModelsJson)
)
listModelJobsUrl <- UrlJoin(projectUrl, "modelJobs")
modelsJobResponse <- httr:::response(
  url = listModelJobsUrl,
  status_code = 200L,
  content = charToRaw("[]")
)


test_that("it can get a list of models", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(modelsResponse)
  getStub$onCall(3)$returns(modelsJobResponse)
  models <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListModels(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 3)
  expect_s3_class(models, c("listOfModels", "listSubclass"))
  expect_s3_class(models[[1]], "dataRobotModel")
  ExpectHasKeys(models[[1]],
    c("projectId", "projectName", "modelId", "blueprintId", "featurelistId")
  )
  ExpectHasKeys(models[[1]]$metrics, "LogLoss")
  expect_type(models[[1]]$metrics, "list")
  expect_s3_class(models[[1]]$metrics$LogLoss, "data.frame")
  ExpectHasKeys(models[[1]]$metrics$LogLoss, "validation")
})


test_that("it can summarize a list of models", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(modelsResponse)
  getStub$onCall(3)$returns(modelsJobResponse)
  modelSummary <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    summary(ListModels(fakeProject))
  )
  expect_equal(getStub$calledTimes(), 3)
  expect_type(modelSummary, "list")
  ExpectHasKeys(modelSummary, c("generalSummary", "detailedSummary"))
  expect_type(modelSummary$generalSummary, "character")
  expect_s3_class(modelSummary$detailedSummary, "data.frame")
  ExpectHasKeys(
    modelSummary$detailedSummary,
    c(
      "modelType", "expandedModel", "modelId", "blueprintId", "featurelistName",
      "featurelistId", "samplePct", "validationMetric"
    )
  )
  expect_equal(nrow(modelSummary$detailedSummary), 6)
})


test_that("it can get a list of models by projectId", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(modelsResponse)
  getStub$onCall(3)$returns(modelsJobResponse)
  models <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListModels(fakeProjectId)
  )
  expect_equal(getStub$calledTimes(), 3)
  expect_s3_class(models, c("listOfModels", "listSubclass"))
  expect_s3_class(models[[1]], "dataRobotModel")
})


test_that("ListModels messages when autopilot is not yet done", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(modelsResponse)
  modelJobJson <- fileToChar("responses/modelJob.json")
  listModelJobsUrl <- UrlJoin(projectUrl, "modelJobs")
  modelsJobResponse <- httr:::response(
    url = listModelJobsUrl,
    status_code = 200L,
    content = charToRaw(modelJobJson)
  )
  getStub$onCall(3)$returns(modelsJobResponse)
  expect_message(
    {
      models <- with_mock(
        "httr::GET" = getStub$f,
        "datarobot:::Endpoint" = function() fakeEndpoint,
        "datarobot:::Token" = function() fakeToken,
        ListModels(fakeProjectId)
      )
    },
    "Some models are still in progress"
  )
})

test_that("ListModels messages when there are no models", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(projectResponse)
  listModelsUrl <- UrlJoin(projectUrl, "models")
  modelsResponse <- httr:::response(
    url = listModelsUrl,
    status_code = 200L,
    content = charToRaw("[]")
  )
  getStub$onCall(2)$returns(modelsResponse)
  getStub$onCall(3)$returns(modelsJobResponse)
  expect_message(
    {
      models <- with_mock(
        "httr::GET" = getStub$f,
        "datarobot:::Endpoint" = function() fakeEndpoint,
        "datarobot:::Token" = function() fakeToken,
        ListModels(fakeProjectId)
      )
    },
    "No models have been built yet in this project."
  )
  expect_equal(getStub$calledTimes(), 3)
  expect_s3_class(models, c("listOfModels", "listSubclass"))
  expect_equal(length(models), 0)
})


test_that("filter parameter must be a list", {
  expect_error(ListModels(fakeProject, filter = "not-a-list"))
})

test_that("orderBy parameter must be a character vector", {
  expect_error(ListModels(fakeProject, orderBy = FALSE))
})


test_that("it can filter models", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(modelsResponse)
  getStub$onCall(3)$returns(modelsJobResponse)
  models <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListModels(fakeProject, filter = list("samplePct__gt" = 64))
  )
  expect_equal(getStub$calledTimes(), 3)
  expect_s3_class(models[[1]], "dataRobotModel")
})


test_that("it can filter models by name", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(modelsResponse)
  getStub$onCall(3)$returns(modelsJobResponse)
  models <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListModels(fakeProject, filter = list("name" = "Ridge"))
  )
  expect_equal(getStub$calledTimes(), 3)
  expect_s3_class(models[[1]], "dataRobotModel")
})


test_that("it can filter models by star status", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(modelsResponse)
  getStub$onCall(3)$returns(modelsJobResponse)
  models <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListModels(fakeProject, filter = list("isStarred" = TRUE))
  )
  expect_equal(getStub$calledTimes(), 3)
  expect_s3_class(models[[1]], "dataRobotModel")
})


test_that("it can order models", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(modelsResponse)
  getStub$onCall(3)$returns(modelsJobResponse)
  models <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListModels(fakeProject, orderBy = "samplePct")
  )
  expect_equal(getStub$calledTimes(), 3)
  expect_s3_class(models[[1]], "dataRobotModel")
})


test_that("it can order models by two parameters in a vector", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(modelsResponse)
  getStub$onCall(3)$returns(modelsJobResponse)
  models <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListModels(fakeProject, orderBy = c("samplePct", "metric"))
  )
  expect_equal(getStub$calledTimes(), 3)
  expect_s3_class(models[[1]], "dataRobotModel")
})


test_that("it can order models by two parameters in a single string", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(modelsResponse)
  getStub$onCall(3)$returns(modelsJobResponse)
  models <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListModels(fakeProject, orderBy = "samplePct,metric")
  )
  expect_equal(getStub$calledTimes(), 3)
  expect_s3_class(models[[1]], "dataRobotModel")
})
