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

test_that("it can get a list of starred models", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  listModelsUrl <- UrlJoin(projectUrl, "models")
  listModelsJson <- fileToChar("responses/ListStarredModels.json")
  modelsResponse <- httr:::response(
    url = listModelsUrl,
    status_code = 200L,
    content = charToRaw(listModelsJson)
  )
  getStub$onCall(2)$returns(modelsResponse)
  listModelJobsUrl <- UrlJoin(projectUrl, "modelJobs")
  modelsResponse <- httr:::response(
    url = listModelJobsUrl,
    status_code = 200L,
    content = charToRaw("[]")
  )
  getStub$onCall(3)$returns(modelsResponse)
  models <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListStarredModels(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 3)
  expect_s3_class(models, c("listOfModels", "listSubclass"))
  expect_s3_class(models[[1]], "dataRobotModel")
  ExpectHasKeys(models[[1]], c(
    "projectId", "projectName", "modelId", "blueprintId",
    "featurelistId", "isStarred"
  ))
  expect_true(models[[1]]$isStarred)
})


starredModelJson <- fileToChar("responses/getStarredModel.json")
unstarredModelJson <- fileToChar("responses/getModel.json")
starredModelResponse <- httr:::response(
  url = modelUrl,
  status_code = 200L,
  content = charToRaw(starredModelJson)
)
unstarredModelResponse <- httr:::response(
  url = modelUrl,
  status_code = 200L,
  content = charToRaw(unstarredModelJson)
)
mockedStarredModel <- datarobot:::as.dataRobotModel(fromJSON(starredModelJson))
mockedUnstarredModel <- datarobot:::as.dataRobotModel(fromJSON(unstarredModelJson))
starActionResponse <- httr:::response(
  url = "fake-url",
  status_code = 202L,
  headers = list(location = "fake-url"),
  content = raw(0)
)

test_that("StarModel succeeds", {
  patchStub <- stub(httr::PATCH)
  patchStub$onCall(1)$returns(starActionResponse)
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(starredModelResponse)
  model <- with_mock(
    "httr::PATCH" = patchStub$f,
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::GetProject" = function(project) {
      p <- fromJSON(fileToChar("responses/GetProject.json"))
      datarobot:::as.dataRobotProject(p)
    }, StarModel(fakeModel)
  )
  expect_equal(patchStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(model, "dataRobotModel")
  expect_true(model$isStarred)
})


test_that("UnstarModel succeeds", {
  patchStub <- stub(httr::PATCH)
  patchStub$onCall(1)$returns(starActionResponse)
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(unstarredModelResponse)
  model <- with_mock(
    "httr::PATCH" = patchStub$f,
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::GetProject" = function(project) {
      p <- fromJSON(fileToChar("responses/GetProject.json"))
      datarobot:::as.dataRobotProject(p)
    }, UnstarModel(fakeModel)
  )
  expect_equal(patchStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(model, "dataRobotModel")
  expect_false(model$isStarred)
})

test_that("ToggleStarForModel succeeds", {
  patchStub <- stub(httr::PATCH)
  patchStub$onCall(1)$returns(starActionResponse)
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(starredModelResponse)
  model <- with_mock(
    "httr::PATCH" = patchStub$f,
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::GetProject" = function(project) {
      p <- fromJSON(fileToChar("responses/GetProject.json"))
      datarobot:::as.dataRobotProject(p)
    }, ToggleStarForModel(fakeModel)
  )
  expect_equal(patchStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(model, "dataRobotModel")
  expect_true(model$isStarred)
})
