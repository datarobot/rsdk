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

test_that("it can get all model recommendations", {
  getStub <- stub(httr::GET)
  listModelRecommendationsUrl <- UrlJoin(projectUrl, "recommendedModels")
  listModelRecommendationsJson <- fileToChar("responses/listModelRecommendations.json")
  modelRecommendationResponse <- httr:::response(
    url = listModelRecommendationsUrl,
    status_code = 200L,
    content = charToRaw(listModelRecommendationsJson)
  )
  getStub$onCall(1)$returns(modelRecommendationResponse)
  modelRecommendations <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListModelRecommendations(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(length(modelRecommendations), 3)
  expect_s3_class(modelRecommendations, "listOfModelRecommendations")
  expect_s3_class(modelRecommendations[[1]], "dataRobotModelRecommendation")
})

test_that("if there are no recommendations, getting all recommendations returns nothing", {
  getStub <- stub(httr::GET)
  modelRecommendationUrl <- UrlJoin(projectUrl, "modelRecommendations")
  modelRecommendationResponse <- httr:::response(
    url = modelRecommendationUrl,
    status_code = 200L,
    content = charToRaw("[]")
  )
  getStub$onCall(1)$returns(modelRecommendationResponse)
  modelRecommendations <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListModelRecommendations(fakeProject)
  )
  emptyList <- structure(list(), class = c(
    "listOfModelRecommendations",
    "listSubclass"
  ))
  expect_equal(modelRecommendations, emptyList)
  expect_equal(getStub$calledTimes(), 1)
})


listModelRecommendationsUrl <- UrlJoin(projectUrl, "recommendedModels")
listModelRecommendationsJson <- fileToChar("responses/listModelRecommendations.json")
modelRecommendationResponse <- httr:::response(
  url = listModelRecommendationsUrl,
  status_code = 200L,
  content = charToRaw(listModelRecommendationsJson)
)

test_that("it can get a model recommendation", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(modelRecommendationResponse)
  modelRecommendation <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetModelRecommendation(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(modelRecommendation, "dataRobotModelRecommendation")
  expectedCols <- c("projectId", "recommendationType", "modelId")
  expect_equal(sort(names(modelRecommendation)), sort(expectedCols))
})

test_that("it can get a model recommendation for a particular type", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(modelRecommendationResponse)
  modelRecommendation <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetModelRecommendation(fakeProject, type = "Fast & Accurate")
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(modelRecommendation, "dataRobotModelRecommendation")
  expectedCols <- c("projectId", "recommendationType", "modelId")
  expect_equal(modelRecommendation$recommendationType, "Fast & Accurate")
  expect_equal(sort(names(modelRecommendation)), sort(expectedCols))
})

test_that("it can get a recommended model", {
  getStub <- stub(httr::GET)
  modelJson <- fileToChar("responses/getModel.json")
  completedModelResponse <- httr:::response(
    url = modelUrl,
    status_code = 200L,
    content = charToRaw(modelJson)
  )
  getStub$onCall(1)$returns(completedModelResponse)
  model <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::GetModelRecommendation" = function(project, type) {
      list(
        modelId = "bogus",
        projectId = "bogus",
        recommendedType = "whatever"
      )
    },
    "datarobot::GetProject" = function(project) {
      p <- fromJSON(fileToChar("responses/GetProject.json"))
      datarobot:::as.dataRobotProject(p)
    }, GetRecommendedModel(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(model, "dataRobotModel")
})
