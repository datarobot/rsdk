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

GetListOfModels <- function() {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  getStub$onCall(1)$returns(projectResponse)
  listModelsUrl <- UrlJoin(projectUrl, "models")
  listModelsJson <- fileToChar("responses/ListModels.json")
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
    ListModels(fakeProjectId)
  )
  expect_equal(getStub$calledTimes(), 3)
  expect_s3_class(models, c("listOfModels", "listSubclass"))
  models
}

test_that("it can turn a list of models into a dataframe", {
  models <- GetListOfModels()
  dataFrame <- as.data.frame(models)
  expect_s3_class(dataFrame, "data.frame")
  ExpectHasKeys(dataFrame, c(
    "modelType", "expandedModel", "modelId", "blueprintId",
    "featurelistName", "featurelistId", "samplePct",
    "validationMetric"
  ))
  expect_equal(nrow(dataFrame), length(models))
})

test_that("it can turn a list of models into a dataframe, simple = TRUE", {
  models <- GetListOfModels()
  dataFrame <- as.data.frame(models, simple = TRUE)
  expect_s3_class(dataFrame, "data.frame")
  ExpectHasKeys(dataFrame, c(
    "modelType", "expandedModel", "modelId", "blueprintId",
    "featurelistName", "featurelistId", "samplePct",
    "validationMetric"
  ))
  expect_equal(nrow(dataFrame), length(models))
})

test_that("it can turn a list of models into a dataframe, simple = FALSE", {
  models <- GetListOfModels()
  dataFrame <- as.data.frame(models, simple = FALSE)
  expect_s3_class(dataFrame, "data.frame")
  ExpectHasKeys(dataFrame, c(
    "modelType", "expandedModel", "modelId", "blueprintId",
    "featurelistName", "featurelistId", "samplePct",
    "modelCategory", "projectName", "projectId", "projectTarget",
    "projectMetric", "AUC.validation", "Rate@Top5%.validation",
    "RMSE.validation", "Kolmogorov-Smirnov.validation",
    "Rate@TopTenth%.validation", "LogLoss.validation",
    "FVE Binomial.validation", "Gini Norm.validation",
    "Rate@Top10%.validation", "AUC.crossValidation",
    "Rate@Top5%.crossValidation", "RMSE.crossValidation",
    "Kolmogorov-Smirnov.crossValidation",
    "Rate@TopTenth%.crossValidation", "LogLoss.crossValidation",
    "FVE Binomial.crossValidation", "Gini Norm.crossValidation",
    "Rate@Top10%.crossValidation", "AUC.holdout",
    "Rate@Top5%.holdout", "RMSE.holdout",
    "Kolmogorov-Smirnov.holdout", "Rate@TopTenth%.holdout",
    "LogLoss.holdout", "FVE Binomial.holdout",
    "Gini Norm.holdout", "Rate@Top10%.holdout"
  ))
  expect_equal(nrow(dataFrame), length(models))
})

test_that("it can turn into a dataframe, simple = TRUE with missing featurelist", {
  models <- GetListOfModels()
  models[[1]]$featurelistName <- NULL
  models[[1]]$featurelistId <- NULL
  dataFrame <- as.data.frame(models, simple = TRUE)
  expect_s3_class(dataFrame, "data.frame")
  ExpectHasKeys(dataFrame, c(
    "modelType", "expandedModel", "modelId", "blueprintId",
    "featurelistName", "featurelistId", "samplePct",
    "validationMetric"
  ))
  expect_equal(dataFrame$featurelistName[[1]], "Multiple featurelists")
  expect_equal(dataFrame$featurelistId[[1]], "Multiple featurelist ids")
  expect_equal(dataFrame$featurelistName[[2]], "Informative Features")
  expect_equal(dataFrame$featurelistId[[2]], "5ade643f1b17bdf9e5b15067")
})

test_that("it can turn into a dataframe, simple = FALSE with missing featurelist", {
  models <- GetListOfModels()
  models[[1]]$featurelistName <- NULL
  models[[1]]$featurelistId <- NULL
  dataFrame <- as.data.frame(models, simple = FALSE)
  expect_s3_class(dataFrame, "data.frame")
  expect_equal(dataFrame$featurelistName[[1]], "Multiple featurelists")
  expect_equal(dataFrame$featurelistId[[1]], "Multiple featurelist ids")
  expect_equal(dataFrame$featurelistName[[2]], "Informative Features")
  expect_equal(dataFrame$featurelistId[[2]], "5ade643f1b17bdf9e5b15067")
})

test_that("it can turn into a dataframe, simple = TRUE with missing samplePct", {
  models <- GetListOfModels()
  models[[1]]$samplePct <- NULL
  dataFrame <- as.data.frame(models, simple = TRUE)
  expect_s3_class(dataFrame, "data.frame")
  ExpectHasKeys(dataFrame, c(
    "modelType", "expandedModel", "modelId", "blueprintId",
    "featurelistName", "featurelistId", "samplePct",
    "validationMetric"
  ))
  expect_true(is.na(dataFrame$samplePct[[1]]))
})

test_that("it can turn into a dataframe, simple = FALSE with missing featurelist", {
  models <- GetListOfModels()
  models[[1]]$samplePct <- NULL
  dataFrame <- as.data.frame(models, simple = FALSE)
  expect_s3_class(dataFrame, "data.frame")
  expect_true(is.na(dataFrame$samplePct[[1]]))
})

test_that("it can turn into a dataframe, simple = TRUE with row names", {
  models <- GetListOfModels()
  models[[1]]$featurelistName <- NULL
  models[[1]]$featurelistId <- NULL
  newNames <- paste0("newName", seq_along(models))
  dataFrame <- as.data.frame(models, simple = TRUE, row.names = newNames)
  expect_s3_class(dataFrame, "data.frame")
  expect_equal(row.names(dataFrame), newNames)
})

test_that("it can turn into a dataframe, simple = FALSE with row names", {
  models <- GetListOfModels()
  models[[1]]$featurelistName <- NULL
  models[[1]]$featurelistId <- NULL
  newNames <- paste0("newName", seq_along(models))
  dataFrame <- as.data.frame(models, simple = FALSE, row.names = newNames)
  expect_s3_class(dataFrame, "data.frame")
  expect_equal(row.names(dataFrame), newNames)
})

test_that("simple must be logical", {
  models <- GetListOfModels()
  expect_error(as.data.frame(models, simple = 3), "must be TRUE or FALSE")
})

test_that("as.data.frame for empty 'listOfModels' object", {
  emptyModelList <- list()
  class(emptyModelList) <- c("listOfModels", "listSubclass")
  dataFrame <- as.data.frame(emptyModelList)
  ExpectHasKeys(dataFrame, c(
    "modelType", "expandedModel", "modelId", "blueprintId",
    "featurelistName", "featurelistId", "samplePct",
    "validationMetric"
  ))
  expect_equal(nrow(dataFrame), 0)
})
