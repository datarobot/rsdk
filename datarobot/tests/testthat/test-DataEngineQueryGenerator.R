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

generatorId <- "59a5af20c80891534e3c2bde"

CreateDataEngineQueryGeneratorUrl <- UrlJoin("dataEngineQueryGenerators")
GetDataEngineQueryGeneratorUrl <- UrlJoin("dataEngineQueryGenerators", generatorId)

test_that("create DataEngineQueryGenerator", {
  postStub <- stub(httr::POST)
  createQueryGeneratorJson <- fileToChar("responses/dataEngineQueryGenerator.json")
  createQueryGeneratorResponse <- httr:::response(
    url = CreateDataEngineQueryGeneratorUrl,
    status_code = 202L,
    content = charToRaw(createQueryGeneratorJson)
  )
  postStub$onCall(1)$returns(createQueryGeneratorResponse)

  queryGenerator <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot::WaitForAsyncReturn" = function(...) {
      jsonlite::fromJSON(createQueryGeneratorJson)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    CreateDataEngineQueryGenerator("TimeSeries", "someDataset", "genSettings")
  )
  expect_equal(postStub$calledTimes(), 1)
})

test_that("get DataEngineQueryGenerator", {
  getStub <- stub(httr::GET)
  getQueryGeneratorJson <- fileToChar("responses/dataEngineQueryGenerator.json")
  getQueryGeneratorResponse <- httr:::response(
    url = GetDataEngineQueryGeneratorUrl,
    status_code = 200L,
    content = charToRaw(getQueryGeneratorJson)
  )
  getStub$onCall(1)$returns(getQueryGeneratorResponse)

  queryGenerator <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDataEngineQueryGenerator(generatorId)
  )
  expect_equal(getStub$calledTimes(), 1)

  expectedCols <- c("id", "query", "datasets", "generatorType", "generatorSettings")
  expect_type(queryGenerator, "list")
  expect_s3_class(queryGenerator, "dataRobotQueryGenerator")
  expect_equal(queryGenerator$id, generatorId)
  expect_equal(queryGenerator$generatorType, "TimeSeries")
  expect_s3_class(queryGenerator$generatorSettings, "queryGeneratorSettings")
  expect_type(queryGenerator$datasets, "list")
  expect_s3_class(queryGenerator$datasets[[1]], "queryGeneratorDataset")
  ExpectHasKeys(queryGenerator, keys = expectedCols)
})

test_that("create dataset from DataEngineQueryGenerator", {
  postStub <- stub(httr::POST)
  createWorkspaceStateJson <- '{"workspaceStateId": "59a5af20c80891534e3c2bde"}'
  createWorkspaceStateUrl <- UrlJoin("dataEngineWorkspaceStates", "fromDataEngineQueryGenerator")
  createWorkspaceStateResponse <- httr:::response(
    url = createWorkspaceStateUrl,
    status_code = 201L,
    content = charToRaw(createWorkspaceStateJson)
  )
  postStub$onCall(1)$returns(createWorkspaceStateResponse)

  createDatasetJson <- fileToChar("responses/datasetFromQueryGenerator.json")
  createDatasetUrl <- UrlJoin("datasets", "fromDataEngineWorkspaceState")
  createDatasetResponse <- httr:::response(
    url = createDatasetUrl,
    status_code = 202L,
    content = charToRaw(createDatasetJson)
  )
  postStub$onCall(2)$returns(createDatasetResponse)

  dataset <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        jsonlite::fromJSON(createDatasetJson)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      CreateDatasetFromDataEngineQueryGenerator("someGeneratorId")
    )
  )
  expect_equal(postStub$calledTimes(), 2)
  expectedCols <- c(
    "datasetId",
    "versionId",
    "name",
    "categories",
    "creationDate",
    "createdBy",
    "dataPersisted",
    "isDataEngineEligible",
    "isLatestVersion",
    "isSnapshot",
    "datasetSize",
    "rowCount",
    "processingState"
  )
  expect_s3_class(dataset, "dataRobotDataset")
  print(dataset$categories)
  expect_equal(dataset$categories, c("TRAINING", "PREDICTION", "MULTI_SERIES_CALENDAR"))
  expect_equal(dataset$creationDate, datarobot.apicore::ParseRFC3339Timestamp("2021-10-05T15:57:52.000000Z"))
  expect_true(dataset$dataPersisted)
  expect_true(dataset$isDataEngineEligible)
  expect_true(dataset$isLatestVersion)
  expect_true(dataset$isSnapshot)
  ExpectHasKeys(dataset, keys = expectedCols)
})
