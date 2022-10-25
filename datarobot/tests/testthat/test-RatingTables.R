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

fakeRatingTableId <- "rating-table-id"
fakeRatingTableJobId <- "rating-table-job-id"
fakeRatingTableModelJobId <- "rating-table-model-job-id"
getRatingTableModelUrl <- UrlJoin(projectUrl, "ratingTableModel", fakeModelId)
listRatingTableModelsUrl <- UrlJoin(projectUrl, "ratingTableModel")
getRatingTableUrl <- UrlJoin(projectUrl, "ratingTable", fakeModelId)
listRatingTablesUrl <- UrlJoin(projectUrl, "ratingTable")

test_that("it can get a model with a rating table", {
  getStub <- stub(httr::GET)
  getRatingTableModelJson <- fileToChar("responses/getRatingTableModel.json")
  ratingTableResponse <- httr:::response(
    url = getRatingTableModelUrl,
    status_code = 200L,
    content = charToRaw(getRatingTableModelJson)
  )
  getStub$onCall(1)$returns(ratingTableResponse)
  ratingTableModel <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetRatingTableModel(fakeProject, fakeModelId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(ratingTableModel, "dataRobotRatingTableModel")
  expectedCols <- c(
    "projectId", "modelType", "metrics", "blueprintId", "ratingTableId", "id",
    "featurelistId", "processes", "featurelistName", "samplePct", "isFrozen",
    "modelCategory", "modelNumber"
  )
  ExpectHasKeys(ratingTableModel, expectedCols)
})

test_that("it can get all models with rating tables", {
  getStub <- stub(httr::GET)
  listRatingTableModelsJson <- fileToChar("responses/listRatingTableModels.json")
  ratingTableResponse <- httr:::response(
    url = listRatingTableModelsUrl,
    status_code = 200L,
    content = charToRaw(listRatingTableModelsJson)
  )
  getStub$onCall(1)$returns(ratingTableResponse)
  ratingTableModels <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListRatingTableModels(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(length(ratingTableModels), 1)
  expect_s3_class(ratingTableModels, "listOfRatingTableModels")
  expect_s3_class(ratingTableModels[[1]], "dataRobotRatingTableModel")
})

test_that("if there are no models with rating tables, getting all models
          with rating tables returns nothing", {
  getStub <- stub(httr::GET)
  ratingTableResponse <- httr:::response(
    url = listRatingTableModelsUrl,
    status_code = 200L,
    content = charToRaw("[]")
  )
  getStub$onCall(1)$returns(ratingTableResponse)
  ratingTables <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListRatingTableModels(fakeProject)
  )
  emptyList <- structure(list(), class = c(
    "listOfRatingTableModels",
    "listOfModels",
    "listSubclass"
  ))
  expect_equal(ratingTables, emptyList)
  expect_equal(getStub$calledTimes(), 1)
})


expectedCols <- c(
  "validationJobId", "validationError", "projectId", "ratingTableName",
  "parentModelId", "modelJobId", "id", "originalFilename", "modelId"
)

test_that("it can get a rating table", {
  getStub <- stub(httr::GET)
  getRatingTableJson <- fileToChar("responses/getRatingTable.json")
  ratingTableResponse <- httr:::response(
    url = getRatingTableUrl,
    status_code = 200L,
    content = charToRaw(getRatingTableJson)
  )
  getStub$onCall(1)$returns(ratingTableResponse)
  ratingTable <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetRatingTable(fakeProject, fakeRatingTableId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(ratingTable, "dataRobotRatingTable")
  ExpectHasKeys(ratingTable, expectedCols)
})

test_that("if it gets an invalid rating table, it gets fine but has a warning", {
  getStub <- stub(httr::GET)
  getRatingTableJson <- fileToChar("responses/getInvalidRatingTable.json")
  ratingTableResponse <- httr:::response(
    url = getRatingTableUrl,
    status_code = 200L,
    content = charToRaw(getRatingTableJson)
  )
  getStub$onCall(1)$returns(ratingTableResponse)
  expect_warning(
    {
      ratingTable <- with_mock(
        "httr::GET" = getStub$f,
        "datarobot:::Endpoint" = function() fakeEndpoint,
        "datarobot:::Token" = function() fakeToken,
        GetRatingTable(fakeProject, fakeRatingTableId)
      )
    },
    "The retrieved rating table was invalid"
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(ratingTable, "dataRobotRatingTable")
  expectedCols <- c(
    "validationJobId", "validationError", "projectId", "ratingTableName",
    "parentModelId", "modelJobId", "id", "originalFilename", "modelId"
  )
  ExpectHasKeys(ratingTable, expectedCols)
  expect_true(grepl("Wrong number of columns", ratingTable$validationError))
})

test_that("it can get all rating tables", {
  getStub <- stub(httr::GET)
  listRatingTablesJson <- fileToChar("responses/listRatingTables.json")
  ratingTablesResponse <- httr:::response(
    url = listRatingTablesUrl,
    status_code = 200L,
    content = charToRaw(listRatingTablesJson)
  )
  getStub$onCall(1)$returns(ratingTablesResponse)
  ratingTables <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListRatingTables(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(length(ratingTables), 2)
  expect_s3_class(ratingTables, "listOfRatingTables")
  expect_s3_class(ratingTables[[1]], "dataRobotRatingTable")
})

test_that("if there are no rating tables, getting all rating tables returns nothing", {
  getStub <- stub(httr::GET)
  ratingTableResponse <- httr:::response(
    url = listRatingTableModelsUrl,
    status_code = 200L,
    content = charToRaw("[]")
  )
  getStub$onCall(1)$returns(ratingTableResponse)
  ratingTables <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListRatingTables(fakeProject)
  )
  emptyList <- structure(list(), class = c("listOfRatingTables", "listSubclass"))
  expect_equal(ratingTables, emptyList)
  expect_equal(getStub$calledTimes(), 1)
})

test_that("it can download a rating table", {
  getStub <- stub(httr::GET)
  downloadRatingTableUrl <- UrlJoin(projectUrl, "ratingTables", fakeRatingTableId, "file")
  downloadRatingTableTxt <- fileToChar("responses/downloadRatingTable.txt")
  downloadedRatingTableResponse <- httr:::response(
    url = downloadRatingTableUrl,
    status_code = 200L,
    content = charToRaw(downloadRatingTableTxt)
  )
  getStub$onCall(1)$returns(downloadedRatingTableResponse)
  expect_false(file.exists(fakeFilePath))
  with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DownloadRatingTable(fakeProject, fakeRatingTableId, fakeFilePath)
  )
  expect_equal(getStub$calledTimes(), 1)
})

test_that("it can create a new rating table from a rating table CSV", {
  postStub <- stub(httr::POST)
  createRatingTableUrl <- UrlJoin(projectUrl, "ratingTables")
  createRatingTableJson <- fileToChar("responses/createRatingTable.json")
  createRatingTableResponse <- httr:::response(
    url = createRatingTableUrl,
    status_code = 202L,
    content = charToRaw(createRatingTableJson)
  )
  postStub$onCall(1)$returns(createRatingTableResponse)
  response <- with_mock(
    `httr::POST` = postStub$f,
    `httr::GET` = function() stop("Should not be called!"),
    `datarobot:::UploadData` = function(file) file,
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    `datarobot:::JobIdFromResponse` = identity,
    CreateRatingTable(fakeProject, fakeModelId, fakeFilePath)
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(response$url, createRatingTableUrl)
})

test_that("it can rename a rating table", {
  patchStub <- stub(httr::PATCH)
  postRatingTableUrl <- UrlJoin(projectUrl, "ratingTable", fakeRatingTableId)
  postRatingTableJson <- fileToChar("responses/getRatingTable.json")
  ratingTableResponse <- httr:::response(
    url = postRatingTableUrl,
    status_code = 202L,
    content = charToRaw(postRatingTableJson)
  )
  patchStub$onCall(1)$returns(ratingTableResponse)
  ratingTable <- with_mock(
    "httr::PATCH" = patchStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RenameRatingTable(fakeProject, fakeRatingTableId, "ThrowawayName")
  )
  expect_equal(patchStub$calledTimes(), 1)
  expect_s3_class(ratingTable, "dataRobotRatingTable")
  ExpectHasKeys(ratingTable, expectedCols)
})

test_that("It can get a rating table from a rating table creation job ID", {
  getRatingTableJson <- fileToChar("responses/getRatingTable.json")
  waitResponse <- httr:::response(
    url = getRatingTableUrl,
    status_code = 303L,
    content = charToRaw(getRatingTableJson)
  )
  ratingTable <- with_mock(
    "datarobot::WaitForAsyncReturn" = function(...) {
      ParseReturnResponse(waitResponse)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetRatingTableFromJobId(fakeProject, fakeRatingTableJobId)
  )
  expect_s3_class(ratingTable, "dataRobotRatingTable")
  ExpectHasKeys(ratingTable, expectedCols)
})

test_that("if it gets an invalid rating table from job ID, it gets fine but has a warning", {
  getStub <- stub(httr::GET)
  getRatingTableJson <- fileToChar("responses/getInvalidRatingTable.json")
  waitResponse <- httr:::response(
    url = getRatingTableUrl,
    status_code = 303L,
    content = charToRaw(getRatingTableJson)
  )
  expect_warning(
    {
      ratingTable <- with_mock(
        "datarobot::WaitForAsyncReturn" = function(...) {
          ParseReturnResponse(waitResponse)
        },
        "datarobot:::Endpoint" = function() fakeEndpoint,
        "datarobot:::Token" = function() fakeToken,
        GetRatingTableFromJobId(fakeProject, fakeRatingTableId)
      )
    },
    "The retrieved rating table was invalid"
  )
  expect_s3_class(ratingTable, "dataRobotRatingTable")
  ExpectHasKeys(ratingTable, expectedCols)
  expect_true(grepl("Wrong number of columns", ratingTable$validationError))
})

test_that("it can create a new model from a rating table", {
  postStub <- stub(httr::POST)
  createRatingTableModelUrl <- UrlJoin(projectUrl, "ratingTableModels")
  createRatingTableModelJson <- fileToChar("responses/requestNewRatingTableModel.json")
  ratingTableModelResponse <- httr:::response(
    url = createRatingTableModelUrl,
    status_code = 200L,
    content = charToRaw(createRatingTableModelJson)
  )
  postStub$onCall(1)$returns(ratingTableModelResponse)
  response <- with_mock(
    `httr::POST` = postStub$f,
    `httr::GET` = function() stop("Should not be called!"),
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    `datarobot:::JobIdFromResponse` = identity,
    RequestNewRatingTableModel(fakeProject, fakeRatingTableId)
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(response$url, createRatingTableModelUrl)
})

test_that("it can get a rating table model from a job ID", {
  getStub <- stub(httr::GET)
  getRatingTableJobUrl <- UrlJoin(projectUrl, "jobs", fakeRatingTableJobId)
  getRatingTableJobJson <- fileToChar("responses/getCreateRatingTableJob.json")
  ratingTableJobResponse <- httr:::response(
    url = getRatingTableJobUrl,
    status_code = 200L,
    content = charToRaw(getRatingTableJobJson)
  )
  getStub$onCall(1)$returns(ratingTableJobResponse)
  getRatingTableModelJson <- fileToChar("responses/getRatingTableModel.json")
  ratingTableModelResponse <- httr:::response(
    url = getRatingTableModelUrl,
    status_code = 200L,
    content = charToRaw(getRatingTableModelJson)
  )
  asyncStub <- stub(datarobot:::WaitForAsyncReturn)
  asyncStub$onCall(1)$returns(ParseReturnResponse(ratingTableJobResponse))
  getStub$onCall(1)$returns(ratingTableModelResponse)
  getStub$onCall(2)$returns(ratingTableModelResponse)
  getStub$onCall(3)$returns(ratingTableModelResponse)
  ratingTableModel <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::WaitForAsyncReturn" = asyncStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetRatingTableModelFromJobId(fakeProject, fakeRatingTableModelJobId)
  )
  expect_equal(getStub$calledTimes(), 3)
  expect_equal(asyncStub$calledTimes(), 1)
  expect_s3_class(ratingTableModel, "dataRobotRatingTableModel")
  expectedCols <- c(
    "projectId", "modelType", "metrics", "blueprintId", "ratingTableId", "id",
    "featurelistId", "processes", "featurelistName", "samplePct", "isFrozen",
    "modelCategory", "modelNumber"
  )
  ExpectHasKeys(ratingTableModel, expectedCols)
})
