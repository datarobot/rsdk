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

test_that("it can list dataStores", {
  getStub <- stub(httr::GET)
  listDataStoreUrl <- UrlJoin("externalDataStores")
  listDataStoreJson <- fileToChar("responses/listDataStores.json")
  dataStoreResponse <- httr:::response(
    url = listDataStoreUrl,
    status_code = 200L,
    content = charToRaw(listDataStoreJson)
  )
  getStub$onCall(1)$returns(dataStoreResponse)
  dataStores <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListDataStores()
  )
  expect_equal(getStub$calledTimes(), 1)
  expectedCols <- c("id", "canonicalName", "type", "updated", "creator", "params")
  ExpectHasKeys(dataStores, expectedCols)
})

test_that("it can get a dataStore", {
  getStub <- stub(httr::GET)
  getDataStoreUrl <- UrlJoin("externalDataStores", fakeDataStoreId)
  getDataStoreJson <- fileToChar("responses/getDataStore.json")
  dataStoreResponse <- httr:::response(
    url = getDataStoreUrl,
    status_code = 200L,
    content = charToRaw(getDataStoreJson)
  )
  getStub$onCall(1)$returns(dataStoreResponse)
  dataStore <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDataStore(fakeDataStoreId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(dataStore, "dataRobotDataStore")
  expectedCols <- c("id", "canonicalName", "type", "updated", "creator", "params")
  ExpectHasKeys(dataStore, expectedCols)
})

test_that("it can get a dataStore schema", {
  postStub <- stub(httr::POST)
  getDataStoreSchemaUrl <- UrlJoin("externalDataStores", fakeDataStoreId, "schema")
  getDataStoreSchemaJson <- fileToChar("responses/getDataStoreSchemas.json")
  dataStoreResponse <- httr:::response(
    url = getDataStoreSchemaUrl,
    status_code = 200L,
    content = charToRaw(getDataStoreSchemaJson)
  )
  postStub$onCall(1)$returns(dataStoreResponse)
  dataStoreSchemas <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDataStoreSchemas(fakeDataStoreId, fakeUsername, fakePassword)
  )
  expect_equal(postStub$calledTimes(), 1)
  ExpectHasKeys(dataStoreSchemas, c("catalog", "schemas"))
})

test_that("it can get dataStore tables", {
  postStub <- stub(httr::POST)
  getDataStoreTablesUrl <- UrlJoin("externalDataStores", fakeDataStoreId, "tables")
  getDataStoreTablesJson <- fileToChar("responses/getDataStoreTables.json")
  dataStoreResponse <- httr:::response(
    url = getDataStoreTablesUrl,
    status_code = 200L,
    content = charToRaw(getDataStoreTablesJson)
  )
  postStub$onCall(1)$returns(dataStoreResponse)
  dataStoreTables <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDataStoreTables(fakeDataStoreId, fakeUsername, fakePassword)
  )
  expect_equal(postStub$calledTimes(), 1)
  ExpectHasKeys(dataStoreTables, c("catalog", "tables"))
})

test_that("it can test a dataStore connection", {
  postStub <- stub(httr::POST)
  getDataStoreTestUrl <- UrlJoin("externalDataStores", fakeDataStoreId, "test")
  getDataStoreTestJson <- fileToChar("responses/getDataStoreTest.json")
  dataStoreResponse <- httr:::response(
    url = getDataStoreTestUrl,
    status_code = 200L,
    content = charToRaw(getDataStoreTestJson)
  )
  postStub$onCall(1)$returns(dataStoreResponse)
  dataStoreTest <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    TestDataStore(fakeDataStoreId, fakeUsername, fakePassword)
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_true(dataStoreTest)
})

test_that("it can create a new data store", {
  postStub <- stub(httr::POST)
  createDataStoreUrl <- UrlJoin("externalDataStores")
  createDataStoreJson <- fileToChar("responses/createDataStore.json")
  createDataStoreResponse <- httr:::response(
    url = createDataStoreUrl,
    status_code = 202L,
    content = charToRaw(createDataStoreJson)
  )
  postStub$onCall(1)$returns(createDataStoreResponse)
  response <- with_mock(
    `httr::POST` = postStub$f,
    `httr::GET` = function() stop("Should not be called!"),
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    CreateDataStore(
      type = "jdbc",
      canonicalName = "test",
      driverId = fakeDriverId,
      jdbcUrl = fakeJdbcUrl
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_s3_class(response, "dataRobotDataStore")
})

test_that("it can update a data store", {
  patchStub <- stub(httr::PATCH)
  patchDataStoreUrl <- UrlJoin("externalDataStores", fakeDataStoreId)
  patchDataStoreJson <- fileToChar("responses/updateDataStore.json")
  dataStoreResponse <- httr:::response(
    url = patchDataStoreUrl,
    status_code = 202L,
    content = charToRaw(patchDataStoreJson)
  )
  patchStub$onCall(1)$returns(dataStoreResponse)
  dataStore <- with_mock(
    "httr::PATCH" = patchStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UpdateDataStore(fakeDataStoreId, canonicalName = "New Name")
  )
  expect_equal(patchStub$calledTimes(), 1)
  expect_s3_class(dataStore, "dataRobotDataStore")
  expect_equal(dataStore$canonicalName, "New Name")
})

test_that("it can update a data store with an object", {
  patchStub <- stub(httr::PATCH)
  patchDataStoreUrl <- UrlJoin("externalDataStores", fakeDataStoreId)
  patchDataStoreJson <- fileToChar("responses/updateDataStore.json")
  dataStoreResponse <- httr:::response(
    url = patchDataStoreUrl,
    status_code = 202L,
    content = charToRaw(patchDataStoreJson)
  )
  patchStub$onCall(1)$returns(dataStoreResponse)
  dataStore <- with_mock(
    "httr::PATCH" = patchStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UpdateDataStore(fakeDataStore, canonicalName = "New Name")
  )
  expect_equal(patchStub$calledTimes(), 1)
  expect_s3_class(dataStore, "dataRobotDataStore")
  expect_equal(dataStore$canonicalName, "New Name")
})

test_that("it can delete a data store", {
  deleteStub <- stub(httr::DELETE)
  deleteDataStoreUrl <- UrlJoin("externalDataStores", fakeDataStoreId)
  dataStoreResponse <- httr:::response(
    url = deleteDataStoreUrl,
    status_code = 204L,
    content = raw(0)
  )
  deleteStub$onCall(1)$returns(dataStoreResponse)
  dataStore <- with_mock(
    "httr::DELETE" = deleteStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DeleteDataStore(fakeDataStoreId)
  )
  expect_null(dataStore)
})

test_that("it can delete a data store with an object", {
  deleteStub <- stub(httr::DELETE)
  deleteDataStoreUrl <- UrlJoin("externalDataStores", fakeDataStoreId)
  dataStoreResponse <- httr:::response(
    url = deleteDataStoreUrl,
    status_code = 204L,
    content = raw(0)
  )
  deleteStub$onCall(1)$returns(dataStoreResponse)
  dataStore <- with_mock(
    "httr::DELETE" = deleteStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DeleteDataStore(fakeDataStore)
  )
  expect_null(dataStore)
})
