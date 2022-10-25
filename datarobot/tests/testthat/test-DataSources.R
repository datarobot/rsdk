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

test_that("it can list dataSources", {
  getStub <- stub(httr::GET)
  listDataSourceUrl <- UrlJoin("externalDataSources")
  listDataSourceJson <- fileToChar("responses/listDataSources.json")
  dataSourceResponse <- httr:::response(
    url = listDataSourceUrl,
    status_code = 200L,
    content = charToRaw(listDataSourceJson)
  )
  getStub$onCall(1)$returns(dataSourceResponse)
  dataSources <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListDataSources()
  )
  expect_equal(getStub$calledTimes(), 1)
  expectedCols <- c("id", "canonicalName", "type", "table", "dataStoreId", "creator", "updated")
  ExpectHasKeys(dataSources, expectedCols)
})

test_that("it can get a dataSource", {
  getStub <- stub(httr::GET)
  getDataSourceUrl <- UrlJoin("externalDataSources", fakeDataSourceId)
  getDataSourceJson <- fileToChar("responses/getDataSource.json")
  dataSourceResponse <- httr:::response(
    url = getDataSourceUrl,
    status_code = 200L,
    content = charToRaw(getDataSourceJson)
  )
  getStub$onCall(1)$returns(dataSourceResponse)
  dataSource <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDataSource(fakeDataSourceId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(dataSource, "dataRobotDataSource")
  expectedCols <- c("id", "canonicalName", "type", "updated", "creator", "params")
  ExpectHasKeys(dataSource, expectedCols)
})

test_that("it can create a new data source", {
  postStub <- stub(httr::POST)
  createDataSourceUrl <- UrlJoin("externalDataSources")
  createDataSourceJson <- fileToChar("responses/createDataSource.json")
  createDataSourceResponse <- httr:::response(
    url = createDataSourceUrl,
    status_code = 202L,
    content = charToRaw(createDataSourceJson)
  )
  postStub$onCall(1)$returns(createDataSourceResponse)
  response <- with_mock(
    `httr::POST` = postStub$f,
    `httr::GET` = function() stop("Should not be called!"),
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    CreateDataSource(
      type = "jdbc",
      canonicalName = "test",
      dataStoreId = fakeDataStoreId
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_s3_class(response, "dataRobotDataSource")
})

test_that("it can update a data source", {
  patchStub <- stub(httr::PATCH)
  patchDataSourceUrl <- UrlJoin("externalDataSources", fakeDataSourceId)
  patchDataSourceJson <- fileToChar("responses/updateDataSource.json")
  dataSourceResponse <- httr:::response(
    url = patchDataSourceUrl,
    status_code = 202L,
    content = charToRaw(patchDataSourceJson)
  )
  patchStub$onCall(1)$returns(dataSourceResponse)
  dataSource <- with_mock(
    "httr::PATCH" = patchStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UpdateDataSource(fakeDataSourceId, canonicalName = "New Name")
  )
  expect_equal(patchStub$calledTimes(), 1)
  expect_s3_class(dataSource, "dataRobotDataSource")
  expect_equal(dataSource$canonicalName, "New Name")
})

test_that("it can update a data source with an object", {
  patchStub <- stub(httr::PATCH)
  patchDataSourceUrl <- UrlJoin("externalDataSources", fakeDataSourceId)
  patchDataSourceJson <- fileToChar("responses/updateDataSource.json")
  dataSourceResponse <- httr:::response(
    url = patchDataSourceUrl,
    status_code = 202L,
    content = charToRaw(patchDataSourceJson)
  )
  patchStub$onCall(1)$returns(dataSourceResponse)
  dataSource <- with_mock(
    "httr::PATCH" = patchStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UpdateDataSource(fakeDataSource, canonicalName = "New Name")
  )
  expect_equal(patchStub$calledTimes(), 1)
  expect_s3_class(dataSource, "dataRobotDataSource")
  expect_equal(dataSource$canonicalName, "New Name")
})

test_that("it can delete a data source", {
  deleteStub <- stub(httr::DELETE)
  deleteDataSourceUrl <- UrlJoin("externalDataSources", fakeDataSourceId)
  dataSourceResponse <- httr:::response(
    url = deleteDataSourceUrl,
    status_code = 204L,
    content = raw(0)
  )
  deleteStub$onCall(1)$returns(dataSourceResponse)
  dataSource <- with_mock(
    "httr::DELETE" = deleteStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DeleteDataSource(fakeDataSourceId)
  )
  expect_null(dataSource)
})

test_that("it can delete a data source with an object", {
  deleteStub <- stub(httr::DELETE)
  deleteDataSourceUrl <- UrlJoin("externalDataSources", fakeDataSourceId)
  dataSourceResponse <- httr:::response(
    url = deleteDataSourceUrl,
    status_code = 204L,
    content = raw(0)
  )
  deleteStub$onCall(1)$returns(dataSourceResponse)
  dataSource <- with_mock(
    "httr::DELETE" = deleteStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DeleteDataSource(fakeDataSource)
  )
  expect_null(dataSource)
})
