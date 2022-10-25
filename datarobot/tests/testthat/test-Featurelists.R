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


test_that("Required parameters are present", {
  expect_error(CreateFeaturelist())
  expect_error(CreateFeaturelist(preProject))
  expect_error(CreateFeaturelist(preProject, listName))
  expect_error(CreateFeaturelist(preProject, featureNames = featureNames))
  expect_error(CreateFeaturelist(
    listName = listName,
    featureNames = featureNames
  ))
})


test_that("it can create a featurelist", {
  getStub <- stub(httr::GET)
  getFeaturelistsUrl <- UrlJoin(projectUrl, "featurelists", fakeFeaturelistId)
  getFeaturelistsJson <- fileToChar("responses/GetFeaturelist.json")
  featurelistsResponse <- httr:::response(
    url = getFeaturelistsUrl,
    status_code = 200L,
    content = charToRaw(getFeaturelistsJson)
  )
  getStub$onCall(1)$returns(featurelistsResponse)
  postStub <- stub(httr::POST)
  createResponse <- httr:::response(
    url = getFeaturelistsUrl,
    status_code = 303L,
    headers = list(location = jobUrl),
    content = raw(0)
  )
  postStub$onCall(1)$returns(createResponse)
  featurelist <- with_mock(
    "httr::GET" = getStub$f,
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    CreateFeaturelist(fakeProject, fakeFeaturelistName, fakeFeatures)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(postStub$calledTimes(), 1)
  expect_type(featurelist, "list")
  ExpectHasKeys(featurelist, c(
    "projectId", "features", "name", "featurelistId",
    "created", "isUserCreated", "numModels", "description"
  ))
})

test_that("it can create a modeling featurelist", {
  postStub <- stub(httr::POST)
  getFeaturelistsUrl <- UrlJoin(projectUrl, "featurelists", fakeFeaturelistId)
  getFeaturelistsJson <- fileToChar("responses/GetModelingFeaturelist.json")
  featurelistsResponse <- httr:::response(
    url = getFeaturelistsUrl,
    status_code = 200L,
    content = charToRaw(getFeaturelistsJson)
  )
  postStub$onCall(1)$returns(featurelistsResponse)
  featurelist <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    CreateModelingFeaturelist(
      fakeProject,
      fakeFeaturelistName,
      fakeFeatures
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(featurelist, "list")
  expect_equal(
    sort(names(featurelist)),
    sort(c("projectId", "features", "name", "featurelistId"))
  )
  expect_true(any(sapply(featurelist$features, function(f) grepl("lag", f))))
  expect_true(any(sapply(featurelist$features, function(f) grepl("actual", f))))
  expect_true(any(sapply(featurelist$features, function(f) grepl("seasonal", f))))
})


test_that("it can get a featurelist", {
  getStub <- stub(httr::GET)
  getFeaturelistsUrl <- UrlJoin(projectUrl, "featurelists", fakeFeaturelistId)
  getFeaturelistsJson <- fileToChar("responses/GetFeaturelist.json")
  featurelistsResponse <- httr:::response(
    url = getFeaturelistsUrl,
    status_code = 200L,
    content = charToRaw(getFeaturelistsJson)
  )
  getStub$onCall(1)$returns(featurelistsResponse)
  featurelist <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetFeaturelist(fakeProject, fakeFeaturelistId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(featurelist, "list")
  ExpectHasKeys(featurelist, c(
    "projectId", "features", "name", "featurelistId",
    "created", "isUserCreated", "numModels", "description"
  ))
})


test_that("it can get a modeling featurelist", {
  getStub <- stub(httr::GET)
  listFeaturelistsUrl <- UrlJoin(projectUrl, "modelingFeaturelists", fakeFeaturelistId)
  listFeaturelistsJson <- fileToChar("responses/GetModelingFeaturelist.json")
  featurelistsResponse <- httr:::response(
    url = listFeaturelistsUrl,
    status_code = 200L,
    content = charToRaw(listFeaturelistsJson)
  )
  getStub$onCall(1)$returns(featurelistsResponse)
  featurelist <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetModelingFeaturelist(fakeProject, fakeFeaturelistId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(featurelist, "list")
  expect_equal(
    sort(names(featurelist)),
    sort(c("projectId", "features", "name", "featurelistId"))
  )
  expect_true(any(sapply(featurelist$features, function(f) grepl("lag", f))))
  expect_true(any(sapply(featurelist$features, function(f) grepl("actual", f))))
  expect_true(any(sapply(featurelist$features, function(f) grepl("seasonal", f))))
})


test_that("it can get a list of featurelists", {
  getStub <- stub(httr::GET)
  listFeaturelistsUrl <- UrlJoin(projectUrl, "featurelists")
  listFeaturelistsJson <- fileToChar("responses/ListFeaturelists.json")
  featurelistsResponse <- httr:::response(
    url = listFeaturelistsUrl,
    status_code = 200L,
    content = charToRaw(listFeaturelistsJson)
  )
  getStub$onCall(1)$returns(featurelistsResponse)
  featurelists <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListFeaturelists(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(featurelists, c("listOfFeaturelists", "listSubclass"))
  ExpectHasKeys(featurelists[[1]], c(
    "projectId", "features", "name", "featurelistId",
    "created", "isUserCreated", "numModels", "description"
  ))
})


test_that("it can summarize featurelists", {
  getStub <- stub(httr::GET)
  listFeaturelistsUrl <- UrlJoin(projectUrl, "featurelists")
  listFeaturelistsJson <- fileToChar("responses/ListFeaturelists.json")
  featurelistsResponse <- httr:::response(
    url = listFeaturelistsUrl,
    status_code = 200L,
    content = charToRaw(listFeaturelistsJson)
  )
  getStub$onCall(1)$returns(featurelistsResponse)
  featurelistSummary <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    summary(ListFeaturelists(fakeProject))
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(featurelistSummary, "list")
  ExpectHasKeys(featurelistSummary, c("generalSummary", "detailedSummary"))
  expect_type(featurelistSummary$generalSummary, "character")
  expect_s3_class(featurelistSummary$detailedSummary, "data.frame")
  ExpectHasKeys(
    featurelistSummary$detailedSummary,
    c(
      "description", "created", "name", "isUserCreated", "numModels",
      "features", "featurelistId"
    )
  )
  expect_equal(nrow(featurelistSummary$detailedSummary), 58)
})


test_that("it can as.data.frame list of featurelists", {
  getStub <- stub(httr::GET)
  listFeaturelistsUrl <- UrlJoin(projectUrl, "featurelists")
  listFeaturelistsJson <- fileToChar("responses/ListFeaturelists.json")
  featurelistsResponse <- httr:::response(
    url = listFeaturelistsUrl,
    status_code = 200L,
    content = charToRaw(listFeaturelistsJson)
  )
  getStub$onCall(1)$returns(featurelistsResponse)
  featurelistDf <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    as.data.frame(ListFeaturelists(fakeProject))
  )
  expect_s3_class(featurelistDf, "data.frame")
  expect_equal(nrow(featurelistDf), 58)
  ExpectHasKeys(
    featurelistDf,
    c(
      "description", "created", "projectId", "name", "isUserCreated",
      "numModels", "features", "featurelistId"
    )
  )
})


test_that("it can get a list of modeling featurelists", {
  getStub <- stub(httr::GET)
  listFeaturelistsUrl <- UrlJoin(projectUrl, "modelingFeaturelists")
  listFeaturelistsJson <- fileToChar("responses/ListModelingFeaturelists.json")
  featurelistsResponse <- httr:::response(
    url = listFeaturelistsUrl,
    status_code = 200L,
    content = charToRaw(listFeaturelistsJson)
  )
  getStub$onCall(1)$returns(featurelistsResponse)
  featurelists <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListModelingFeaturelists(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(featurelists, c("listOfFeaturelists", "listSubclass"))
  expect_equal(
    sort(names(featurelists[[1]])),
    sort(c("projectId", "features", "name", "featurelistId"))
  )
  expect_true(any(sapply(featurelists[[1]]$features, function(f) grepl("lag", f))))
  expect_true(any(sapply(featurelists[[1]]$features, function(f) grepl("actual", f))))
  expect_true(any(sapply(featurelists[[1]]$features, function(f) grepl("seasonal", f))))
})


test_that("it can update a featurelist", {
  getStub <- stub(httr::GET)
  getFeaturelistsUrl <- UrlJoin(projectUrl, "featurelists", fakeFeaturelistId)
  getFeaturelistsJson <- fileToChar("responses/GetFeaturelist.json")
  featurelistsResponse <- httr:::response(
    url = getFeaturelistsUrl,
    status_code = 200L,
    content = charToRaw(getFeaturelistsJson)
  )
  getStub$onCall(1)$returns(featurelistsResponse)
  featurelist <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::DataRobotPATCH" = function(...) {
      paramsForInspect <<- list(...)
      "NOOP"
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UpdateFeaturelist(fakeFeaturelist, description = "bob")
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(paramsForInspect$body$description, "bob")
  expect_type(featurelist, "list")
  ExpectHasKeys(featurelist, c(
    "projectId", "features", "name", "featurelistId",
    "created", "isUserCreated", "numModels", "description"
  ))
})

test_that("it can update a modeling featurelist", {
  getStub <- stub(httr::GET)
  getFeaturelistsUrl <- UrlJoin(projectUrl, "modelingFeaturelists", fakeFeaturelistId)
  getFeaturelistsJson <- fileToChar("responses/GetFeaturelist.json")
  featurelistsResponse <- httr:::response(
    url = getFeaturelistsUrl,
    status_code = 200L,
    content = charToRaw(getFeaturelistsJson)
  )
  getStub$onCall(1)$returns(featurelistsResponse)
  featurelist <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::DataRobotPATCH" = function(...) {
      paramsForInspect <<- list(...)
      "NOOP"
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UpdateModelingFeaturelist(fakeFeaturelist, description = "bob")
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(paramsForInspect$body$description, "bob")
  expect_type(featurelist, "list")
  ExpectHasKeys(featurelist, c(
    "projectId", "features", "name", "featurelistId",
    "created", "isUserCreated", "numModels", "description"
  ))
})


test_that("it can delete a featurelist", {
  deleteStub <- stub(httr::DELETE)
  deleteFeaturelistsUrl <- UrlJoin(projectUrl, "featurelists", fakeFeaturelistId)
  deleteResponse <- httr:::response(
    url = deleteFeaturelistsUrl,
    status_code = 204L,
    content = raw(0)
  )
  deleteStub$onCall(1)$returns(deleteResponse)
  flist <- with_mock(
    "httr::DELETE" = deleteStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DeleteFeaturelist(fakeFeaturelist)
  )
  expect_null(flist)
})

test_that("it can delete a modeling featurelist", {
  deleteStub <- stub(httr::DELETE)
  deleteFeaturelistsUrl <- UrlJoin(projectUrl, "modelingFeaturelists", fakeFeaturelistId)
  deleteResponse <- httr:::response(
    url = deleteFeaturelistsUrl,
    status_code = 204L,
    content = raw(0)
  )
  deleteStub$onCall(1)$returns(deleteResponse)
  flist <- with_mock(
    "httr::DELETE" = deleteStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DeleteModelingFeaturelist(fakeFeaturelist)
  )
  expect_null(flist)
})
