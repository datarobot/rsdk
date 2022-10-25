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

transferableJobJson <- fileToChar("responses/transferableJob.json")
completedJobResponse <- httr:::response(
  url = jobUrl,
  status_code = 200L,
  content = charToRaw(transferableJobJson)
)
modelExportUrl <- UrlJoin(projectUrl, "modelExports")
completedModelExportResponse <- httr:::response(
  url = modelExportUrl,
  status_code = 202L,
  headers = list(location = modelExportUrl),
  content = raw(0)
)

test_that("RequestTransferableModel succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedJobResponse)
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(completedModelExportResponse)
  jobId <- with_mock(
    "httr::GET" = getStub$f,
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RequestTransferableModel(fakeProject, fakeModelId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(postStub$calledTimes(), 1)
  expect_type(jobId, "character")
})

test_that("RequestTransferableModel sets percentile parameter when needed", {
  # DSX-870 adds prediction intervals support for supervised ts model exports
  jobId <- with_mock(
    "datarobot:::DataRobotGET" = function(...) completedJobResponse,
    "datarobot:::DataRobotPOST" = function(body = NULL, ...) {
      bodyToInspect <<- body
      completedModelExportResponse
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RequestTransferableModel(fakeProject,
      fakeModelId,
      predictionIntervalSize = 50
    )
  )
  expect_equal(bodyToInspect$percentile, 50)
})

fakeFilePath <- "fake_Transferable_model.drmodel"
transferableFilestUrl <- UrlJoin(projectUrl, "models", fakeModelId, "export")
completedFilesResponse <- httr:::response(
  url = transferableFilestUrl,
  status_code = 200L,
  content = raw(100)
)
test_that("DownloadTransferableModel succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedFilesResponse)
  primeCode <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DownloadTransferableModel(fakeProject, fakeModelId, fakeFilePath)
  )
  expect_equal(getStub$calledTimes(), 1)
})


datasetsEndpoint <- datarobot:::UrlJoin(fakeEndpoint, "importedModels", fakeModelId)
statusUrl <- datarobot:::UrlJoin(fakeEndpoint, "status", "some-status")
postResponse <- httr:::response(
  url = datasetsEndpoint,
  status_code = 202L,
  headers = list(location = statusUrl),
  content = raw(0)
)

transferableModelJson <- fileToChar("responses/transferredModel.json")
uploadEndpoint <- datarobot:::UrlJoin(fakeEndpoint, "importedModels")
expectedKeys <- c(
  "note", "datasetName", "modelName", "displayName", "target",
  "projectName", "importedByUsername", "originUrl", "importedAt",
  "version", "projectId", "featurelistName", "createdByUsername",
  "importedById", "id", "createdById", "modelId"
)

test_that("UploadTransferableModel succeeds", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$expects(url = uploadEndpoint)
  postStub$onCall(1)$returns(postResponse)
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 200L,
    content = charToRaw('{"status": "someStatus"}')
  ))
  getStub$onCall(2)$expects(url = statusUrl)
  getStub$onCall(2)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = datasetsEndpoint),
    content = raw(0)
  ))
  getStub$onCall(3)$expects(url = datasetsEndpoint)
  getStub$onCall(3)$returns(httr:::response(
    url = datasetsEndpoint,
    status_code = 200L,
    content = charToRaw(transferableModelJson)
  ))
  tranferredM <- with_mock(
    "httr::GET" = getStub$f,
    "httr::POST" = postStub$f,
    "httr::upload_file" = function(modelFile) "some_file",
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UploadTransferableModel("some_file")
  )
  expect_type(tranferredM, "list")
  expect_equal(sort(expectedKeys), sort(names(tranferredM)))
})


importId <- "fake_inportId"
completedModelResponse <- httr:::response(
  url = datasetsEndpoint,
  status_code = 200L,
  content = charToRaw(transferableModelJson)
)
test_that("GetTransferableModel succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedModelResponse)
  transferableModel <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetTransferableModel(importId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(transferableModel, "list")
  expect_equal(sort(expectedKeys), sort(names(transferableModel)))
  expect_type(transferableModel$id, "character")
  expect_type(transferableModel$version, "double")
  expect_type(transferableModel$target, "character")
  expect_type(transferableModel$datasetName, "character")
})


transferableModelJson <- fileToChar("responses/transferredModels.json")
completedModelResponse <- httr:::response(
  url = transferableFilestUrl,
  status_code = 200L,
  content = charToRaw(transferableModelJson)
)
test_that("GetTransferableModel succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedModelResponse)
  transferableModel <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListTransferableModels()
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(transferableModel, "data.frame")
  expect_equal(sort(expectedKeys), sort(names(transferableModel)))
  expect_type(transferableModel$id, "character")
  expect_type(transferableModel$version, "double")
  expect_type(transferableModel$target, "character")
  expect_type(transferableModel$datasetName, "character")
})


importId <- "fake_importId"
transferableFilestUrl <- UrlJoin(projectUrl, "importedModels", fakeModelId)
transferableModelJson <- fileToChar("responses/transferredModel.json")
completedModelResponse1 <- httr:::response(
  url = transferableFilestUrl,
  status_code = 200L,
  content = charToRaw(transferableModelJson)
)
completedModelResponse2 <- httr:::response(
  url = transferableFilestUrl,
  status_code = 200L,
  content = raw(0)
)
test_that("UpdateTransferableModel succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedModelResponse1)
  patchStub <- stub(httr::PATCH)
  patchStub$onCall(1)$returns(completedModelResponse2)

  transferableModel <- with_mock(
    "httr::GET" = getStub$f,
    "httr::PATCH" = patchStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UpdateTransferableModel(importId,
      displayName = "new name",
      note = "new note"
    )
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(transferableModel, "list")
  expect_equal(sort(expectedKeys), sort(names(transferableModel)))
  expect_type(transferableModel$id, "character")
  expect_type(transferableModel$version, "double")
  expect_type(transferableModel$target, "character")
  expect_type(transferableModel$datasetName, "character")
})


test_that("Required parameters are present", {
  expect_error(DeleteTransferableModel())
})


test_that("Delete succeeds", {
  importToDelete <- "fake_import"
  testReturn <- suppressMessages(with_mock(
    "datarobot::DataRobotDELETE" = function(RouteString, AddURL, body, ...) {
      ""
    },
    "datarobot::DataRobotGET" = function(RouteString, AddURL, body, ...) {
      fileToChar("responses/transferredModel.json")
    },
    list(
      returnValue = DeleteProject(importToDelete),
      messageCheck = expect_message(
        DeleteProject(importToDelete),
        "deleted"
      )
    )
  ))
})
