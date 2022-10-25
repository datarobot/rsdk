# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(stubthat)

datasetsEndpoint <- datarobot:::UrlJoin(
  fakeEndpoint, "projects", fakeProjectId,
  "predictionDatasets"
)
datasetEndpoint <- datarobot:::UrlJoin(datasetsEndpoint, fakeDatasetId)
uploadEndpoint <- datarobot:::UrlJoin(datasetsEndpoint, "fileUploads")

oneDatasetEndpoint <- datarobot:::UrlJoin(datasetsEndpoint, fakeDatasetId)
statusUrl <- datarobot:::UrlJoin(fakeEndpoint, "status", "some-status")

datasetJson <- sprintf(
  '{"id": "%s", "projectId": "%s", "name": "My Dataset", "created": "2016-02-16T12:00:00.123456Z",
  "numRows": 100, "numColumns": 20, "forecastPoint": null, "predictionsStartDate": null,
  "predictionsEndDate": null}',
  fakeDatasetId, fakeProjectId
)
datasetListJson <- sprintf(
  '{"count": 1, "next": null, "previous": null, "data": [%s]}',
  datasetJson
)

datasetEmptyListJson <- sprintf('{"count": 1, "next": null, "previous": null, "data": []}')

forecastDatasetJson <- sprintf(
  '{"id": "%s", "projectId": "%s", "name": "My Dataset", "created": "2016-02-16T12:00:00.123456Z",
  "numRows": 100, "numColumns": 20, "forecastPoint": "2017-01-01T15:00:00Z",
  "predictionsStartDate": null, "predictionsEndDate": null}',
  fakeDatasetId, fakeProjectId
)

startEndDatasetJson <- sprintf(
  '{"id": "%s", "projectId": "%s", "name": "My Dataset", "created": "2016-02-16T12:00:00.123456Z",
  "numRows": 100, "numColumns": 20, "forecastPoint": null,
  "predictionsStartDate": "2017-01-01T00:00:00Z", "predictionsEndDate": "2017-03-01T00:00:00Z"}',
  fakeDatasetId, fakeProjectId
)

msForecastDatasetJson <- sprintf(
  '{"id": "%s", "projectId": "%s", "name": "Millisecond Dataset",
  "created": "2016-02-16T12:00:00.123456Z",
  "numRows": 100, "numColumns": 20, "forecastPoint": "2017-01-01T15:00:00.800000Z",
  "predictionsStartDate": null, "predictionsEndDate": null}',
  fakeDatasetId, fakeProjectId
)


listResponse <- httr:::response(
  url = datasetsEndpoint,
  status_code = 200L,
  content = charToRaw(datasetListJson)
)

getResponse <- httr:::response(
  url = datasetsEndpoint,
  status_code = 200L,
  content = charToRaw(datasetJson)
)

emptyListResponse <- httr:::response(
  url = datasetsEndpoint,
  status_code = 200L,
  content = charToRaw(datasetEmptyListJson)
)

postResponse <- httr:::response(
  url = datasetsEndpoint,
  status_code = 202L,
  headers = list(location = statusUrl),
  content = raw(0)
)

deleteResponse <- httr:::response(
  url = oneDatasetEndpoint,
  status_code = 204,
  content = raw(0)
)

expectedKeys <- c(
  "id", "projectId", "name", "created", "numRows", "numColumns", "forecastPoint",
  "predictionsStartDate", "predictionsEndDate"
)

test_that("delete", {
  deleteStub <- stub(httr::DELETE)
  deleteStub$expects(url = oneDatasetEndpoint)
  deleteStub$returns(deleteResponse)
  testReturn <- with_mock(
    "httr::DELETE" = deleteStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DeletePredictionDataset(fakeProject, fakeDatasetId)
  )
  expect_null(testReturn)
})

test_that("list", {
  getStub <- stub(httr::GET)
  getStub$expects(url = datasetsEndpoint)
  getStub$returns(listResponse)
  datasetList <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListPredictionDatasets(fakeProject)
  )
  expect_equal(length(datasetList), 1)
  expect_s3_class(datasetList[[1]], "dataRobotPredictionDataset")
  ExpectHasKeys(datasetList[[1]], expectedKeys)
})

test_that("empty list", {
  getStub <- stub(httr::GET)
  getStub$expects(url = datasetsEndpoint)
  getStub$returns(emptyListResponse)
  datasetList <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListPredictionDatasets(fakeProject)
  )
  expect_equal(length(datasetList), 0)
})

test_that("can call as.data.frame on list", {
  getStub <- stub(httr::GET)
  getStub$expects(url = datasetsEndpoint)
  getStub$returns(listResponse)
  datasetList <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListPredictionDatasets(fakeProject)
  )
  datasetDf <- as.data.frame(datasetList)
  expect_s3_class(datasetDf, "data.frame")
  expect_equal(nrow(datasetDf), 1)
  ExpectHasKeys(datasetDf, expectedKeys)
})

test_that("get one", {
  getStub <- stub(httr::GET)
  getStub$expects(url = datasetEndpoint)
  getStub$returns(getResponse)
  dataset <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictionDataset(fakeProject, fakeDatasetId)
  )
  expect_s3_class(dataset, "dataRobotPredictionDataset")
  ExpectHasKeys(dataset, expectedKeys)
})

test_that("upload", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$expects(url = uploadEndpoint)
  postStub$onCall(1)$returns(httr:::response(
    url = uploadEndpoint,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  ))
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = oneDatasetEndpoint),
    content = raw(0)
  ))
  getStub$onCall(2)$expects(url = oneDatasetEndpoint)
  getStub$onCall(2)$returns(httr:::response(
    url = oneDatasetEndpoint,
    status_code = 200L,
    content = charToRaw(datasetJson)
  ))
  dataset <- with_mock(
    "httr::GET" = getStub$f,
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UploadPredictionDataset(fakeProject, "Boston_autoSavedDF.csv")
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 2)
  expect_s3_class(dataset, "dataRobotPredictionDataset")
  ExpectHasKeys(dataset, expectedKeys)
})

test_that("upload time series with forecastPoint", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$expects(url = uploadEndpoint)
  postStub$onCall(1)$returns(httr:::response(
    url = uploadEndpoint,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  ))
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = oneDatasetEndpoint),
    content = raw(0)
  ))
  getStub$onCall(2)$expects(url = oneDatasetEndpoint)
  getStub$onCall(2)$returns(httr:::response(
    url = oneDatasetEndpoint,
    status_code = 200L,
    content = charToRaw(forecastDatasetJson)
  ))
  dataset <- with_mock(
    "httr::GET" = getStub$f,
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UploadPredictionDataset(fakeProject,
      "Boston_autoSavedDF.csv",
      forecastPoint = "2017-01-01T15:00:00Z"
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 2)
  expect_s3_class(dataset, "dataRobotPredictionDataset")
  ExpectHasKeys(dataset, expectedKeys)
})

test_that("upload time series with forecastPoint - millisecond resolution", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$expects(url = uploadEndpoint)
  postStub$onCall(1)$returns(httr:::response(
    url = uploadEndpoint,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  ))
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = oneDatasetEndpoint),
    content = raw(0)
  ))
  getStub$onCall(2)$expects(url = oneDatasetEndpoint)
  getStub$onCall(2)$returns(httr:::response(
    url = oneDatasetEndpoint,
    status_code = 200L,
    content = charToRaw(msForecastDatasetJson)
  ))
  dataset <- with_mock(
    "httr::GET" = getStub$f,
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UploadPredictionDataset(fakeProject,
      "Boston_autoSavedDF.csv",
      forecastPoint = "2017-01-01T15:00:00.800000Z"
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 2)
  expect_s3_class(dataset, "dataRobotPredictionDataset")
  ExpectHasKeys(dataset, expectedKeys)
})

test_that("upload time series with forecastPoint", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$expects(url = uploadEndpoint)
  postStub$onCall(1)$returns(httr:::response(
    url = uploadEndpoint,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  ))
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = oneDatasetEndpoint),
    content = raw(0)
  ))
  getStub$onCall(2)$expects(url = oneDatasetEndpoint)
  getStub$onCall(2)$returns(httr:::response(
    url = oneDatasetEndpoint,
    status_code = 200L,
    content = charToRaw(startEndDatasetJson)
  ))
  dataset <- with_mock(
    "httr::GET" = getStub$f,
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UploadPredictionDataset(fakeProject,
      "Boston_autoSavedDF.csv",
      predictionsStartDate = "2017-01-01",
      predictionsEndDate = "2017-03-01"
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 2)
  expect_s3_class(dataset, "dataRobotPredictionDataset")
  ExpectHasKeys(dataset, expectedKeys)
})


test_that("You can't have both a forecast point and a prediction date", {
  expect_error(
    UploadPredictionDataset(fakeProject, "Boston_autoSavedDF.csv",
      forecastPoint = "2017-01-01",
      predictionsStartDate = "2017-01-01"
    ),
    "cannot be provided along with"
  )
})

test_that("You need both a predictionStartDate and a predictionEndDate", {
  expect_error(
    UploadPredictionDataset(fakeProject, "Boston_autoSavedDF.csv",
      predictionsStartDate = "2017-01-01"
    ),
    "You must specify"
  )
})

test_that("upload raises appropriate exception for failure", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$expects(url = uploadEndpoint)
  postStub$onCall(1)$returns(httr:::response(
    url = uploadEndpoint,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  ))
  jobFailureJson <- '{"status": "ERROR", "message": "some failure message"}'
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 200L,
    content = charToRaw(jobFailureJson)
  ))
  with_mock(
    "httr::GET" = getStub$f,
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    expect_error(
      UploadPredictionDataset(fakeProject, "Boston_autoSavedDF.csv"),
      "PendingJobFailed"
    )
  )
})

test_that("maxWait parameter is passed to WaitForAsyncReturn", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$expects(url = uploadEndpoint)
  postStub$onCall(1)$returns(httr:::response(
    url = uploadEndpoint,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  ))
  maxWaitToUse <- 2
  with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::WaitForAsyncReturn" = function(...) {
      expect_equal(list(...)$maxWait, maxWaitToUse)
      charToRaw(datasetJson)
    },
    UploadPredictionDataset(fakeProject, "Boston_autoSavedDF.csv", maxWait = maxWaitToUse)
  )
})

uploadEndpoint <- datarobot:::UrlJoin(datasetsEndpoint, "urlUploads")
test_that("upload from URL", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$expects(url = uploadEndpoint)
  postStub$onCall(1)$returns(httr:::response(
    url = uploadEndpoint,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  ))
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = oneDatasetEndpoint),
    content = raw(0)
  ))
  getStub$onCall(2)$expects(url = oneDatasetEndpoint)
  getStub$onCall(2)$returns(httr:::response(
    url = oneDatasetEndpoint,
    status_code = 200L,
    content = charToRaw(datasetJson)
  ))
  dataset <- with_mock(
    "httr::GET" = getStub$f,
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UploadPredictionDataset(fakeProject, "http://Boston_autoSavedDF.csv")
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 2)
  expect_s3_class(dataset, "dataRobotPredictionDataset")
  ExpectHasKeys(dataset, expectedKeys)
})

uploadEndpoint <- datarobot:::UrlJoin(datasetsEndpoint, "dataSourceUploads")
test_that("upload from data source", {
  postResponse <- httr:::response(
    url = datasetsEndpoint,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  )
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(postResponse)
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = oneDatasetEndpoint),
    content = raw(0)
  ))
  getStub$onCall(2)$expects(url = oneDatasetEndpoint)
  getStub$onCall(2)$returns(httr:::response(
    url = oneDatasetEndpoint,
    status_code = 200L,
    content = charToRaw(datasetJson)
  ))
  dataset <- with_mock(
    "httr::GET" = getStub$f,
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UploadPredictionDatasetFromDataSource(
      fakeProject,
      fakeDataSourceId,
      fakeUsername,
      fakePassword
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 2)
  expect_s3_class(dataset, "dataRobotPredictionDataset")
  ExpectHasKeys(dataset, expectedKeys)
})

uploadEndpoint <- datarobot:::UrlJoin(datasetsEndpoint, "datasetUploads")
test_that("upload from dataset", {
  postResponse <- httr:::response(
    url = datasetsEndpoint,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  )
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(postResponse)
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = oneDatasetEndpoint),
    content = raw(0)
  ))
  getStub$onCall(2)$expects(url = oneDatasetEndpoint)
  getStub$onCall(2)$returns(httr:::response(
    url = oneDatasetEndpoint,
    status_code = 200L,
    content = charToRaw(datasetJson)
  ))
  dataset <- with_mock(
    "httr::GET" = getStub$f,
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UploadPredictionDatasetFromCatalog(fakeProject, fakeDataSourceId)
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 2)
  expect_s3_class(dataset, "dataRobotPredictionDataset")
  ExpectHasKeys(dataset, expectedKeys)
})

test_that("upload to URL with relaxKIAFeaturesCheck", {
  postResponse <- httr:::response(
    url = datasetsEndpoint,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  )
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(postResponse)
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = oneDatasetEndpoint),
    content = raw(0)
  ))
  getStub$onCall(2)$expects(url = oneDatasetEndpoint)
  getStub$onCall(2)$returns(httr:::response(
    url = oneDatasetEndpoint,
    status_code = 200L,
    content = charToRaw(datasetJson)
  ))
  dataset <- with_mock(
    "httr::GET" = getStub$f,
    "httr::POST" = postStub$f,
    "datarobot:::DataRobotPOST" = function(routeString,
                                           addUrl = TRUE,
                                           body = NULL,
                                           returnRawResponse = FALSE, ...) {
      bodyForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::POST, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UploadPredictionDataset(fakeProject,
      "http://Boston_autoSavedDF.csv",
      relaxKIAFeaturesCheck = TRUE
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 2)
  expect_s3_class(dataset, "dataRobotPredictionDataset")
  ExpectHasKeys(dataset, expectedKeys)
  expect_equal(bodyForInspect$url, "http://Boston_autoSavedDF.csv")
  expect_true(bodyForInspect$relaxKIAFeaturesCheck)
})

test_that("upload to data source with relaxKIAFeaturesCheck", {
  postResponse <- httr:::response(
    url = datasetsEndpoint,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  )
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(postResponse)
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = oneDatasetEndpoint),
    content = raw(0)
  ))
  getStub$onCall(2)$expects(url = oneDatasetEndpoint)
  getStub$onCall(2)$returns(httr:::response(
    url = oneDatasetEndpoint,
    status_code = 200L,
    content = charToRaw(datasetJson)
  ))
  dataset <- with_mock(
    "httr::GET" = getStub$f,
    "httr::POST" = postStub$f,
    "datarobot:::DataRobotPOST" = function(routeString,
                                           addUrl = TRUE,
                                           body = NULL,
                                           returnRawResponse = FALSE, ...) {
      bodyForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::POST, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UploadPredictionDatasetFromDataSource(fakeProject,
      fakeDataSourceId,
      fakeUsername,
      fakePassword,
      relaxKIAFeaturesCheck = TRUE
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 2)
  expect_s3_class(dataset, "dataRobotPredictionDataset")
  ExpectHasKeys(dataset, expectedKeys)
  expect_equal(bodyForInspect$dataSourceId, fakeDataSourceId)
  expect_equal(bodyForInspect$user, fakeUsername)
  expect_equal(bodyForInspect$password, fakePassword)
  expect_true(bodyForInspect$relaxKIAFeaturesCheck)
})


datasetWithErrorsJson <- sprintf(
  '{"id": "%s", "projectId": "%s", "name": "My Dataset", "created": "2016-02-16T12:00:00.123456Z",
  "numRows": 100, "numColumns": 20, "forecastPoint": null, "predictionsStartDate": null,
  "predictionsEndDate": null,
  "dataQualityWarnings": {"hasKIAMissingValuesInForecastWindow": false}}',
  fakeDatasetId, fakeProjectId
)
getResponse <- httr:::response(
  url = datasetsEndpoint,
  status_code = 200L,
  content = charToRaw(datasetWithErrorsJson)
)
test_that("dataQualityWarnings are returned", {
  getStub <- stub(httr::GET)
  getStub$expects(url = datasetEndpoint)
  getStub$returns(getResponse)
  dataset <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictionDataset(fakeProject, fakeDatasetId)
  )
  expect_s3_class(dataset, "dataRobotPredictionDataset")
  expect_false(is.null(dataset$dataQualityWarnings))
  expect_type(dataset$dataQualityWarnings, "list")
  expect_false(dataset$dataQualityWarnings$hasKIAMissingValuesInForecastWindow)
})
