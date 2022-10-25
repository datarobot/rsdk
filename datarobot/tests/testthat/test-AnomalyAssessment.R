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

projectId <- "5ece5927962d741e19c2febb"
recordId <- "607d657551a4bef68dd29d2f"
modelId <- "607d6544823fa548132dc257"

InitializeAnomalyAssessmentUrl <- UrlJoin(
  "projects", projectId, "models",
  modelId, "anomalyAssessmentInitialization"
)
GetAnomalyAssessmentPredictionsPreviewUrl <- UrlJoin(
  "projects", projectId,
  "anomalyAssessmentRecords",
  recordId, "predictionsPreview"
)
GetAnomalyAssessmentExplanationsUrl <- UrlJoin(
  "projects", projectId,
  "anomalyAssessmentRecords", recordId, "explanations"
)
ListAnomalyAssessmentRecordsUrl <- UrlJoin(
  "projects", projectId,
  "anomalyAssessmentRecords"
)
DeleteAnomalyAssessmentRecordUrl <- UrlJoin(
  "projects", projectId, "anomalyAssessmentRecords",
  recordId
)

test_that("can get list of anomaly assessment records", {
  getStub <- stub(httr::GET)
  listAnomalyAssessmentRecordsJson <- fileToChar("responses/anomalyAssessmentRecords.json")
  listRecordsResponse <- httr:::response(
    url = ListAnomalyAssessmentRecordsUrl,
    status_code = 200L,
    content = charToRaw(listAnomalyAssessmentRecordsJson)
  )
  getStub$onCall(1)$returns(listRecordsResponse)
  records <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListAnomalyAssessmentRecords(projectId,
      modelId,
      backtest = 0
    )
  )
  expect_equal(getStub$calledTimes(), 1)

  expectedCols <- c(
    "status", "seriesId", "endDate", "startDate", "recordId", "projectId",
    "backtest", "source", "latestExplanationsLocation", "deleteLocation",
    "predictionThreshold", "statusDetails", "previewLocation", "modelId"
  )
  expect_type(records, "list")
  noDataRecord <- records[[1]]
  expect_s3_class(noDataRecord, "dataRobotAnomalyAssessmentRecord")
  completedRecord <- records[[2]]
  expect_equal(
    completedRecord$startDate,
    datarobot.apicore::ParseRFC3339Timestamp("2015-10-07T00:00:00.000000Z")
  )
  expect_equal(
    completedRecord$endDate,
    datarobot.apicore::ParseRFC3339Timestamp("2015-11-02T00:00:00.000000Z")
  )
  lapply(records, ExpectHasKeys, keys = expectedCols)
})


test_that("can get anomaly assessment prediction preview", {
  getStub <- stub(httr::GET)
  previewsJson <- fileToChar("responses/anomalyAssessmentPredictionsPreview.json")
  previewsResponse <- httr:::response(
    url = GetAnomalyAssessmentPredictionsPreviewUrl,
    status_code = 200L,
    content = charToRaw(previewsJson)
  )
  getStub$onCall(1)$returns(previewsResponse)
  previews <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetAnomalyAssessmentPredictionsPreview(projectId, recordId)
  )
  expect_equal(getStub$calledTimes(), 1)

  expectedCols <- c(
    "seriesId", "endDate", "startDate", "recordId", "projectId",
    "backtest", "source", "previewBins", "modelId"
  )
  expect_s3_class(previews, "dataRobotAnomalyAssessmentPredictionsPreview")
  expect_equal(
    previews$startDate,
    datarobot.apicore::ParseRFC3339Timestamp("2015-10-07T00:00:00.000000Z")
  )
  expect_equal(
    previews$endDate,
    datarobot.apicore::ParseRFC3339Timestamp("2015-11-02T00:00:00.000000Z")
  )
  ExpectHasKeys(previews, expectedCols)
  firstRow <- previews$previewBins[[1]]
  expect_s3_class(firstRow, "dataRobotPreviewBin")
  expect_equal(
    firstRow$startDate,
    datarobot.apicore::ParseRFC3339Timestamp("2015-10-07T00:00:00.000000Z")
  )
  expect_equal(firstRow$avgPredicted, 0.43967850596755503)
})


test_that("can get anomaly assessment record explanations", {
  getStub <- stub(httr::GET)
  explanationsJson <- fileToChar("responses/anomalyAssessmentExplanations.json")
  explResponse <- httr:::response(
    url = GetAnomalyAssessmentExplanationsUrl,
    status_code = 200L,
    content = charToRaw(explanationsJson)
  )
  getStub$onCall(1)$returns(explResponse)
  explanations <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetAnomalyAssessmentExplanations(projectId, recordId)
  )
  expect_equal(getStub$calledTimes(), 1)

  expectedCols <- c(
    "count", "shapBaseValue", "endDate", "startDate", "recordId", "projectId",
    "backtest", "source", "seriesId", "data", "modelId"
  )
  expect_s3_class(explanations, "dataRobotAnomalyAssessmentExplanations")
  expect_equal(explanations$startDate, datarobot.apicore::ParseRFC3339Timestamp(
    "2015-10-31T00:00:00.000000Z"
  ))
  expect_equal(explanations$endDate, datarobot.apicore::ParseRFC3339Timestamp("2015-11-02T00:00:00.000000Z"))
  ExpectHasKeys(explanations, expectedCols)
  firstRow <- explanations$data[[1]]
  expect_s3_class(firstRow, "dataRobotPredictionWithExplanationsRow")
  expect_equal(firstRow$timestamp, datarobot.apicore::ParseRFC3339Timestamp("2015-10-31T00:00:00.000000Z"))
  firstExplanation <- firstRow$shapExplanation[[1]]
  expect_s3_class(firstExplanation, "dataRobotShapFeatureContribution")
})


test_that("compute anomaly assessment", {
  postStub <- stub(httr::POST)
  createRecordJson <- fileToChar("responses/anomalyAssessmentRecords.json")
  createRecordResponse <- httr:::response(
    url = InitializeAnomalyAssessmentUrl,
    status_code = 202L,
    content = charToRaw(createRecordJson)
  )
  postStub$onCall(1)$returns(createRecordResponse)

  record <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot::WaitForAsyncReturn" = function(...) {
      jsonlite::fromJSON(createRecordJson)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    InitializeAnomalyAssessment(projectId, modelId,
      source = "training",
      backtest = 0
    )
  )
  expect_equal(postStub$calledTimes(), 1)
})
