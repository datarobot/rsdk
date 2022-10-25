# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(testthat)

explanationKeys <- c(
  "rowId", "prediction", "explanation1FeatureName", "explanation1FeatureValue",
  "explanation1QualitativeStrength", "explanation1Strength", "class1Label",
  "class1Probability", "class2Label", "class2Probability", "explanation1Label",
  "predictionExplanationId"
)
regressionExplanationKeys <- c(
  "rowId", "prediction", "explanation1FeatureName",
  "explanation1FeatureValue", "explanation2FeatureName",
  "explanation2FeatureValue", "explanation3FeatureName",
  "explanation3FeatureValue", "explanation2QualitativeStrength",
  "explanation3QualitativeStrength",
  "explanation1QualitativeStrength",
  "explanation1Strength", "explanation1Label",
  "explanation2Strength", "explanation2Label",
  "explanation3Strength", "explanation3Label",
  "predictionExplanationId"
)

predictionExplanationsIniUrl <- UrlJoin(
  projectUrl, "models", fakeModelId,
  "predictionExplanationsInitialization"
)


predictionExplanationsIniRequestResponse <- httr:::response(
  url = predictionExplanationsIniUrl,
  status_code = 303L,
  headers = list(location = jobUrl),
  content = raw(0)
)

test_that("RequestPredictionExplanationsInitialization succeeds with correct job ID", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(predictionExplanationsIniRequestResponse)
  postStub$expects(url = predictionExplanationsIniUrl)
  returnedJobId <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RequestPredictionExplanationsInitialization(fakeModel)
  )
  expect_equal(returnedJobId, fakeJobId)
  expect_equal(postStub$calledTimes(), 1)
})


predictionExpIniJson <- fileToChar("responses/predictionExplanationIni.json")
predictionExplanationsIniRepsonse <- httr:::response(
  url = predictionExplanationsIniUrl,
  status_code = 200L,
  content = charToRaw(predictionExpIniJson)
)

test_that("GetPredictionExplanationsInitializationFromJobId succeeds", {
  jobDataInprogress <- list(
    status = JobStatus$InProgress,
    url = "https://host_name.com/projects/p-id/jobs/1/",
    id = fakeJobId,
    jobType = JobType$PredictionExplanationsInitialization,
    projectId = fakeProjectId
  )

  jobDataComplete <- jobDataInprogress
  jobDataComplete$status <- JobStatus$Completed

  jobInprogressResponse <-
    httr:::response(
      url = predictionExplanationsIniUrl,
      status_code = 200L,
      content = charToRaw(jsonlite::toJSON(jobDataInprogress))
    )

  jobCompleteResponse <-
    httr:::response(
      url = predictionExplanationsIniUrl,
      status_code = 303L,
      headers = list(location = predictionExplanationsIniUrl),
      content = charToRaw(jsonlite::toJSON(jobDataComplete))
    )


  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(jobInprogressResponse)
  getStub$onCall(2)$returns(jobCompleteResponse)
  getStub$onCall(3)$returns(predictionExplanationsIniRepsonse)
  getStub$onCall(4)$returns(predictionExplanationsIniRepsonse)

  initialization <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictionExplanationsInitializationFromJobId(
      fakeProject,
      fakeJobId
    )
  )
  expect_equal(getStub$calledTimes(), 4)
  expect_type(initialization, "list")
  ExpectHasKeys(initialization, c("projectId", "modelId", "predictionExplanationsSample"))
  samples <- initialization$predictionExplanationsSample
  expect_type(samples, "list")
  ExpectHasKeys(samples[[1]], c(
    "predictionValues", "predictionThreshold",
    "predictionExplanations",
    "rowId", "prediction"
  ))
  expect_type(samples[[1]]$predictionValues, "list")
  ExpectHasKeys(samples[[1]]$predictionValues[[1]], c("value", "label"))
  expect_equal(samples[[1]]$predictionThreshold, 0.5)
  expect_type(samples[[1]]$predictionExplanations, "list")
  ExpectHasKeys(samples[[1]]$predictionExplanations[[1]], c(
    "featureValue", "label",
    "strength", "feature",
    "qualitativeStrength"
  ))
  expect_type(samples[[1]]$rowId, "integer")
  expect_type(samples[[1]]$prediction, "character")
})


test_that("GetPredictionExplanationsInitialization succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(predictionExplanationsIniRepsonse)
  initialization <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictionExplanationsInitialization(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(initialization, "list")
  ExpectHasKeys(initialization, c("projectId", "modelId", "predictionExplanationsSample"))
  samples <- initialization$predictionExplanationsSample
  expect_type(samples, "list")
  ExpectHasKeys(samples[[1]], c(
    "predictionValues", "predictionThreshold",
    "predictionExplanations",
    "rowId", "prediction"
  ))
  expect_type(samples[[1]]$predictionValues, "list")
  ExpectHasKeys(samples[[1]]$predictionValues[[1]], c("value", "label"))
  expect_equal(samples[[1]]$predictionThreshold, 0.5)
  expect_type(samples[[1]]$predictionExplanations, "list")
  ExpectHasKeys(samples[[1]]$predictionExplanations[[1]], c(
    "featureValue", "label",
    "strength", "feature",
    "qualitativeStrength"
  ))
  expect_type(samples[[1]]$rowId, "integer")
  expect_type(samples[[1]]$prediction, "character")
})


predictionExplanationsUrl <- UrlJoin(projectUrl, "predictionExplanations")

test_that("RequestPredictionExplanations succeeds with correct job ID", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(predictionExplanationsIniRequestResponse)
  postStub$expects(url = predictionExplanationsUrl)
  returnedJobId <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RequestPredictionExplanations(fakeModel, datasetId = "dummyDataSetId")
  )
  expect_equal(returnedJobId, fakeJobId)
  expect_equal(postStub$calledTimes(), 1)
})



predictionExplanationsMetaJson <- fileToChar("responses/predictionExplanationsMeta.json")
predictionExpMetaResponse <- httr:::response(
  url = predictionExplanationsUrl,
  status_code = 200L,
  content = charToRaw(predictionExplanationsMetaJson)
)

test_that("GetPredictionExplanationsMetadataFromJobId succeeds", {
  jobDataInprogress <- list(
    status = JobStatus$InProgress,
    url = "https://host_name.com/projects/p-id/jobs/1/",
    id = fakeJobId,
    jobType = JobType$PredictionExplanations,
    projectId = fakeProjectId
  )

  jobDataComplete <- jobDataInprogress
  jobDataComplete$status <- JobStatus$Completed

  jobInprogressResponse <-
    httr:::response(
      url = predictionExplanationsUrl,
      status_code = 200L,
      content = charToRaw(jsonlite::toJSON(jobDataInprogress))
    )

  jobCompleteResponse <-
    httr:::response(
      url = predictionExplanationsUrl,
      status_code = 303L,
      headers = list(location = predictionExplanationsUrl),
      content = charToRaw(jsonlite::toJSON(jobDataComplete))
    )

  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(jobInprogressResponse)
  getStub$onCall(2)$returns(jobCompleteResponse)
  getStub$onCall(3)$returns(predictionExpMetaResponse)
  getStub$onCall(4)$returns(predictionExpMetaResponse)

  metadata <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictionExplanationsMetadataFromJobId(fakeProject, fakeJobId)
  )
  expect_equal(getStub$calledTimes(), 4)
  expect_type(metadata, "list")
  ExpectHasKeys(metadata, c(
    "finishTime", "numColumns", "predictionExplanationsLocation",
    "projectId", "thresholdLow", "id", "thresholdHigh",
    "maxExplanations", "datasetId", "modelId"
  ))
  expect_type(metadata$numColumns, "integer")
  expect_type(metadata$maxExplanations, "integer")
})


predExpMetaListJson <- fileToChar("responses/predictionExplanationsMetaList.json")
predictionExpMetaListResponse <- httr:::response(
  url = predictionExplanationsUrl,
  status_code = 200L,
  content = charToRaw(predExpMetaListJson)
)

test_that("GetPredictionExplanationsMetadata succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(predictionExpMetaResponse)
  metadata <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictionExplanationsMetadata(fakeProject, fakeJobId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(metadata, "list")
  ExpectHasKeys(metadata, c(
    "finishTime", "numColumns", "predictionExplanationsLocation",
    "projectId", "thresholdLow", "id", "thresholdHigh",
    "maxExplanations", "datasetId", "modelId"
  ))
  expect_type(metadata$numColumns, "integer")
  expect_type(metadata$maxExplanations, "integer")
})

test_that("ListPredictionExplanationsMetadata succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(predictionExpMetaListResponse)
  metadataList <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListPredictionExplanationsMetadata(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(metadataList, "list")
  expect_type(metadataList[[1]], "list")
  ExpectHasKeys(metadataList[[1]], c(
    "finishTime", "numColumns", "predictionExplanationsLocation",
    "projectId", "thresholdLow", "id", "thresholdHigh",
    "maxExplanations", "datasetId", "modelId"
  ))
})


predictionExplanationsPageJson <- fileToChar("responses/predictionExplanationsPage.json")
predExplanationsPageResponse <- httr:::response(
  url = predictionExplanationsUrl,
  status_code = 200L,
  content = charToRaw(predictionExplanationsPageJson)
)

test_that("GetPredictionExplanationsPage succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(predExplanationsPageResponse)
  predictionExplanationsPage <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictionExplanationsPage(
      fakeProject,
      "fakePredExplainId"
    )
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(predictionExplanationsPage, "list")
  expect_type(predictionExplanationsPage$data, "list")
  ExpectHasKeys(predictionExplanationsPage, c(
    "id", "count", "data", "nextPage", "previousPage",
    "predictionExplanationsRecordLocation"
  ))
})

test_that("GetPredictionExplanationsRows returns a page", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(predExplanationsPageResponse)
  predictionExplanationsRows <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictionExplanationsRows(
      fakeProject,
      "fakePredExplainId"
    )
  )
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(predExplanationsPageResponse)
  predictionExplanationsPage <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictionExplanationsPage(
      fakeProject,
      "fakePredExplainId"
    )
  )
  expect_equal(predictionExplanationsPage$data, predictionExplanationsRows)
})

test_that("GetPredictionExplanationsRowsAsDataFrame succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(predExplanationsPageResponse)
  predFrame <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictionExplanationsRowsAsDataFrame(
      fakeProject,
      "fakePredExplainId"
    )
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_true(nrow(predFrame) > 1)
  ExpectHasKeys(predFrame, explanationKeys)
})

adjustedPredExplnsPageJson <- fileToChar("responses/predExplainsWithAdjusted.json")
adjustedPredExplnsPageResponse <- httr:::response(
  url = predictionExplanationsUrl,
  status_code = 200L,
  content = charToRaw(adjustedPredExplnsPageJson)
)

test_that("GetPredictionExplanationsPage succeeds with adjusted predictions", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(adjustedPredExplnsPageResponse)
  incPredExpPage <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictionExplanationsPage(fakeProject,
      "fakePredExplainId",
      excludeAdjustedPredictions = FALSE
    )
  )
  expect_type(incPredExpPage, "list")
  expect_type(incPredExpPage$data, "list")
  ExpectHasKeys(incPredExpPage, c(
    "id", "count", "predictionExplanationsRecordLocation",
    "adjustmentMethod", "data", "nextPage", "previousPage"
  ))
})

test_that("GetPredictionExplanationsRowsAsDataFrame succeeds with adjusted predictions", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(adjustedPredExplnsPageResponse)
  exp <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictionExplanationsRowsAsDataFrame(fakeProject,
      "fakePredExplainId",
      excludeAdjustedPredictions = FALSE
    )
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_true(nrow(exp) > 1)
  ExpectHasKeys(exp, c(
    regressionExplanationKeys, "adjustedPrediction",
    "adjustedPredictionValues"
  ))
})

test_that("GetPredictionExplanationsRowsAsDataFrame succeeds with empty dataframe", {
  getStub <- stub(httr::GET)
  predExplanationsPageResponse <- httr:::response(
    url = predictionExplanationsUrl,
    status_code = 200L,
    content = charToRaw("[]")
  )
  getStub$onCall(1)$returns(predExplanationsPageResponse)
  predFrame <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictionExplanationsRowsAsDataFrame(
      fakeProject,
      "fakePredExplainId"
    )
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(nrow(predFrame), 0)
  ExpectHasKeys(predFrame, explanationKeys)
})

test_that("DownloadPredictionExplanations succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(predExplanationsPageResponse)
  predFrame <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DownloadPredictionExplanations(
      fakeProject, "fakePredExplainId",
      "testPredictionExplanation.csv"
    )
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_true(file.exists("testPredictionExplanation.csv"))
  unlink("testPredictionExplanation.csv")
})


# Utility functions
noop <- function(...) "NOOP"
mockedId <- function(...) list(id = "mock-id")
doNotCall <- function(...) stop("Should not be called!")
checkParams <- function(...) {
  paramsForCheck <<- list(...)
  list(id = "mocked-id")
}

test_that("GetPredictionExplanations flow succeeds with dataset", {
  getStub <- stub(httr::GET)
  datasetsEndpoint <- datarobot:::UrlJoin(
    fakeEndpoint, "projects", fakeProjectId,
    "predictionDatasets"
  )
  datasetEndpoint <- datarobot:::UrlJoin(datasetsEndpoint, fakeDatasetId)
  getStub$expects(url = datasetEndpoint)
  datasetJson <- sprintf(
    '{"id": "%s", "projectId": "%s", "name": "My Dataset", "created": "2016-02-16T12:00:00.123456Z",
    "numRows": 100, "numColumns": 20, "forecastPoint": null, "predictionsStartDate": null,
  "predictionsEndDate": null}', fakeDatasetId, fakeProjectId
  )
  getResponse <- httr:::response(
    url = datasetsEndpoint,
    status_code = 200L,
    content = charToRaw(datasetJson)
  )
  getStub$returns(getResponse)
  dataset <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictionDataset(fakeProjectId, fakeDatasetId)
  )
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(predExplanationsPageResponse)
  predFrame <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::Predict" = noop,
    "datarobot::GetFeatureImpact" = noop,
    "datarobot::RequestPredictionExplanationsInitialization" = noop,
    "datarobot::GetPredictionExplanationsInitializationFromJobId" = noop,
    "datarobot::RequestPredictionExplanations" = noop,
    "datarobot::GetPredictionExplanationsMetadataFromJobId" = mockedId,
    "datarobot::UploadPredictionDataset" = doNotCall,
    "datarobot::GetPredictionDataset" = doNotCall,
    GetPredictionExplanations(fakeModel, dataset)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_true(nrow(predFrame) > 1)
  ExpectHasKeys(predFrame, explanationKeys)
})

test_that("GetPredictionExplanations flow succeeds with dataframe", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(predExplanationsPageResponse)
  predFrame <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::Predict" = noop,
    "datarobot::GetFeatureImpact" = noop,
    "datarobot::RequestPredictionExplanationsInitialization" = noop,
    "datarobot::GetPredictionExplanationsInitializationFromJobId" = noop,
    "datarobot::RequestPredictionExplanations" = noop,
    "datarobot::GetPredictionExplanationsMetadataFromJobId" = mockedId,
    "datarobot::UploadPredictionDataset" = checkParams,
    "datarobot::GetPredictionDataset" = doNotCall,
    GetPredictionExplanations(fakeModel, iris)
  )
  expect_equal(paramsForCheck[[1]], fakeProjectId)
  expect_equal(paramsForCheck[[2]], iris)
  expect_equal(getStub$calledTimes(), 1)
  expect_true(nrow(predFrame) > 1)
  ExpectHasKeys(predFrame, explanationKeys)
})

test_that("GetPredictionExplanations flow succeeds with CSV path", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(predExplanationsPageResponse)
  predFrame <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::Predict" = noop,
    "datarobot::GetFeatureImpact" = noop,
    "datarobot::RequestPredictionExplanationsInitialization" = noop,
    "datarobot::GetPredictionExplanationsInitializationFromJobId" = noop,
    "datarobot::RequestPredictionExplanations" = noop,
    "datarobot::GetPredictionExplanationsMetadataFromJobId" = mockedId,
    "datarobot::UploadPredictionDataset" = checkParams,
    "datarobot::GetPredictionDataset" = doNotCall,
    GetPredictionExplanations(fakeModel, "path.to.csv")
  )
  expect_equal(paramsForCheck[[1]], fakeProjectId)
  expect_equal(paramsForCheck[[2]], "path.to.csv")
  expect_equal(getStub$calledTimes(), 1)
  expect_true(nrow(predFrame) > 1)
  ExpectHasKeys(predFrame, explanationKeys)
})

test_that("GetPredictionExplanations flow succeeds with URL", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(predExplanationsPageResponse)
  predFrame <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::Predict" = noop,
    "datarobot::GetFeatureImpact" = noop,
    "datarobot::RequestPredictionExplanationsInitialization" = noop,
    "datarobot::GetPredictionExplanationsInitializationFromJobId" = noop,
    "datarobot::RequestPredictionExplanations" = noop,
    "datarobot::GetPredictionExplanationsMetadataFromJobId" = mockedId,
    "datarobot::UploadPredictionDataset" = checkParams,
    "datarobot::GetPredictionDataset" = doNotCall,
    GetPredictionExplanations(fakeModel, "http://url.com/url")
  )
  expect_equal(paramsForCheck[[1]], fakeProjectId)
  expect_equal(paramsForCheck[[2]], "http://url.com/url")
  expect_equal(getStub$calledTimes(), 1)
  expect_true(nrow(predFrame) > 1)
  ExpectHasKeys(predFrame, explanationKeys)
})

test_that("GetPredictionExplanations flow succeeds with datasetId", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(predExplanationsPageResponse)
  predFrame <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::Predict" = noop,
    "datarobot::GetFeatureImpact" = noop,
    "datarobot::RequestPredictionExplanationsInitialization" = noop,
    "datarobot::GetPredictionExplanationsInitializationFromJobId" = noop,
    "datarobot::RequestPredictionExplanations" = noop,
    "datarobot::GetPredictionExplanationsMetadataFromJobId" = mockedId,
    "datarobot::UploadPredictionDataset" = doNotCall,
    "datarobot::GetPredictionDataset" = checkParams,
    GetPredictionExplanations(fakeModel, fakeDatasetId)
  )
  expect_equal(paramsForCheck[[1]], fakeProjectId)
  expect_equal(paramsForCheck[[2]], fakeDatasetId)
  expect_equal(getStub$calledTimes(), 1)
  expect_true(nrow(predFrame) > 1)
  ExpectHasKeys(predFrame, explanationKeys)
})
