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


featureImpactUrl <- UrlJoin(projectUrl, "models", fakeModelId, "featureImpact")

featureImpactRequestResponse <- httr:::response(
  url = featureImpactUrl,
  status_code = 303L,
  headers = list(location = jobUrl),
  content = raw(0)
)

featureImpactJson <- fileToChar("responses/featureImpact.json")
completedFeatureImpactResponse <- httr:::response(
  url = featureImpactUrl,
  status_code = 200L,
  content = charToRaw(featureImpactJson)
)

modelJson <- fileToChar("responses/getModel.json")
completedModelResponse <- httr:::response(
  url = modelUrl,
  status_code = 200L,
  content = charToRaw(modelJson)
)

test_that("RequestFeatureImpact succeeds with correct job ID", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(featureImpactRequestResponse)
  postStub$expects(url = featureImpactUrl)
  returnedJobId <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RequestFeatureImpact(fakeModel)
  )
  expect_equal(returnedJobId, fakeJobId)
  expect_equal(postStub$calledTimes(), 1)
})

test_that("GetFeatureImpactForModel succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedFeatureImpactResponse)
  featureImpact <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetFeatureImpactForModel(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(featureImpact, "data.frame")
  ExpectHasKeys(featureImpact, c(
    "featureName", "impactNormalized",
    "impactUnnormalized", "redundantWith"
  ))
})

test_that("GetFeatureImpactForModel warns if redundancy detection is not run", {
  getStub <- stub(httr::GET)
  noRedundImpactJson <- gsub("true", "false", featureImpactJson)
  completedFeatureImpactResponse <- httr:::response(
    url = featureImpactUrl,
    status_code = 200L,
    content = charToRaw(noRedundImpactJson)
  )
  getStub$onCall(1)$returns(completedFeatureImpactResponse)
  expect_warning(
    with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetFeatureImpactForModel(fakeModel)
    ),
    "Redundancy detection was not run"
  )
  expect_equal(getStub$calledTimes(), 1)
})

test_that("GetFeatureImpactForJobId succeeds", {
  jobDataInprogress <- list(
    status = JobStatus$InProgress,
    url = "https://host_name.com/projects/p-id/jobs/1/",
    id = fakeJobId,
    jobType = JobType$FeatureImpact,
    projectId = fakeProjectId
  )

  jobDataComplete <- jobDataInprogress
  jobDataComplete$status <- JobStatus$Completed

  jobInprogressResponse <-
    httr:::response(
      url = featureImpactUrl,
      status_code = 200L,
      content = charToRaw(jsonlite::toJSON(jobDataInprogress))
    )

  jobCompleteResponse <-
    httr:::response(
      url = featureImpactUrl,
      status_code = 303L,
      headers = list(location = featureImpactUrl),
      content = charToRaw(jsonlite::toJSON(jobDataComplete))
    )


  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(jobInprogressResponse)
  getStub$onCall(2)$returns(jobCompleteResponse)
  getStub$onCall(3)$returns(completedFeatureImpactResponse)

  featureImpact <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetFeatureImpactForJobId(fakeProject, fakeJobId)
  )
  expect_equal(getStub$calledTimes(), 3)
  expect_s3_class(featureImpact, "data.frame")
  ExpectHasKeys(featureImpact, c(
    "featureName", "impactNormalized",
    "impactUnnormalized", "redundantWith"
  ))
})

test_that("GetFeatureImpactForJobId gives error with wrong job type", {
  jobDataInprogress <- list(
    status = JobStatus$InProgress,
    url = "https://host_name.com/projects/p-id/jobs/1/",
    id = fakeJobId,
    jobType = JobType$Model,
    projectId = fakeProjectId
  )

  jobInprogressResponse <-
    httr:::response(
      url = featureImpactUrl,
      status_code = 200L,
      content = charToRaw(jsonlite::toJSON(jobDataInprogress))
    )

  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(jobInprogressResponse)

  expect_error(
    {
      with_mock(
        "httr::GET" = getStub$f,
        "datarobot:::Endpoint" = function() fakeEndpoint,
        "datarobot:::Token" = function() fakeToken,
        GetFeatureImpactForJobId(fakeProject, fakeJobId)
      )
    },
    "jobs of type"
  )
})


test_that("maxWait parameter is passed to WaitForAsyncReturn", {
  # This test uses the old pattern that most of these tests follow of mocking high-level datarobot
  # functions. Would be better sometime to switch to mocking httr functions.
  maxWaitToUse <- 2
  jobDataInprogress <- list(
    status = JobStatus$InProgress,
    url = "https://host_name.com/projects/p-id/jobs/1/",
    id = fakeJobId,
    jobType = JobType$FeatureImpact,
    projectId = fakeProjectId
  )

  jobInprogressResponse <-
    httr:::response(
      url = featureImpactUrl,
      status_code = 200L,
      content = charToRaw(jsonlite::toJSON(jobDataInprogress))
    )

  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedModelResponse)
  model <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::GetProject" = function(project) {
      p <- fromJSON(fileToChar("responses/GetProject.json"))
      datarobot:::as.dataRobotProject(p)
    }, GetModel(fakeProject, fakeModelId)
  )

  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(jobInprogressResponse)

  with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot::WaitForAsyncReturn" = function(...) {
      expect_equal(list(...)$maxWait, maxWaitToUse)
      model
    },
    "datarobot::FeatureImpactFromResponseList" = function(response) {
      completedFeatureImpactResponse
    },
    GetFeatureImpactForJobId(fakeProject, fakeJobId, maxWait = maxWaitToUse)
  )
})


test_that("GetFeatureImpact succeeds when request feature impact is necessary", {
  jobDataInprogress <- list(
    status = JobStatus$InProgress,
    url = "https://host_name.com/projects/p-id/jobs/1/",
    id = fakeJobId,
    jobType = JobType$FeatureImpact,
    projectId = fakeProjectId
  )

  jobDataComplete <- jobDataInprogress
  jobDataComplete$status <- JobStatus$Completed

  jobInprogressResponse <-
    httr:::response(
      url = featureImpactUrl,
      status_code = 200L,
      content = charToRaw(jsonlite::toJSON(jobDataInprogress))
    )

  jobCompleteResponse <-
    httr:::response(
      url = featureImpactUrl,
      status_code = 303L,
      headers = list(location = featureImpactUrl),
      content = charToRaw(jsonlite::toJSON(jobDataComplete))
    )

  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(jobInprogressResponse)
  getStub$onCall(2)$returns(jobCompleteResponse)
  getStub$onCall(3)$returns(completedFeatureImpactResponse)

  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(featureImpactRequestResponse)
  postStub$expects(url = featureImpactUrl)

  featureImpact <- with_mock(
    "httr::POST" = postStub$f,
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetFeatureImpact(fakeModel)
  )

  expect_equal(getStub$calledTimes(), 3)
  expect_equal(postStub$calledTimes(), 1)
  expect_s3_class(featureImpact, "data.frame")
  ExpectHasKeys(featureImpact, c(
    "featureName", "impactNormalized",
    "impactUnnormalized", "redundantWith"
  ))
})


test_that("GetFeatureImpact succeeds when request feature impact is not necessary", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedFeatureImpactResponse)
  featureImpact <- with_mock(
    "httr::POST" = function(...) {
      stop("FEATURE IMPACT ALREADY CALCULATED")
    },
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetFeatureImpact(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(featureImpact, "data.frame")
  ExpectHasKeys(featureImpact, c(
    "featureName", "impactNormalized",
    "impactUnnormalized", "redundantWith"
  ))
})
