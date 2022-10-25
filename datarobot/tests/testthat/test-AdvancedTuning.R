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


describe("GetTuningParameters", {
  test_that("Required parameters are present", {
    expect_error(GetTuningParameters())
    expect_error(GetTuningParameters(fakeProject))
    expect_error(GetModel(modelId))
  })

  projectUrl <- UrlJoin(fakeEndpoint, "projects", fakeProjectId)
  paramsUrl <- UrlJoin(projectUrl, "models", fakeModelId, "advancedTuning", "parameters")
  paramsJson <- fileToChar("responses/GetTuningParameters.json")
  completedParamsResponse <- httr:::response(
    url = paramsUrl,
    status_code = 200L,
    content = charToRaw(paramsJson)
  )

  test_that("GetTuningParameters succeeds", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    params <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetTuningParameters(fakeModel)
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_s3_class(params, "listOfDataRobotTuningParameters")
    params <- params$tuningParameters
    ExpectHasKeys(params[[1]], c(
      "currentValue", "defaultValue", "parameterId", "parameterName",
      "taskName", "constraints"
    ))
    expect_type(params[[1]]$currentValue, "character")
    expect_type(params[[1]]$defaultValue, "character")
    expect_type(params[[1]]$parameterId, "character")
    expect_type(params[[1]]$parameterName, "character")
    expect_type(params[[1]]$taskName, "character")
    expect_type(params[[1]]$constraints, "list")
  })
})


projectUrl <- UrlJoin(fakeEndpoint, "projects", fakeProjectId)
paramsUrl <- UrlJoin(projectUrl, "models", fakeModelId, "advancedTuning", "parameters")
paramsJson <- fileToChar("responses/GetTuningParameters.json")
completedParamsResponse <- httr:::response(
  url = paramsUrl,
  status_code = 200L,
  content = charToRaw(paramsJson)
)
modelJobsUrl <- UrlJoin(projectUrl, "modelJobs", fakeJobId)
tuningResponse <- httr:::response(
  url = modelJobsUrl,
  status_code = 202L,
  headers = list(location = modelJobsUrl),
  content = raw(0)
)

describe("Summarize tuning parameters", {
  test_that("Tuning parameters can be summarized", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    params <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetTuningParameters(fakeModel)
    )
    expect_equal(getStub$calledTimes(), 1)
    paramsSummary <- summary(params)
    expect_s3_class(paramsSummary, "data.frame")
    ExpectHasKeys(paramsSummary, c("current", "default", "name", "constraint"))
  })
})


describe("StartTuningSession", {
  test_that("It dynamically generates a function for tuning", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    RunTune <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      StartTuningSession(fakeModel)
    )
    expect_equal(getStub$calledTimes(), 1)
    ExpectHasKeys(
      formals(RunTune),
      c(
        "model", "card_max", "method", "min_support", "arbimp", "min_count_na",
        "base_margin_initialize", "colsample_bylevel", "colsample_bytree",
        "interval", "learning_rate", "max_bin", "max_delta_step", "max_depth",
        "min_child_weight", "min_split_loss", "missing_value", "n_estimators",
        "num_parallel_tree", "random_state", "reg_alpha", "reg_lambda",
        "scale_pos_weight", "smooth_interval", "subsample", "tree_method",
        "tuningDescription"
      )
    )
  })

  test_that("It can tune one parameter without a tuning description", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    RunTune <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      StartTuningSession(fakeModel)
    )
    expect_equal(getStub$calledTimes(), 1)
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    postStub <- stub(httr::GET)
    postStub$onCall(1)$returns(tuningResponse)
    jobId <- with_mock(
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
      RunTune(fakeModel, colsample_bytree = 0.9)
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_equal(postStub$calledTimes(), 1)
    ExpectHasKeys(bodyForInspect, "tuningParameters")
    expect_equal(length(bodyForInspect$tuningParameters), 1)
    ExpectHasKeys(bodyForInspect$tuningParameters[[1]], c("parameterId", "value"))
    expect_equal(bodyForInspect$tuningParameters[[1]]$value, 0.9)
    expect_equal(jobId, fakeJobId)
  })

  test_that("It can tune one parameter using a variable", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    RunTune <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      StartTuningSession(fakeModel)
    )
    expect_equal(getStub$calledTimes(), 1)
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    postStub <- stub(httr::GET)
    postStub$onCall(1)$returns(tuningResponse)
    jobId <- with_mock(
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
      {
        colsampleToUse <- 0.7
        RunTune(fakeModel, colsample_bytree = colsampleToUse)
      }
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_equal(postStub$calledTimes(), 1)
    ExpectHasKeys(bodyForInspect, "tuningParameters")
    expect_equal(length(bodyForInspect$tuningParameters), 1)
    ExpectHasKeys(bodyForInspect$tuningParameters[[1]], c("parameterId", "value"))
    expect_equal(bodyForInspect$tuningParameters[[1]]$value, 0.7)
    expect_equal(jobId, fakeJobId)
  })

  test_that("It can tune one parameter using a variable II - different scope", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    RunTune <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      StartTuningSession(fakeModel)
    )
    expect_equal(getStub$calledTimes(), 1)
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    postStub <- stub(httr::GET)
    postStub$onCall(1)$returns(tuningResponse)
    colsampleToUse <- 0.6
    jobId <- with_mock(
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
      RunTune(fakeModel, colsample_bytree = colsampleToUse)
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_equal(postStub$calledTimes(), 1)
    ExpectHasKeys(bodyForInspect, "tuningParameters")
    expect_equal(length(bodyForInspect$tuningParameters), 1)
    ExpectHasKeys(bodyForInspect$tuningParameters[[1]], c("parameterId", "value"))
    expect_equal(bodyForInspect$tuningParameters[[1]]$value, colsampleToUse)
    expect_equal(jobId, fakeJobId)
  })

  test_that("It can return a sane error if tuning with a variable that does not exist", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    RunTune <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      StartTuningSession(fakeModel)
    )
    expect_equal(getStub$calledTimes(), 1)
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    postStub <- stub(httr::GET)
    postStub$onCall(1)$returns(tuningResponse)
    expect_error(
      with_mock(
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
        RunTune(fakeModel, colsample_bytree = variableNotFound)
      ),
      "object 'variableNotFound' not found"
    )
  })

  test_that("It can tune one parameter with a tuning description", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    RunTune <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      StartTuningSession(fakeModel)
    )
    expect_equal(getStub$calledTimes(), 1)
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    postStub <- stub(httr::GET)
    postStub$onCall(1)$returns(tuningResponse)
    jobId <- with_mock(
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
      RunTune(fakeModel, colsample_bytree = 0.9, tuningDescription = "bob")
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_equal(postStub$calledTimes(), 1)
    ExpectHasKeys(bodyForInspect, c("tuningParameters", "tuningDescription"))
    expect_equal(bodyForInspect$tuningDescription, "bob")
    expect_equal(length(bodyForInspect$tuningParameters), 1)
    ExpectHasKeys(bodyForInspect$tuningParameters[[1]], c("parameterId", "value"))
    expect_equal(bodyForInspect$tuningParameters[[1]]$value, 0.9)
    expect_equal(jobId, fakeJobId)
  })

  test_that("It can tune two parameters without a tuning description", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    RunTune <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      StartTuningSession(fakeModel)
    )
    expect_equal(getStub$calledTimes(), 1)
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    postStub <- stub(httr::GET)
    postStub$onCall(1)$returns(tuningResponse)
    jobId <- with_mock(
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
      RunTune(fakeModel, colsample_bytree = 0.9, method = "hist")
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_equal(postStub$calledTimes(), 1)
    ExpectHasKeys(bodyForInspect, "tuningParameters")
    expect_equal(length(bodyForInspect$tuningParameters), 2)
    ExpectHasKeys(bodyForInspect$tuningParameters[[1]], c("parameterId", "value"))
    expect_equal(bodyForInspect$tuningParameters[[1]]$value, 0.9)
    ExpectHasKeys(bodyForInspect$tuningParameters[[2]], c("parameterId", "value"))
    expect_equal(bodyForInspect$tuningParameters[[2]]$value, "hist")
    expect_equal(jobId, fakeJobId)
  })

  test_that("It can tune two parameters with a tuning description", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    RunTune <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      StartTuningSession(fakeModel)
    )
    expect_equal(getStub$calledTimes(), 1)
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    postStub <- stub(httr::GET)
    postStub$onCall(1)$returns(tuningResponse)
    jobId <- with_mock(
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
      RunTune(fakeModel,
        colsample_bytree = 0.9,
        method = "hist",
        tuningDescription = "bob"
      )
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_equal(postStub$calledTimes(), 1)
    ExpectHasKeys(bodyForInspect, c("tuningParameters", "tuningDescription"))
    expect_equal(bodyForInspect$tuningDescription, "bob")
    expect_equal(length(bodyForInspect$tuningParameters), 2)
    ExpectHasKeys(bodyForInspect$tuningParameters[[1]], c("parameterId", "value"))
    expect_equal(bodyForInspect$tuningParameters[[1]]$value, 0.9)
    ExpectHasKeys(bodyForInspect$tuningParameters[[2]], c("parameterId", "value"))
    expect_equal(bodyForInspect$tuningParameters[[2]]$value, "hist")
    expect_equal(jobId, fakeJobId)
  })
})


describe("RunInteractiveTuning", {
  test_that("It can tune two parameters with a tuning description", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedParamsResponse)
    postStub <- stub(httr::GET)
    postStub$onCall(1)$returns(tuningResponse)
    jobId <- with_mock(
      "httr::GET" = getStub$f,
      "httr::POST" = postStub$f,
      "datarobot:::IsInteractiveMode" = function() TRUE,
      "datarobot:::GetUserInput" = function(...) 1,
      "datarobot:::DataRobotPOST" = function(routeString, addUrl = TRUE,
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
      RunInteractiveTuning(fakeModel)
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_equal(postStub$calledTimes(), 1)
    ExpectHasKeys(bodyForInspect, c("tuningParameters", "tuningDescription"))
    expect_equal(bodyForInspect$tuningDescription, 1)
    ExpectHasKeys(bodyForInspect$tuningParameters[[1]], c("parameterId", "value"))
    expect_equal(bodyForInspect$tuningParameters[[1]]$value, 1)
    expect_equal(jobId, fakeJobId)
  })
})
