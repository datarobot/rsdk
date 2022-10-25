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


deploymentCols <- c(
  "defaultPredictionServer", "description", "modelHealth",
  "predictionUsage", "capabilities", "label", "id", "model",
  "accuracyHealth", "serviceHealth", "permissions"
)
checks <- c(
  "targetType", "target", "notCurrentModel", "permission", "supported",
  "modelCanBeDeployed", "seriesType", "modelStatus", "featureDataTypes",
  "features"
)


test_that("it can list deployments", {
  getStub <- stub(httr::GET)
  listDeploymentUrl <- "deployments"
  listDeploymentJson <- fileToChar("responses/listDeployments.json")
  deploymentResponse <- httr:::response(
    url = listDeploymentUrl,
    status_code = 200L,
    content = charToRaw(listDeploymentJson)
  )
  getStub$onCall(1)$returns(deploymentResponse)
  deployments <- with_mock(
    `httr::GET` = getStub$f,
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    ListDeployments()
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(deployments, "listOfDeployments")
  deployment <- deployments[[1]]
  expect_s3_class(deployment, "dataRobotDeployment")
  ExpectHasKeys(deployment, deploymentCols)
  expect_s3_class(deployment$model, "dataRobotModel")
  expect_type(deployment$permissions, "character")
})

test_that("it can pass orderBy query parameter to list deployments", {
  listDeploymentUrl <- "deployments"
  deploymentResponse <- httr:::response(
    url = listDeploymentUrl,
    status_code = 200L,
    content = charToRaw("Passed the query params")
  )

  fakeResponse <- with_mock(
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::DataRobotGET" = function(query = NULL, ...) {
      getQueryForInspect <<- query
      deploymentResponse
    },
    ListDeployments(orderBy = "-label")
  )
  expect_equal(getQueryForInspect$orderBy, "-label")
  expect_null(getQueryForInspect$search)
})

test_that("it can pass search query parameter to list deployments", {
  listDeploymentUrl <- "deployments"
  deploymentResponse <- httr:::response(
    url = listDeploymentUrl,
    status_code = 200L,
    content = charToRaw("Passed the query params")
  )

  fakeResponse <- with_mock(
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::DataRobotGET" = function(query = NULL, ...) {
      getQueryForInspect <<- query
      deploymentResponse
    },
    ListDeployments(search = "my_cool_deployment")
  )
  expect_equal(getQueryForInspect$search, "my_cool_deployment")
  expect_null(getQueryForInspect$orderBy)
})

test_that("it can pass both orderBy and search query parameters to list deployments", {
  listDeploymentUrl <- "deployments"
  deploymentResponse <- httr:::response(
    url = listDeploymentUrl,
    status_code = 200L,
    content = charToRaw("Passed the query params")
  )

  fakeResponse <- with_mock(
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    "datarobot:::DataRobotGET" = function(query = NULL, ...) {
      getQueryForInspect <<- query
      deploymentResponse
    },
    ListDeployments(orderBy = "-list", search = "my_cool_deployment")
  )
  expect_equal(getQueryForInspect$search, "my_cool_deployment")
  expect_equal(getQueryForInspect$orderBy, "-list")
})

test_that("it can de-paginate deployments", {
  getStub <- stub(httr::GET)
  listDeploymentUrl <- "deployments"
  listDeploymentJsonPage1 <- fileToChar("responses/listDeploymentsPaginate1.json")
  listDeploymentJsonPage2 <- fileToChar("responses/listDeploymentsPaginate2.json")
  deploymentResponse <- httr:::response(
    url = listDeploymentUrl,
    status_code = 200L,
    content = charToRaw(listDeploymentJsonPage1)
  )
  nextResponse <- httr:::response(
    url = "https://fake.next.page",
    status_code = 200L,
    content = charToRaw(listDeploymentJsonPage2)
  )
  getStub$onCall(1)$returns(deploymentResponse)
  getStub$onCall(2)$returns(nextResponse)
  deployments <- with_mock(
    `httr::GET` = getStub$f,
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    ListDeployments()
  )
  expect_equal(getStub$calledTimes(), 2)
  deploymentResponseParsed <- fromJSON(httr::content(deploymentResponse, as = "text"))
  expect_equal(length(deployments), deploymentResponseParsed$totalCount)
  expect_s3_class(deployments, "listOfDeployments")
  deployment <- deployments[[1]]
  expect_s3_class(deployment, "dataRobotDeployment")
  ExpectHasKeys(deployment, deploymentCols)
  expect_s3_class(deployment$model, "dataRobotModel")
  expect_type(deployment$permissions, "character")
})

test_that("it can get a deployment", {
  getStub <- stub(httr::GET)
  getDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId)
  getDeploymentJson <- fileToChar("responses/getDeployment.json")
  deploymentResponse <- httr:::response(
    url = getDeploymentUrl,
    status_code = 200L,
    content = charToRaw(getDeploymentJson)
  )
  getStub$onCall(1)$returns(deploymentResponse)
  deployment <- with_mock(
    `httr::GET` = getStub$f,
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    GetDeployment(fakeDeploymentId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(deployment, "dataRobotDeployment")
  ExpectHasKeys(deployment, deploymentCols)
  expect_s3_class(deployment$model, "dataRobotModel")
  expect_type(deployment$permissions, "character")
})


describe("CreateDeployment", {
  test_that("it can create a new deployment", {
    postStub <- stub(httr::POST)
    createDeploymentUrl <- UrlJoin("deployments")
    createDeploymentResponse <- httr:::response(
      url = createDeploymentUrl,
      status_code = 202L,
      content = charToRaw(sprintf(
        '{"id": "%s"}',
        fakeDeploymentId
      ))
    )
    postStub$onCall(1)$returns(createDeploymentResponse)
    getStub <- stub(httr::GET)
    getDeploymentUrl <- UrlJoin("deployments", "fromLearningModel")
    getDeploymentJson <- fileToChar("responses/getDeployment.json")
    deploymentResponse <- httr:::response(
      url = getDeploymentUrl,
      status_code = 200L,
      content = charToRaw(getDeploymentJson)
    )
    getStub$onCall(1)$returns(deploymentResponse)
    response <- with_mock(
      `httr::POST` = postStub$f,
      `httr::GET` = getStub$f,
      `datarobot:::DataRobotPOST` = function(routeString,
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
      `datarobot:::Endpoint` = function() fakeEndpoint,
      `datarobot:::Token` = function() fakeToken,
      CreateDeployment(fakeModel,
        label = "label"
      )
    )
    expect_equal(postStub$calledTimes(), 1)
    expect_equal(getStub$calledTimes(), 1)
    expect_s3_class(response, "dataRobotDeployment")
    expect_equal(bodyForInspect$modelId, fakeModelId)
    expect_equal(bodyForInspect$label, "label")
    expect_equal(bodyForInspect$description, "")
    expect_null(bodyForInspect$defaultPredictionServerId)
  })

  test_that("it can pass a description and defaultPredictionServerId", {
    postStub <- stub(httr::POST)
    createDeploymentUrl <- UrlJoin("deployments")
    createDeploymentResponse <- httr:::response(
      url = createDeploymentUrl,
      status_code = 202L,
      content = charToRaw(sprintf(
        '{"id": "%s"}',
        fakeDeploymentId
      ))
    )
    postStub$onCall(1)$returns(createDeploymentResponse)
    getStub <- stub(httr::GET)
    getDeploymentUrl <- UrlJoin("deployments", "fromLearningModel")
    getDeploymentJson <- fileToChar("responses/getDeployment.json")
    deploymentResponse <- httr:::response(
      url = getDeploymentUrl,
      status_code = 200L,
      content = charToRaw(getDeploymentJson)
    )
    getStub$onCall(1)$returns(deploymentResponse)
    response <- with_mock(
      `httr::POST` = postStub$f,
      `httr::GET` = getStub$f,
      `datarobot:::DataRobotPOST` = function(routeString,
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
      `datarobot:::Endpoint` = function() fakeEndpoint,
      `datarobot:::Token` = function() fakeToken,
      CreateDeployment(fakeModel,
        label = "label",
        description = "description",
        defaultPredictionServerId = fakePredictionServerId
      )
    )
    expect_equal(postStub$calledTimes(), 1)
    expect_equal(getStub$calledTimes(), 1)
    expect_s3_class(response, "dataRobotDeployment")
    expect_equal(bodyForInspect$modelId, fakeModelId)
    expect_equal(bodyForInspect$label, "label")
    expect_equal(bodyForInspect$description, "description")
    expect_equal(bodyForInspect$defaultPredictionServerId, fakePredictionServerId)
  })

  test_that("it can pass a prediction server as the defaultPredictionServerId", {
    postStub <- stub(httr::POST)
    createDeploymentUrl <- UrlJoin("deployments")
    createDeploymentResponse <- httr:::response(
      url = createDeploymentUrl,
      status_code = 202L,
      content = charToRaw(sprintf(
        '{"id": "%s"}',
        fakeDeploymentId
      ))
    )
    postStub$onCall(1)$returns(createDeploymentResponse)
    getStub <- stub(httr::GET)
    getDeploymentUrl <- UrlJoin("deployments", "fromLearningModel")
    getDeploymentJson <- fileToChar("responses/getDeployment.json")
    deploymentResponse <- httr:::response(
      url = getDeploymentUrl,
      status_code = 200L,
      content = charToRaw(getDeploymentJson)
    )
    getStub$onCall(1)$returns(deploymentResponse)
    response <- with_mock(
      `httr::POST` = postStub$f,
      `httr::GET` = getStub$f,
      `datarobot:::DataRobotPOST` = function(routeString,
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
      `datarobot:::Endpoint` = function() fakeEndpoint,
      `datarobot:::Token` = function() fakeToken,
      CreateDeployment(fakeModel,
        label = "label",
        description = "description",
        defaultPredictionServerId = fakePredictionServer
      )
    )
    expect_equal(postStub$calledTimes(), 1)
    expect_equal(getStub$calledTimes(), 1)
    expect_s3_class(response, "dataRobotDeployment")
    expect_equal(bodyForInspect$modelId, fakeModelId)
    expect_equal(bodyForInspect$label, "label")
    expect_equal(bodyForInspect$description, "description")
    expect_equal(bodyForInspect$defaultPredictionServerId, fakePredictionServerId)
  })
})


describe("DeleteDeployment", {
  test_that("it can delete a deployment", {
    deleteStub <- stub(httr::DELETE)
    deleteDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId)
    deploymentResponse <- httr:::response(
      url = deleteDeploymentUrl,
      status_code = 204L,
      content = raw(0)
    )
    deleteStub$onCall(1)$returns(deploymentResponse)
    deployment <- with_mock(
      `httr::DELETE` = deleteStub$f,
      `datarobot:::Endpoint` = function() fakeEndpoint,
      `datarobot:::Token` = function() fakeToken,
      DeleteDeployment(fakeDeploymentId)
    )
    expect_null(deployment)
  })

  test_that("it can delete a deployment with an object", {
    deleteStub <- stub(httr::DELETE)
    deleteDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId)
    deploymentResponse <- httr:::response(
      url = deleteDeploymentUrl,
      status_code = 204L,
      content = raw(0)
    )
    deleteStub$onCall(1)$returns(deploymentResponse)
    deployment <- with_mock(
      `httr::DELETE` = deleteStub$f,
      `datarobot:::Endpoint` = function() fakeEndpoint,
      `datarobot:::Token` = function() fakeToken,
      DeleteDeployment(fakeDeployment)
    )
    expect_null(deployment)
  })
})


describe("ValidateReplaceDeployedModel", {
  test_that("it can validate a deployment replacement", {
    postStub <- stub(httr::POST)
    replaceDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId, "model", "validation")
    replaceDeploymentJson <- fileToChar("responses/validateDeployment.json")
    deploymentResponse <- httr:::response(
      url = replaceDeploymentUrl,
      status_code = 202L,
      content = charToRaw(replaceDeploymentJson)
    )
    postStub$onCall(1)$returns(deploymentResponse)
    response <- with_mock(
      `httr::POST` = postStub$f,
      `httr::GET` = function(...) stop("Should not be called!"),
      `datarobot:::Endpoint` = function() fakeEndpoint,
      `datarobot:::Token` = function() fakeToken,
      ValidateReplaceDeployedModel(fakeDeployment, fakeModel)
    )
    expect_equal(postStub$calledTimes(), 1)
    expect_type(response, "list")
    ExpectHasKeys(response, c("status", "message", "checks"))
    expect_equal(response$status, "passing")
    ExpectHasKeys(response$checks, checks)
    ExpectHasKeys(response$checks[[1]], c("status", "message"))
    expect_equal(response$checks[[1]]$status, "passing")
  })

  test_that("it can validate a deployment replacement that has errors", {
    postStub <- stub(httr::POST)
    replaceDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId, "model", "validation")
    replaceDeploymentJson <- fileToChar("responses/validateDeploymentWithErrors.json")
    deploymentResponse <- httr:::response(
      url = replaceDeploymentUrl,
      status_code = 202L,
      content = charToRaw(replaceDeploymentJson)
    )
    postStub$onCall(1)$returns(deploymentResponse)
    response <- with_mock(
      `httr::POST` = postStub$f,
      `httr::GET` = function(...) stop("Should not be called!"),
      `datarobot:::Endpoint` = function() fakeEndpoint,
      `datarobot:::Token` = function() fakeToken,
      ValidateReplaceDeployedModel(fakeDeployment, fakeModel)
    )
    expect_equal(postStub$calledTimes(), 1)
    expect_type(response, "list")
    ExpectHasKeys(response, c("status", "message", "checks"))
    expect_equal(response$status, "failing")
    ExpectHasKeys(response$checks, checks)
    ExpectHasKeys(response$checks[[1]], c("status", "message"))
    expect_equal(response$checks[[1]]$status, "passing")
    expect_equal(response$checks[[3]]$status, "failing")
  })
})


describe("ReplaceDeployedModel", {
  test_that("it can replace a deployment", {
    patchStub <- stub(httr::PATCH)
    replaceDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId, "model")
    replaceDeploymentJson <- fileToChar("responses/replaceDeployment.json")
    replaceDeploymentResponse <- httr:::response(
      url = replaceDeploymentUrl,
      status_code = 202L,
      content = charToRaw(replaceDeploymentJson)
    )
    patchStub$onCall(1)$returns(replaceDeploymentResponse)
    deploymentReplaceJson <- fileToChar("responses/getDeployment.json")
    deploymentReplaceUrl <- UrlJoin("deployments", fakeDeploymentId, "model")
    deploymentReplaceRequestResponse <- httr:::response(
      url = deploymentReplaceUrl,
      status_code = 202L,
      content = charToRaw(deploymentReplaceJson)
    )
    response <- with_mock(
      `httr::PATCH` = patchStub$f,
      `httr::POST` = function(...) stop("Should not be called!"),
      `datarobot:::DataRobotPATCH` = function(routeString,
                                              addUrl = TRUE,
                                              body = NULL,
                                              returnRawResponse = FALSE, ...) {
        bodyForInspect <<- body
        datarobot:::MakeDataRobotRequest(httr::PATCH, routeString,
          addUrl = addUrl,
          returnRawResponse = returnRawResponse,
          body = body, ...
        )
      },
      `datarobot::WaitForAsyncReturn` = function(...) {
        ParseReturnResponse(deploymentReplaceRequestResponse)
      },
      `datarobot:::Endpoint` = function() fakeEndpoint,
      `datarobot:::Token` = function() fakeToken,
      ReplaceDeployedModel(
        fakeDeployment,
        fakeModel,
        ModelReplacementReason$Other
      )
    )
    expect_equal(patchStub$calledTimes(), 1)
    expect_s3_class(response, "dataRobotDeployment")
    expect_equal(bodyForInspect$modelId, fakeModelId)
    expect_equal(bodyForInspect$reason, ModelReplacementReason$Other)
  })

  test_that("it can replace a deployment", {
    patchStub <- stub(httr::PATCH)
    replaceDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId, "model")
    replaceDeploymentJson <- fileToChar("responses/replaceDeploymentWithErrors.json")
    replaceDeploymentResponse <- httr:::response(
      url = replaceDeploymentUrl,
      status_code = 409L,
      content = charToRaw(replaceDeploymentJson)
    )
    patchStub$onCall(1)$returns(replaceDeploymentResponse)
    deploymentReplaceJson <- fileToChar("responses/getDeployment.json")
    deploymentReplaceUrl <- UrlJoin("deployments", fakeDeploymentId, "model")
    deploymentReplaceRequestResponse <- httr:::response(
      url = deploymentReplaceUrl,
      status_code = 202L,
      content = charToRaw(deploymentReplaceJson)
    )
    expect_error(
      with_mock(
        `httr::PATCH` = patchStub$f,
        `httr::POST` = function(...) stop("Should not be called!"),
        `datarobot:::DataRobotPATCH` = function(routeString,
                                                addUrl = TRUE,
                                                body = NULL,
                                                returnRawResponse = FALSE, ...) {
          bodyForInspect <<- body
          datarobot:::MakeDataRobotRequest(httr::PATCH, routeString,
            addUrl = addUrl,
            returnRawResponse = returnRawResponse,
            body = body, ...
          )
        },
        `datarobot::WaitForAsyncReturn` = function(...) {
          ParseReturnResponse(deploymentReplaceRequestResponse)
        },
        `datarobot:::Endpoint` = function() fakeEndpoint,
        `datarobot:::Token` = function() fakeToken,
        ReplaceDeployedModel(
          fakeDeployment,
          fakeModel,
          ModelReplacementReason$Other
        )
      ),
      paste(
        "Model deployment failure - The following model deployment checks",
        "failed: Model is already used as current model of the deployment."
      )
    )
    expect_equal(patchStub$calledTimes(), 1)
    expect_equal(bodyForInspect$modelId, fakeModelId)
    expect_equal(bodyForInspect$reason, ModelReplacementReason$Other)
  })
})


test_that("it can list prediction servers", {
  getStub <- stub(httr::GET)
  listPredictionServerUrl <- "predictionServers"
  listPredictionServerJson <- fileToChar("responses/listPredictionServers.json")
  predictionServerResponse <- httr:::response(
    url = listPredictionServerUrl,
    status_code = 200L,
    content = charToRaw(listPredictionServerJson)
  )
  getStub$onCall(1)$returns(predictionServerResponse)
  predictionServers <- with_mock(
    `httr::GET` = getStub$f,
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    ListPredictionServers()
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(predictionServers, "listOfPredictionServers")
  predictionServer <- predictionServers[[1]]
  expect_s3_class(predictionServer, "dataRobotPredictionServer")
  ExpectHasKeys(predictionServer, c("id", "url", "dataRobotKey"))
})

describe("Submit Actuals", {
  postSubmitActualsUrl <- UrlJoin("deployments", fakeDeploymentId, "actuals", "fromJSON")
  postSubmitActualsMock <- function(postStub, submitActualsRequestResponse, submitActualsArgs) {
    response <- with_mock(
      `httr::POST` = postStub$f,
      `datarobot:::DataRobotPOST` = function(routeString,
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
      `datarobot::WaitForAsyncReturn` = function(...) {
        ParseReturnResponse(submitActualsRequestResponse)
      },
      `datarobot:::Endpoint` = function() fakeEndpoint,
      `datarobot:::Token` = function() fakeToken,
      do.call(SubmitActuals, submitActualsArgs)
    )
    response
  }

  test_that("it will submit valid actuals", {
    postStub <- stub(httr::POST)
    statusUrl <- UrlJoin(fakeEndpoint, "status", fakeJobId)
    submitActualsRequestResponse <- httr:::response(
      url = postSubmitActualsUrl,
      status_code = 202L,
      headers = list(location = statusUrl),
      content = raw(0)
    )
    postStub$onCall(1)$returns(submitActualsRequestResponse)
    fakeValidActuals <- data.frame(
      associationId = c("123", "abc"),
      actualValue = c("True", "False"),
      wasActedOn = c(TRUE, FALSE),
      timestamp = c(
        as.POSIXct("2021-01-28 19:11:04"),
        as.POSIXct("2020-01-01 15:11:04")
      ),
      stringsAsFactors = FALSE
    )
    postSubmitActualsMock(
      postStub,
      submitActualsRequestResponse,
      list(fakeValidActuals, fakeDeploymentId)
    )
    convertedTimeStampValidActuals <- fakeValidActuals
    formattedTimeStamps <- formatRFC3339Timestamp(fakeValidActuals[["timestamp"]])
    convertedTimeStampValidActuals[["timestamp"]] <- formattedTimeStamps

    expect_equal(postStub$calledTimes(), 1)
    expect_identical(as.data.frame(bodyForInspect$data), convertedTimeStampValidActuals)
  })

  test_that("it will submit valid actuals that encode strings with factors", {
    postStub <- stub(httr::POST)
    statusUrl <- UrlJoin(fakeEndpoint, "status", fakeJobId)
    submitActualsRequestResponse <- httr:::response(
      url = postSubmitActualsUrl,
      status_code = 202L,
      headers = list(location = statusUrl),
      content = raw(0)
    )
    postStub$onCall(1)$returns(submitActualsRequestResponse)
    fakeValidActuals <- data.frame(
      associationId = c("123", "abc"),
      actualValue = c("True", "False"),
      wasActedOn = c(TRUE, FALSE),
      timestamp = c(
        as.POSIXct("2021-01-28 19:11:04"),
        as.POSIXct("2020-01-01 15:11:04")
      ),
      stringsAsFactors = TRUE
    )
    postSubmitActualsMock(
      postStub,
      submitActualsRequestResponse,
      list(fakeValidActuals, fakeDeploymentId)
    )

    expect_equal(postStub$calledTimes(), 1)
  })

  test_that("it won't submit invalid actuals", {
    postStub <- stub(httr::POST)
    statusUrl <- UrlJoin(fakeEndpoint, "status", fakeJobId)
    submitActualsRequestResponse <- httr:::response(
      url = postSubmitActualsUrl,
      status_code = 202L,
      headers = list(location = statusUrl),
      content = raw(0)
    )
    postStub$onCall(1)$returns(submitActualsRequestResponse)
    fakeInvalidActuals <- data.frame(
      actualValue = c("True", "False"),
      wasActedOn = c("TRUE", "FALSE"),
      timestamp = c(
        "2021-01-28 19:11:04",
        "2020-01-01 15:11:04"
      ),
      stringsAsFactors = FALSE
    )
    expect_error(postSubmitActualsMock(
      postStub,
      submitActualsRequestResponse,
      list(
        fakeInvalidActuals,
        fakeDeploymentId
      )
    ))
    expect_equal(postStub$calledTimes(), 0)
  })

  test_that("it will batch actuals", {
    postStub <- stub(httr::POST)
    statusUrl <- UrlJoin(fakeEndpoint, "status", fakeJobId)
    submitActualsRequestResponse <- httr:::response(
      url = postSubmitActualsUrl,
      status_code = 202L,
      headers = list(location = statusUrl),
      content = raw(0)
    )
    postStub$returns(submitActualsRequestResponse)
    fakeValidActuals <- data.frame(
      associationId = c("123", "abc"),
      actualValue = c("True", "False"),
      wasActedOn = c(TRUE, FALSE),
      timestamp = c(
        as.POSIXct("2021-01-28 19:11:04"),
        as.POSIXct("2020-01-01 15:11:04")
      ),
      stringsAsFactors = FALSE
    )
    postSubmitActualsMock(
      postStub,
      submitActualsRequestResponse,
      list(fakeValidActuals, fakeDeploymentId, batchSize = 1)
    )

    expect_equal(postStub$calledTimes(), 2)
  })
})

describe("GetDeploymentSettings()", {
  settingsJson <- fileToChar("responses/getDeploymentSettings.json")
  getSettingsUrl <- UrlJoin("deployments", fakeDeploymentId, "settings")

  getDeploymentJson <- fileToChar("responses/getDeploymentSettings.json")
  deploymentResponse <- httr:::response(
    url = getSettingsUrl,
    status_code = 200L,
    content = charToRaw(getDeploymentJson)
  )

  test_that("succeeds for a valid deployment", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(deploymentResponse)
    settings <- with_mock(
      `httr::GET` = getStub$f,
      `datarobot:::Endpoint` = function() fakeEndpoint,
      `datarobot:::Token` = function() fakeToken,
      GetDeploymentSettings(fakeDeployment)
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_s3_class(settings, "dataRobotDeploymentSettings")
  })

  test_that("succeeds for a valid deployment id", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(deploymentResponse)
    settings <- with_mock(
      `httr::GET` = getStub$f,
      `datarobot:::Endpoint` = function() fakeEndpoint,
      `datarobot:::Token` = function() fakeToken,
      GetDeploymentSettings(fakeDeployment$id)
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_s3_class(settings, "dataRobotDeploymentSettings")
  })

  test_that("will 404 for an invalid deployment", {
    errorMsg <- jsonlite::toJSON(list(message = list("404 Not Found")))
    notFoundResponse <- httr:::response(
      url = getSettingsUrl,
      status_code = 404L,
      content = charToRaw(errorMsg)
    )

    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(notFoundResponse)

    expect_error(
      with_mock(
        `httr::GET` = getStub$f,
        `datarobot:::Endpoint` = function() fakeEndpoint,
        `datarobot:::Token` = function() fakeToken,
        GetDeploymentSettings(fakeDeploymentId)
      ),
      "404"
    )
  })
})

describe("UpdateDeploymentSettings()", {
  # test_that("succeeds for a valid deployment")
  # test_that("succeeds for a valid deployment id")
  test_that("returns a deployment object", {
    newSettings <- list()
    newSettings$associationId$requiredInPredictionRequests <- TRUE
    return <- with_mock(
      `datarobot:::PatchSettingsAndWait` = function(routeString,
                                                    payload = NULL,
                                                    ...) {
        payloadToInspect <<- payload
        fakeDeployment
      },
      UpdateDeploymentSettings(fakeDeployment, newSettings, maxWait = 10)
    )
    expect_equal(payloadToInspect, newSettings)
    # "The update deployment settings endpoint should return a deployment"
    expect_s3_class(return, "dataRobotDeployment")
    expect_equal(return, fakeDeployment)
  })
})

describe("Deployment drift tracking settings", {
  test_that("it can get deployment drift tracking settings", {
    getStub <- stub(httr::GET)
    getDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId, "settings")
    getDeploymentJson <- fileToChar("responses/getDeploymentSettings.json")
    deploymentResponse <- httr:::response(
      url = getDeploymentUrl,
      status_code = 200L,
      content = charToRaw(getDeploymentJson)
    )
    getStub$onCall(1)$returns(deploymentResponse)
    settings <- with_mock(
      `httr::GET` = getStub$f,
      `datarobot:::Endpoint` = function() fakeEndpoint,
      `datarobot:::Token` = function() fakeToken,
      GetDeploymentDriftTrackingSettings(fakeDeploymentId)
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_type(settings, "list")
    ExpectHasKeys(settings, c(
      "predictionIntervals", "targetDrift",
      "associationId", "featureDrift"
    ))
    expect_type(settings$predictionIntervals, "list")
    ExpectHasKeys(settings$predictionIntervals, c("percentiles", "enabled"))
    expect_type(settings$featureDrift, "list")
    ExpectHasKeys(settings$featureDrift, "enabled")
    expect_type(settings$associationId, "list")
    ExpectHasKeys(settings$associationId, c("columnNames", "requiredInPredictionRequests"))
  })

  test_that("it can update deployment drift tracking settings", {
    patchStub <- stub(httr::PATCH)
    replaceDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId, "settings")
    replaceDeploymentJson <- fileToChar("responses/replaceDeploymentDriftSettings.json")
    replaceDeploymentResponse <- httr:::response(
      url = replaceDeploymentUrl,
      status_code = 202L,
      content = charToRaw(replaceDeploymentJson)
    )
    patchStub$onCall(1)$returns(replaceDeploymentResponse)
    getStub <- stub(httr::GET)
    getDeploymentSettingsUrl <- UrlJoin("deployments", fakeDeploymentId, "settings")
    getDeploymentSettingsJson <- fileToChar("responses/getDeploymentSettings.json")
    deploymentSettingResponse <- httr:::response(
      url = getDeploymentSettingsUrl,
      status_code = 200L,
      content = charToRaw(getDeploymentSettingsJson)
    )
    getStub$onCall(1)$returns(deploymentSettingResponse)
    settings <- with_mock(
      `httr::PATCH` = patchStub$f,
      `httr::GET` = getStub$f,
      `httr::POST` = function(...) stop("Should not be called!"),
      `datarobot:::DataRobotPATCH` = function(routeString,
                                              addUrl = TRUE,
                                              body = NULL,
                                              returnRawResponse = FALSE, ...) {
        bodyForInspect <<- body
        datarobot:::MakeDataRobotRequest(httr::PATCH, routeString,
          addUrl = addUrl,
          returnRawResponse = returnRawResponse,
          body = body, ...
        )
      },
      `datarobot::WaitForAsyncReturn` = function(...) {
        fakeDeployment
      },
      `datarobot:::Endpoint` = function() fakeEndpoint,
      `datarobot:::Token` = function() fakeToken,
      UpdateDeploymentDriftTrackingSettings(fakeDeployment,
        targetDriftEnabled = TRUE,
        featureDriftEnabled = FALSE
      )
    )
    expect_equal(patchStub$calledTimes(), 1)
    expect_false(bodyForInspect$featureDrift$enabled)
    expect_true(bodyForInspect$targetDrift$enabled)
    expect_type(settings, "list")
    ExpectHasKeys(settings, c(
      "predictionIntervals", "targetDrift",
      "associationId", "featureDrift"
    ))
    expect_type(settings$predictionIntervals, "list")
    ExpectHasKeys(settings$predictionIntervals, c("percentiles", "enabled"))
    expect_type(settings$featureDrift, "list")
    ExpectHasKeys(settings$featureDrift, "enabled")
    expect_type(settings$associationId, "list")
    ExpectHasKeys(settings$associationId, c("columnNames", "requiredInPredictionRequests"))
  })
})

describe("GetDeploymentAssociationId()", {
  settingsResponse <- jsonlite::fromJSON(fileToChar("responses/getDeploymentSettings.json"))

  test_that("return value is the right shape", {
    actual <- with_mock(
      "datarobot::GetDeploymentSettings" = function(...) settingsResponse,
      GetDeploymentAssociationId(fakeDeployment)
    )

    expected <- settingsResponse$associationId
    expect_s3_class(actual, "dataRobotDeploymentAssociationIdSettings")
    expect_equivalent(actual,
      expected,
      info = "Return value for GetDeploymentAssociationId() is effectively the same
                      as GetDeploymentSettings()$associationId"
    )
  })
})

describe("UpdateDeploymentAssociationId()", {
  test_that("fails if you don't pass a valid deployment", {
    expect_error(UpdateDeploymentAssociationId(NULL))
  })

  test_that("is a no-op if you don't make any changes", {
    expect_error(
      with_mock(
        `datarobot:::MakeDataRobotRequest` = function(...) {
          stop("No HTTP requests should have been made!")
        },
        UpdateDeploymentAssociationId(fakeDeployment)
      )
    )
  })

  #' A helper method that extracts the payload from the
  #' UpdateDeploymentSettings() invocation so that we can ensure we are calling
  #' it with the expected arguments
  extractUpdateSettingsPayload <- function(f) {
    settingsResponse <- jsonlite::fromJSON(fileToChar("responses/getDeploymentSettings.json"))

    newSettingsToInspect <- list()
    # We ignore the actual return value of arg f; we only care that we are
    # retrieving the arguments passed to UpdateDeploymentSettings
    with_mock(
      "datarobot::UpdateDeploymentSettings" = function(deployment,
                                                       newSettings = NULL, ...) {
        newSettingsToInspect <<- newSettings
      },
      "datarobot::GetDeploymentSettings" = function(...) settingsResponse,
      f
    )
    newSettingsToInspect
  }

  test_that("UpdateSettings request is set with all values", {
    apiPayload <- extractUpdateSettingsPayload(
      UpdateDeploymentAssociationId(fakeDeployment,
        columnNames = c("score", "score2"),
        requiredInPredictionRequests = TRUE
      )
    )
    expect_equal(
      apiPayload$associationId$columnNames,
      list("score", "score2")
    )
    expect_true(apiPayload$associationId$requiredInPredictionRequests)
  })

  test_that("UpdateSettings request doesn't include columnNames if not given", {
    apiPayload <- extractUpdateSettingsPayload(
      UpdateDeploymentAssociationId(fakeDeployment,
        requiredInPredictionRequests = FALSE
      )
    )
    expect_null(apiPayload$associationId$columnNames,
      info = "No columnNames in the request if the arg wasn't set"
    )
    expect_false(apiPayload$associationId$requiredInPredictionRequests)

    apiPayload2 <- extractUpdateSettingsPayload(
      UpdateDeploymentAssociationId(fakeDeployment,
        columnNames = list(),
        requiredInPredictionRequests = FALSE
      )
    )
    expect_null(apiPayload2$associationId$columnNames,
      info = "No columnNames in the request if the arg is an empty list"
    )
    expect_false(apiPayload2$associationId$requiredInPredictionRequests)
  })

  test_that("UpdateSettings doesn't include required if not given", {
    apiPayload <- extractUpdateSettingsPayload(
      UpdateDeploymentAssociationId(fakeDeployment,
        columnNames = c("score")
      )
    )
    expect_equal(apiPayload$associationId$columnNames, list("score"))
    expect_null(apiPayload$associationId$requiredInPredictionRequests)
  })
})
