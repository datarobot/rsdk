# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(testthat)

requestPredictionsUrl <- UrlJoin("projects", fakeProjectId, "predictions")
requestPredictionsJobUrl <- UrlJoin(requestPredictionsUrl, fakeJobId)
predictionPostResponse <- httr:::response(
  url = requestPredictionsUrl,
  status_code = 202L,
  headers = list(location = requestPredictionsJobUrl),
  content = raw(0)
)

describe("Regression", {
  getPredictionsJson <- fileToChar("responses/getRegressionPredictions.json")
  predictionsResponse <- httr:::response(
    url = getPredictionsUrl,
    status_code = 202L,
    content = charToRaw(getPredictionsJson)
  )

  test_that("Regression predictions work for type = response", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "response")
    )
    expect_type(predictions, "double")
    expect_equal(length(predictions), 698)
  })

  test_that("Regression predictions work for type = probability", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "probability")
    )
    expect_type(predictions, "double")
    expect_equal(length(predictions), 698)
  })

  test_that("Regression predictions work for type = raw", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "raw")
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c("prediction", "rowId"))
  })
})


describe("Binary predictions", {
  test_that("Binary predictions work for type = response", {
    getPredictionsJson <- fileToChar("responses/getBinaryResponsePredictions.json")
    predictionsResponse <- httr:::response(
      url = getPredictionsUrl,
      status_code = 202L,
      content = charToRaw(getPredictionsJson)
    )
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "response")
    )
    expect_type(predictions, "character")
    expect_equal(sort(unique(predictions)), sort(c("No", "Yes")))
    expect_equal(length(predictions), 698)
  })

  getPredictionsJson <- fileToChar("responses/getBinaryProbPredictions.json")
  predictionsResponse <- httr:::response(
    url = getPredictionsUrl,
    status_code = 202L,
    content = charToRaw(getPredictionsJson)
  )

  test_that("Binary predictions work for type = probability", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "probability")
    )
    expect_type(predictions, "double")
    expect_equal(length(predictions), 698)
    expect_true(all(0 <= predictions & predictions <= 1))
  })

  test_that("Binary predictions work for type = raw", {
    getStub <- stub(httr::GET)
    getPredictionsJson <- fileToChar("responses/getBinaryProbPredictions.json")
    predictionsResponse <- httr:::response(
      url = getPredictionsUrl,
      status_code = 202L,
      content = charToRaw(getPredictionsJson)
    )
    getStub$onCall(1)$returns(predictionsResponse)
    postStub <- stub(httr::POST)
    requestPredictionsUrl <- UrlJoin("projects", fakeProjectId, "predictions")
    requestPredictionsJobUrl <- UrlJoin("projects", fakeProjectId, "predictions", fakeJobId)
    predictionPostResponse <- httr:::response(
      url = requestPredictionsUrl,
      status_code = 202L,
      headers = list(location = requestPredictionsJobUrl),
      content = raw(0)
    )
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "raw")
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c("positiveProbability", "prediction", "rowId", "predictionValues"))
  })
})


describe("Multiclass", {
  getPredictionsJson <- fileToChar("responses/getMulticlassResponsePredictions.json")
  predictionsResponse <- httr:::response(
    url = getPredictionsUrl,
    status_code = 202L,
    content = charToRaw(getPredictionsJson)
  )

  test_that("Multiclass predictions work for type = response", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "response")
    )
    expect_type(predictions, "character")
    expect_equal(sort(unique(predictions)), sort(unique(as.character(iris$Species))))
    expect_equal(length(predictions), nrow(iris))
  })

  getPredictionsJson <- fileToChar("responses/getMulticlassProbPredictions.json")
  predictionsResponse <- httr:::response(
    url = getPredictionsUrl,
    status_code = 202L,
    content = charToRaw(getPredictionsJson)
  )

  test_that("Multiclass predictions work for type = probability", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "probability")
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c("class_setosa", "class_versicolor", "class_virginica"))
    expect_true(all(0 <= predictions$class_setosa & predictions$class_setosa <= 1))
    expect_true(all(0 <= predictions$class_versicolor & predictions$class_versicolor <= 1))
    expect_true(all(0 <= predictions$class_virginica & predictions$class_virginica <= 1))
  })

  test_that("Multiclass predictions work for type = raw", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "raw")
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c("predictionValues", "prediction", "rowId"))
  })

  test_that("Multiclass column names change with class_prefix", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel,
        fakeDataset,
        type = "probability",
        classPrefix = "foo"
      )
    )
    ExpectHasKeys(predictions, c("foosetosa", "fooversicolor", "foovirginica"))
  })
})


describe("Time series", {
  getPredictionsJson <- fileToChar("responses/getTimeSeriesPredictions.json")
  predictionsResponse <- httr:::response(
    url = getPredictionsUrl,
    status_code = 202L,
    content = charToRaw(getPredictionsJson)
  )

  test_that("Time series predictions work for type = response", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "response")
    )
    expect_type(predictions, "double")
  })

  test_that("Time series predictions work for type = probability", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "probability")
    )
    expect_type(predictions, "double")
  })

  test_that("Time series predictions work for type = raw", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "raw")
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c(
      "forecastDistance", "forecastPoint",
      "prediction", "rowId", "timestamp"
    ))
  })

  getPredictionsJson <- fileToChar("responses/GetPredictionsWithIntervals.json")
  predictionsResponse <- httr:::response(
    url = getPredictionsUrl,
    status_code = 202L,
    content = charToRaw(getPredictionsJson)
  )

  test_that("Time series predictions work for type = raw and prediction intervals", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel,
        fakeDataset,
        includePredictionIntervals = TRUE,
        type = "raw"
      )
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c(
      "forecastDistance", "forecastPoint",
      "prediction", "rowId", "timestamp",
      "predictionIntervalLowerBound",
      "predictionIntervalUpperBound"
    ))
  })

  test_that("Time series predictions work for type = raw and prediction intervals with custom", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::DataRobotPOST" = function(routeString,
                                             addUrl = TRUE,
                                             body = NULL,
                                             returnRawResponse = FALSE, ...) {
        if (grepl("predictions", routeString)) { # Record params for
          bodyForInspect <<- body # request preds call
        }
        datarobot:::MakeDataRobotRequest(httr::POST, routeString,
          addUrl = addUrl,
          returnRawResponse = returnRawResponse,
          body = body, ...
        )
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel,
        fakeDataset,
        includePredictionIntervals = TRUE,
        predictionIntervalsSize = 100,
        type = "raw"
      )
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c(
      "forecastDistance", "forecastPoint",
      "prediction", "rowId", "timestamp",
      "predictionIntervalLowerBound",
      "predictionIntervalUpperBound"
    ))
    expect_equal(bodyForInspect$modelId, fakeModelId)
    expect_equal(bodyForInspect$datasetId, fakeDatasetId)
    expect_equal(bodyForInspect$predictionIntervalsSize, 100)
  })

  test_that("Time series predictions work for type = raw and custom interval", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::DataRobotPOST" = function(routeString,
                                             addUrl = TRUE,
                                             body = NULL,
                                             returnRawResponse = FALSE, ...) {
        if (grepl("predictions", routeString)) { # Record params for
          bodyForInspect <<- body # request preds call
        }
        datarobot:::MakeDataRobotRequest(httr::POST,
          routeString,
          addUrl = addUrl,
          returnRawResponse = returnRawResponse,
          body = body, ...
        )
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel,
        fakeDataset,
        predictionIntervalsSize = 100,
        type = "raw"
      )
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c(
      "forecastDistance", "forecastPoint",
      "prediction", "rowId", "timestamp",
      "predictionIntervalLowerBound",
      "predictionIntervalUpperBound"
    ))
    expect_equal(bodyForInspect$modelId, fakeModelId)
    expect_equal(bodyForInspect$datasetId, fakeDatasetId)
  })
})


describe("Binary time series", {
  getPredictionsJson <- fileToChar("responses/getBinaryTimePredictions.json")
  predictionsResponse <- httr:::response(
    url = getPredictionsUrl,
    status_code = 202L,
    content = charToRaw(getPredictionsJson)
  )

  test_that("Binary time series predictions work for type = response", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "response")
    )
    expect_type(predictions, "integer")
    expect_equal(sort(unique(predictions)), c(0, 1))
  })

  test_that("Binary time series predictions work for type = probability", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "probability")
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c(
      "forecastDistance", "forecastPoint",
      "positiveProbability", "prediction", "rowId",
      "timestamp", "predictionValues", "predictionThreshold"
    ))
  })

  test_that("Binary time series predictions work for type = raw", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "raw")
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c(
      "forecastDistance", "forecastPoint",
      "positiveProbability", "prediction", "rowId",
      "timestamp", "predictionValues", "predictionThreshold"
    ))
  })
})


describe("Multiseries", {
  getPredictionsJson <- fileToChar("responses/getMultiseriesPredictions.json")
  predictionsResponse <- httr:::response(
    url = getPredictionsUrl,
    status_code = 202L,
    content = charToRaw(getPredictionsJson)
  )

  test_that("Multiseries predictions work for type = response", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "response")
    )
    expect_type(predictions, "double")
  })

  test_that("Multiseries predictions work for type = probability", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "probability")
    )
    expect_type(predictions, "double")
  })

  test_that("Multiseries predictions work for type = raw", {
    postStub <- stub(httr::POST)
    postStub$onCall(1)$returns(predictionPostResponse)
    predictions <- with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      Predict(fakeModel, fakeDataset, type = "raw")
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c(
      "seriesId", "forecastDistance", "forecastPoint",
      "prediction", "rowId", "timestamp"
    ))
  })
})


test_that("GetPredictions raises appropriate exception when job status indicates failure", {
  jobFailureJson <- '{"status": "error", "message": "some job failure message"}'
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(predictionPostResponse)
  testReturn <- with_mock(
    "datarobot::DataRobotPOST" = postStub$f,
    "datarobot::DataRobotGET" = function(url, ...) {
      failureResponse <- httr:::response(
        url = url,
        status_code = 200,
        content = charToRaw(jobFailureJson)
      )
      failureResponse
    },
    expect_error(Predict(fakeModel, fakeDataset), "PendingJobFailed")
  )
})

getPredictionsJson <- fileToChar("responses/getRegressionPredictions.json")
predictionsResponse <- httr:::response(
  url = getPredictionsUrl,
  status_code = 202L,
  content = charToRaw(getPredictionsJson)
)

test_that("maxWait parameter is passed to WaitForAsyncReturn", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(predictionPostResponse)
  maxWaitToUse <- 2
  maxWaitPassed <- with_mock(
    "datarobot::DataRobotPOST" = postStub$f,
    "datarobot::WaitForAsyncReturn" = function(...) {
      expect_equal(list(...)$maxWait, maxWaitToUse)
      ParseReturnResponse(predictionsResponse)
    },
    Predict(fakeModel, fakeDataset, maxWait = maxWaitToUse)
  )
})

test_that("It can upload a dataframe and then predict", {
  postStub <- stub(httr::POST)
  datasetsEndpoint <- datarobot:::UrlJoin(
    fakeEndpoint, "projects",
    fakeProjectId, "predictionDatasets"
  )
  statusUrl <- datarobot:::UrlJoin(fakeEndpoint, "status", "some-status")
  uploadResponse <- httr:::response(
    url = datasetsEndpoint,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  )
  postStub$onCall(1)$returns(uploadResponse)
  postStub$onCall(2)$returns(predictionPostResponse)
  predictions <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot::WaitForAsyncReturn" = function(...) {
      ParseReturnResponse(predictionsResponse)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    Predict(fakeModel, iris, type = "response")
  )
  expect_equal(postStub$calledTimes(), 2)
  expect_type(predictions, "double")
  expect_equal(length(predictions), 698)
})

test_that("predict", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(predictionPostResponse)
  predictions <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot::WaitForAsyncReturn" = function(...) {
      ParseReturnResponse(predictionsResponse)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    predict(fakeModel, fakeDataset, type = "response")
  )
  expect_type(predictions, "double")
  expect_equal(length(predictions), 698)
})
