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


describe("Errors", {
  test_that("Required parameters are present", {
    expect_error(GetPredictions())
    expect_error(GetPredictions(fakeProjectId))
    expect_error(GetModelFromJobId(fakeJobId))
  })

  test_that("Invalid type fails", {
    expect_error(GetPredictions(fakeProjectId, fakeJobId, type = "invalid"))
  })
})


describe("Regression predictions", {
  getPredictionsJson <- fileToChar("responses/getRegressionPredictions.json")
  predictionsResponse <- httr:::response(
    url = getPredictionsUrl,
    status_code = 303L,
    content = charToRaw(getPredictionsJson)
  )

  test_that("Regression predictions work for type = response", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "response")
    )
    expect_type(predictions, "double")
    expect_equal(length(predictions), 698)
  })

  test_that("Regression predictions work for type = probability", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "probability")
    )
    expect_type(predictions, "double")
    expect_equal(length(predictions), 698)
  })

  test_that("Regression predictions work for type = raw", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "raw")
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c("prediction", "rowId"))
  })
})


describe("Binary predictions", {
  test_that("Binary predictions work for type = response", {
    getStub <- stub(httr::GET)
    getPredictionsJson <- fileToChar("responses/getBinaryResponsePredictions.json")
    predictionsResponse <- httr:::response(
      url = getPredictionsUrl,
      status_code = 303L,
      content = charToRaw(getPredictionsJson)
    )
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "response")
    )
    expect_type(predictions, "character")
    expect_equal(sort(unique(predictions)), sort(c("No", "Yes")))
    expect_equal(length(predictions), 698)
  })

  getPredictionsJson <- fileToChar("responses/getBinaryProbPredictions.json")
  predictionsResponse <- httr:::response(
    url = getPredictionsUrl,
    status_code = 303L,
    content = charToRaw(getPredictionsJson)
  )

  test_that("Binary predictions work for type = probability", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "probability")
    )
    expect_type(predictions, "double")
    expect_equal(length(predictions), 698)
    expect_true(all(0 <= predictions & predictions <= 1))
  })

  test_that("Binary predictions work for type = raw", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "raw")
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c("positiveProbability", "prediction", "rowId", "predictionValues"))
  })
})


describe("Multiclass", {
  getPredictionsJson <- fileToChar("responses/getMulticlassResponsePredictions.json")
  predictionsResponse <- httr:::response(
    url = getPredictionsUrl,
    status_code = 303L,
    content = charToRaw(getPredictionsJson)
  )

  test_that("Multiclass predictions work for type = response", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "response")
    )
    expect_type(predictions, "character")
    expect_equal(sort(unique(predictions)), sort(unique(as.character(iris$Species))))
    expect_equal(length(predictions), nrow(iris))
  })

  test_that("Multiclass predictions work for type = probability", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "probability")
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c("class_setosa", "class_versicolor", "class_virginica"))
    expect_true(all(0 <= predictions$class_setosa & predictions$class_setosa <= 1))
    expect_true(all(0 <= predictions$class_versicolor & predictions$class_versicolor <= 1))
    expect_true(all(0 <= predictions$class_virginica & predictions$class_virginica <= 1))
  })

  test_that("Multiclass predictions work for type = raw", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "raw")
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c("predictionValues", "prediction", "rowId"))
  })

  test_that("Multiclass column names change with class_prefix", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject,
        fakeJobId,
        type = "probability",
        classPrefix = "foo"
      )
    )
    ExpectHasKeys(predictions, c("foosetosa", "fooversicolor", "foovirginica"))
  })

  test_that("Multiclass - class_prefix can be blank", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject,
        fakeJobId,
        type = "probability",
        classPrefix = ""
      )
    )
    ExpectHasKeys(predictions, c("setosa", "versicolor", "virginica"))
  })
})


describe("Time series", {
  test_that("Time series predictions work for type = response", {
    getStub <- stub(httr::GET)
    getPredictionsJson <- fileToChar("responses/getTimeSeriesPredictions.json")
    predictionsResponse <- httr:::response(
      url = getPredictionsUrl,
      status_code = 303L,
      content = charToRaw(getPredictionsJson)
    )
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "response")
    )
    expect_type(predictions, "double")
  })

  test_that("Time series predictions work for type = probability", {
    getStub <- stub(httr::GET)
    getPredictionsJson <- fileToChar("responses/getBinaryProbPredictions.json")
    predictionsResponse <- httr:::response(
      url = getPredictionsUrl,
      status_code = 303L,
      content = charToRaw(getPredictionsJson)
    )
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "probability")
    )
    expect_type(predictions, "double")
  })

  test_that("Time series predictions work for type = raw", {
    getStub <- stub(httr::GET)
    getPredictionsJson <- fileToChar("responses/getBinaryTimePredictions.json")
    predictionsResponse <- httr:::response(
      url = getPredictionsUrl,
      status_code = 303L,
      content = charToRaw(getPredictionsJson)
    )
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "raw")
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c(
      "forecastDistance", "forecastPoint",
      "positiveProbability", "prediction", "rowId", "timestamp",
      "predictionValues", "predictionThreshold"
    ))
  })

  test_that("Time series predictions work for type = raw with prediction intervals", {
    getStub <- stub(httr::GET)
    getPredictionsJson <- fileToChar("responses/GetPredictionsWithIntervals.json")
    predictionsResponse <- httr:::response(
      url = getPredictionsUrl,
      status_code = 303L,
      content = charToRaw(getPredictionsJson)
    )
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "raw")
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c(
      "prediction", "rowId", "timestamp",
      "forecastDistance", "forecastPoint",
      "predictionIntervalLowerBound",
      "predictionIntervalUpperBound"
    ))
  })
})


describe("Binary time series", {
  getPredictionsJson <- fileToChar("responses/getBinaryTimePredictions.json")
  predictionsResponse <- httr:::response(
    url = getPredictionsUrl,
    status_code = 303L,
    content = charToRaw(getPredictionsJson)
  )
  test_that("Binary time series predictions work for type = response", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "response")
    )
    expect_type(predictions, "integer")
    expect_equal(sort(unique(predictions)), c(0, 1))
  })

  test_that("Binary time series predictions work for type = probability", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "probability")
    )
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c(
      "forecastDistance", "forecastPoint",
      "positiveProbability", "prediction", "rowId",
      "timestamp", "predictionValues", "predictionThreshold"
    ))
  })

  test_that("Binary time series predictions work for type = raw", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "raw")
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
    status_code = 303L,
    content = charToRaw(getPredictionsJson)
  )
  test_that("Multiseries predictions work for type = response", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "response")
    )
    expect_type(predictions, "double")
  })

  test_that("Multiseries predictions work for type = probability", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "probability")
    )
    expect_type(predictions, "double")
  })

  test_that("Multiseries predictions work for type = raw", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(predictionsResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetPredictions(fakeProject, fakeJobId, type = "raw")
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
  testReturn <- with_mock(
    "datarobot::DataRobotGET" = function(url, ...) {
      httr:::response(
        url = url,
        status_code = 200,
        content = charToRaw(jobFailureJson)
      )
    },
    expect_error(GetPredictions(fakeProjectId, fakeJobId), "PendingJobFailed")
  )
})

test_that("GetPredictions raises appropriate exception for AsyncTimeout", {
  jobFailureJson <- '{"status": "error", "message": "some job failure message"}'
  testReturn <- with_mock(
    "datarobot:::WaitForAsyncReturn" = function(...) {
      Raise(Exceptions$AsyncTimeout(message = "Async service timed out"))
    },
    {
      error <- tryCatch(GetPredictions(fakeProjectId, fakeJobId),
        error = function(e) e
      )
      expect_true(grepl("predictions did not complete", as.character(error$message)))
    }
  )
})

test_that("maxWait parameter is passed to WaitForAsyncReturn", {
  getStub <- stub(httr::GET)
  getPredictionsJson <- fileToChar("responses/getRegressionPredictions.json")
  predictionsResponse <- httr:::response(
    url = getPredictionsUrl,
    status_code = 303L,
    content = charToRaw(getPredictionsJson)
  )
  maxWaitToUse <- 2
  maxWaitPassed <- with_mock(
    "datarobot::WaitForAsyncReturn" = function(...) {
      expect_equal(list(...)$maxWait, maxWaitToUse)
      ParseReturnResponse(predictionsResponse)
    },
    GetPredictions(fakeProjectId, fakeJobId, maxWait = maxWaitToUse)
  )
})


test_that("GetPredictions works with predictionId", {
  getStub <- stub(httr::GET)
  getPredictionsJson <- fileToChar("responses/getRegressionPredictions.json")
  predictionsResponse <- httr:::response(
    url = getPredictionsUrl,
    status_code = 200L,
    content = charToRaw(getPredictionsJson)
  )
  getStub$onCall(1)$returns(predictionsResponse)
  predictions <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetPredictions(fakeProject, fakePredictionId, type = "response")
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(predictions, "double")
  expect_equal(length(predictions), 698)
})


describe("ListPredictions", {
  listPredictionsUrl <- UrlJoin("projects", fakeProjectId, "predictions")
  listPredictionsJson <- fileToChar("responses/ListPredictions.json")
  predictionsResponse <- httr:::response(
    url = listPredictionsUrl,
    status_code = 200L,
    content = charToRaw(listPredictionsJson)
  )
  test_that("ListPredictions works", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      ListPredictions(fakeProject)
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c(
      "projectId", "datasetId", "modelId", "predictionId",
      "includesPredictionIntervals", "predictionIntervalsSize"
    ))
  })

  test_that("ListPredictions can filter by modelId", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      ListPredictions(fakeProject, modelId = fakeModelId)
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c(
      "projectId", "datasetId", "modelId", "predictionId",
      "includesPredictionIntervals", "predictionIntervalsSize"
    ))
  })

  test_that("ListPredictions can filter by datasetId", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      ListPredictions(fakeProject, datasetId = fakeDatasetId)
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c(
      "projectId", "datasetId", "modelId", "predictionId",
      "includesPredictionIntervals", "predictionIntervalsSize"
    ))
  })

  test_that("ListPredictions can filter by datasetId and modelId", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(predictionsResponse)
    predictions <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      ListPredictions(fakeProject,
        modelId = fakeModelId,
        datasetId = fakeDatasetId
      )
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_s3_class(predictions, "data.frame")
    ExpectHasKeys(predictions, c(
      "projectId", "datasetId", "modelId", "predictionId",
      "includesPredictionIntervals", "predictionIntervalsSize"
    ))
  })
})
