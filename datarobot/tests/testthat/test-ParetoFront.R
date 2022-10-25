# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(testthat)

fakeEureqaSolutionId <- "10"

describe("it can get a pareto front", {
  getStub <- stub(httr::GET)
  getParetoFrontUrl <- UrlJoin(projectUrl, "eureqaModels", fakeModelId)
  getParetoFrontJson <- fileToChar("responses/getParetoFront.json")
  paretoFrontResponse <- httr:::response(
    url = getParetoFrontUrl,
    status_code = 200L,
    content = charToRaw(getParetoFrontJson)
  )
  getStub$onCall(1)$returns(paretoFrontResponse)
  paretoFront <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetParetoFront(fakeModel)
  )
  expect_equal(getStub$calledTimes(), 1)

  test_that("the pareto front is a list", {
    expect_type(paretoFront, "list")
  })

  test_that("paretoFront", {
    expect_equal(paretoFront$projectId, "5b2827556523cd05bd1507a5")
    expect_equal(paretoFront$errorMetric, "Log Loss Error")
    expect_type(paretoFront$hyperparameters, "list")
    expect_equal(paretoFront$targetType, "Binary")
  })

  test_that("paretoPoint", {
    expect_equal(length(paretoFront$solutions), 5)

    expect_equal(paretoFront$solutions[[1]]$eureqaSolutionId, "5b2940f202d5abf17ddca9f8")
    expect_equal(paretoFront$solutions[[1]]$complexity, 1)
    expect_equal(paretoFront$solutions[[1]]$error, 0.6568885703145153)
    expect_equal(paretoFront$solutions[[1]]$expression, "target = 0")
    expect_equal(paretoFront$solutions[[1]]$expressionAnnotated, "target = 0")
    expect_false(paretoFront$solutions[[1]]$bestModel)

    expect_equal(paretoFront$solutions[[2]]$eureqaSolutionId, "5b2940f202d5abf17ddca9f9")
    expect_equal(paretoFront$solutions[[2]]$complexity, 2)
    expect_equal(paretoFront$solutions[[2]]$error, 0.6463236186153043)
    expect_equal(paretoFront$solutions[[2]]$expression, "target = 0")
    expect_equal(paretoFront$solutions[[2]]$expressionAnnotated, "target = 0")
    expect_false(paretoFront$solutions[[2]]$bestModel)

    expect_equal(paretoFront$solutions[[3]]$eureqaSolutionId, "5b2940f202d5abf17ddca9fa")
    expect_equal(paretoFront$solutions[[3]]$complexity, 3)
    expect_equal(paretoFront$solutions[[3]]$error, 0.6369812561414678)
    expect_equal(paretoFront$solutions[[3]]$expression, "target = 0")
    expect_equal(paretoFront$solutions[[3]]$expressionAnnotated, "target = 0")
    expect_false(paretoFront$solutions[[3]]$bestModel)

    expect_equal(paretoFront$solutions[[4]]$eureqaSolutionId, "5b2940f202d5abf17ddca9fb")
    expect_equal(paretoFront$solutions[[4]]$complexity, 4)
    expect_equal(paretoFront$solutions[[4]]$error, 0.6341910672553971)
    expect_equal(paretoFront$solutions[[4]]$expression, "target = 0")
    expect_equal(paretoFront$solutions[[4]]$expressionAnnotated, "target = 0")
    expect_false(paretoFront$solutions[[4]]$bestModel)

    expect_equal(paretoFront$solutions[[5]]$eureqaSolutionId, "5b29406c6523cd0665685a8d")
    expect_equal(paretoFront$solutions[[5]]$complexity, 5)
    expect_equal(paretoFront$solutions[[5]]$error, 0.6327990044770845)
    expect_equal(paretoFront$solutions[[5]]$expression, "target = 0")
    expect_equal(paretoFront$solutions[[5]]$expressionAnnotated, "target = 0")
    expect_true(paretoFront$solutions[[5]]$bestModel)
  })
})


test_that("a pareto front on a non-eureqa model is an error", {
  getStub <- stub(httr::GET)
  getParetoFrontUrl <- UrlJoin(projectUrl, "eureqaModels", fakeModelId)
  msg <- "Model ObjectId('5cc4f43123d2a1539262eec5') does not have Eureqa metadata."
  msg <- jsonlite::toJSON(list(message = list(msg)))
  paretoFrontResponse <- httr:::response(
    url = getParetoFrontUrl,
    status_code = 404L,
    content = charToRaw(msg)
  )
  getStub$onCall(1)$returns(paretoFrontResponse)
  expect_error(
    with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetParetoFront(fakeModel)
    ),
    "(404)"
  )
})


projectUrl <- UrlJoin(fakeEndpoint, "projects", fakeProjectId)
modelUrl <- UrlJoin(projectUrl, "eureqaModels")
requestResponse <- httr:::response(
  url = modelUrl,
  status_code = 202L,
  headers = list(location = modelUrl),
  content = raw(0)
)
test_that("AddEureqaSolution adds a eureqa solution", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    expect_message(
      AddEureqaSolution(fakeProjectId, fakeEureqaSolutionId),
      "Solution added"
    )
  )
  expect_equal(postStub$calledTimes(), 1)
})
